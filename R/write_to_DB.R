
#' Add a new person entry to the DB if not already present
#'
#' This function takes a person's details and checks if they already exist in the
#' given person table. First, it checks for an exact match of email or orcid.
#' Then it looks for potential matches based on fuzzy name matching. If (potential)
#' matches are found, the user is prompted to decide whether (it is indeed a match
#' and) - if some of the details have changed - they should be updated in the DB.
#' If there is no match found/confirmed in DB, the new person is inserted.
#' In all cases, the function returns the person's details including the retrieved
#' or newly generated person_id.
add_person <- function(new_person, person_table, dbcon, schema, fuzzy_thres = 0.25){
  # TODO: if interactive() ...

  # check for exact match on orcid or email
  matched_exact <- subset(
    person_table,
    (!is.na(orcid) & orcid == new_person$orcid) |
      (tolower(email) == tolower(new_person$email))
  )
  # if there is an exact match:
  if (nrow(matched_exact) > 0) {
    res_combined <- matched_exact %>% dplyr::bind_rows(new_person) %>% tibble::add_column(source = c('DB','new'), .before = 1)
    cat("Person already exists in DB (exact match on ORCID or Email):\n")
    print(res_combined)
    # if new information is different from DB entry:
    if (nrow(dplyr::distinct(res_combined, dplyr::pick(last_name:orcid)))>1) {
      sel_opt <- tolower(readline(prompt = "Should the DB entry be updated with the new information? [y/n]: "))
      switch(sel_opt,
        "y" = {
          DBI::dbGetQuery(
            dbcon,
            sprintf("UPDATE %s.person SET (last_name, first_name, email, orcid) = ($1, $2, $3, nullif($4, 'NA')) WHERE person_id = $5;", schema),
            params = c(as.list(new_person), matched_exact$person_id)
          )
          cat("Person entry successfully updated in DB.")
          person_entry <- cbind(person_id = matched_exact$person_id, new_person)
        },
        "n" = {
          cat("Person entry up to date, no changes made to DB.")
          person_entry <- matched_exact
        },
        stop("Response not recognized. No changes made, run function again.")
      )
    } else {
      cat("Person entry up to date, no changes made to DB.")
      person_entry <- matched_exact
    }
    return(person_entry)
  }

  # else: check for fuzzy match on names
  name_dist <- stringdist::stringdist(
    tolower(paste(person_table$first_name, person_table$last_name)),
    tolower(paste(new_person$first_name, new_person$last_name)),
    method = "jw")

  matched_fuzzy <- subset(person_table, name_dist < fuzzy_thres)

  if (nrow(matched_fuzzy) > 0) {
    res_combined <- matched_fuzzy %>% dplyr::bind_rows(new_person) %>% tibble::add_column(source = c('DB','new'), .before = 1)
    cat("Potential existing entries found in DB (fuzzy name match):\n")
    print(res_combined)
    # already know that there is no exact match on email (or orcid), so either insert or update needed
    sel_opt <- tolower(readline(prompt = "Is the author already in the DB? [y/n]: "))
    switch(sel_opt,

      "y" = {
        # if there are multiple potential matches, ask user to select one
        if (nrow(matched_fuzzy) > 1) {
          sel_row<- readline(prompt = sprintf("Which ROW contains the match? [1-%d]: ", nrow(matched_fuzzy)))
          sel_row <- as.numeric(sel_row)
          if (is.na(sel_row) || !(sel_row %in% 1:nrow(matched_fuzzy))) {
            stop("Response not recognized. No changes made, run function again.")
          }
        } else {
          sel_row <- 1
        }
        # update DB entry or keep original information?
        sel_update <- tolower(readline(prompt = "Should the DB entry be updated with the new information? [y/n]: "))
        switch(sel_update,
          "y" = {
            DBI::dbGetQuery(
              dbcon,
              sprintf("UPDATE %s.person SET (last_name, first_name, email, orcid) = ($1, $2, $3, nullif($4, 'NA')) WHERE person_id = $5;", schema),
              params = c(as.list(new_person), matched_fuzzy$person_id[sel_row])
            )
            cat("Person entry successfully updated in DB.")
            person_entry <- cbind(person_id = matched_fuzzy$person_id[sel_row], new_person)
          },
          "n" = {
            cat("Person entry up to date, no changes made to DB.")
            person_entry <- matched_fuzzy[sel_row,]
          },
          stop("Response not recognized. No changes made, run function again.")
        )
      },

      # if there is no true match, insert the new person
      "n" = {
        person_entry_id <- DBI::dbGetQuery(
          dbcon,
          sprintf("INSERT INTO %s.person (last_name, first_name, email, orcid) VALUES ($1, $2, $3, nullif($4, 'NA')) RETURNING person_id;", schema),
          params = as.list(new_person)
        )
        cat("New person added to DB.\n")
        person_entry <- cbind(person_entry_id, new_person)
      },

      stop("Response not recognized. No changes made, run function again.")
    )

    return(person_entry)
  }

  # else: insert the new person
  person_entry_id <- DBI::dbGetQuery(
    dbcon,
    sprintf("INSERT INTO %s.person (last_name, first_name, email, orcid) VALUES ($1, $2, $3, nullif($4, 'NA')) RETURNING person_id;", schema),
    params = as.list(new_person)
  )
  cat("New person added to DB.\n")
  person_entry <- cbind(person_entry_id, new_person)
  return(person_entry)
}

write_persons_to_DB <- function(author_data, dbcon, schema, fuzzy_thres = 0.25){
  # TODO: check that required fields exist, that authors are unique,
  # that email and orcids are valid format, that NA is NA (not "" etc) (check orcid with api?)
  person_table <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'person'))
  person_entries <- data.frame(person_id = numeric(0),
                               last_name = character(0),
                               first_name = character(0),
                               email = character(0),
                               orcid = character(0))
  for (k in 1:nrow(author_data)){
    new_person <- author_data[k, c('last_name','first_name','email','orcid')]
    cat(sprintf("\nAuthor %d of %d: %s %s-----------\n", k, nrow(author_data), new_person$last_name, new_person$first_name))
    person_entries[k,] <- add_person(new_person, person_table, dbcon, schema, fuzzy_thres = 0.25)
    rownames(person_entries) <- NULL
  }
  return(cbind(person_entries, author_data[, c('org_name', 'org_rorid', 'aff_dep', 'aff_street', 'aff_plz', 'aff_city', 'org_country', 'contact_person')]))
}


add_new_DS_entry <- function(ds_data, author_data_with_ids, dbcon, schema){
  # TODO: check required fields, formats of name, dates etc

  # reformat ds_data for upload, add locked status
  ds_data$description <- ds_data$ds_desc
  ds_data$embargoed_until <-  switch((ds_data$ds_embargoed == "")+1, ds_data$ds_embargoed, NULL)
  ds_data$acknowledgements <- switch((ds_data$ds_ackn == "")+1, ds_data$ds_ackn, NULL)
  ds_data$locked <- FALSE
  # NOTE: entry_created_at and last_modified_at are set to default now() in DB

  # access rights
  access_rights <- DBI::dbGetQuery(
    dbcon,
    sprintf("SELECT * FROM %s.data_access WHERE access_name = '%s'", schema, ds_data$ds_access)
  )
  if (nrow(access_rights) == 0) {
    stop(sprintf("Access rights '%s' not found in DB. Add first, then retry.", ds_data$ds_access))
  }
  ds_data$access_id <- access_rights$access_id

  # license
  license <- DBI::dbGetQuery(
    dbcon,
    sprintf("SELECT * FROM %s.license WHERE license_name = '%s'", schema, ds_data$ds_license)
  )
  if (nrow(license) == 0) {
    stop(sprintf("License '%s' not found in DB. Add first, then retry.", ds_data$ds_license))
    # TODO: adding licenses if necessary
  }
  ds_data$license_id <- license$license_id

  # contact person
  contact_person <- author_data_with_ids[author_data_with_ids$contact_person,'person_id']
  if (length(contact_person) != 1) {
    stop("There should be exactly one contact person in the author data.")
  }
  ds_data$contact_person_id <- contact_person

  # select only relevant columns
  ds_table <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'dataset'))
  ds_data <- ds_data[names(ds_data) %in% names(ds_table)]

  # check if name exists in db
  if (ds_data$ds_name %in% ds_table$ds_name) {
    cat(sprintf("Dataset with name '%s' already exists in DB: \n", ds_data$ds_name))
    res_combined <- ds_table %>% dplyr::filter(ds_name == ds_data$ds_name) %>%
      dplyr::mutate(embargoed_until = format(embargoed_until)) %>%
      dplyr::bind_rows(ds_data) %>% tibble::add_column(source = c('DB','new'), .before = 1)
    print(res_combined)
    sel_opt <- tolower(readline(prompt = "Is it the same dataset, and DB entry is correct? [y/n]: "))

    switch(sel_opt,
      "y" = {
        cat('No changes made to DB.')
        # return ds_data with matched ids
        result <- res_combined[-1,c('ds_id',names(ds_data))]
        result$ds_id <- res_combined$ds_id[1]
        rownames(result) <- NULL
        return(result)
      },
      "n" = {
        stop("Please check the DB entry and/or change the name for a new dataset.")
      },
      stop("Response not recognized. No changes made, run function again.")
    )

  }

  # else: make query
  col_names <- paste(names(ds_data), collapse = ", ")
  placeholders <- paste0("$", 1:length(ds_data), collapse = ", ")
  #placeholders <- paste0("nullif($", 1:ncol(ds_data), " , 'NA')", collapse = ", ")
  query <- sprintf(
    "INSERT INTO %s.dataset (%s) VALUES (%s) RETURNING ds_id;",
    schema, col_names, placeholders
  )
  # execute insert
  ds_entry_id <- DBI::dbGetQuery(
    dbcon, query, params = ds_data
  )

  cat(sprintf("New dataset '%s' added to DB.\n", ds_data$ds_name))
  return(cbind(ds_entry_id, ds_data))
}

add_authorships <- function(ds_data_with_id, author_data_with_ids, dbcon, schema){
  # NOTE this assumes that author_data_with_ids are in correct order of authorship
  dataset_authors <- data.frame(
    ds_id = ds_data_with_id$ds_id,
    author_nr = 1:nrow(author_data_with_ids),
    person_id = author_data_with_ids$person_id
  )

  author_table <- DBI::dbGetQuery(
    dbcon,
    sprintf("SELECT * FROM %s.author WHERE ds_id = %d", schema, ds_data_with_id$ds_id)
  )

  if (nrow(author_table) == 0) {
    result <- DBI::dbWriteTable(
      dbcon,
      c(schema, 'author'),
      dataset_authors,
      row.names = FALSE, overwrite = FALSE, append = TRUE
    )
    cat(sprintf("%d authors added to DB.\n", nrow(dataset_authors)))
  } else {
    cat('Existing authorship assignments in DB:\n')
    print(author_table)
    stop("Please check and revise authorship manually.")
  }

}

# TODO: add tryCatch around DBIs to catch errors/warning, especially for readtable stuff (otherwise checks might not work, unintended consequences?)


add_organization <- function(new_org, org_table, dbcon, schema, fuzzy_thres = 0.25){
  # TODO: if interactive() ...

  # check for exact match on rorid
  matched_exact <- subset(
    org_table,
    (!is.na(rorid) & rorid == new_org$rorid)
  )
  # if there is an exact match:
  if (nrow(matched_exact) > 0) {
    res_combined <- matched_exact %>% dplyr::bind_rows(new_org) %>%
      tibble::add_column(source = c('DB','new'), .before = 1)
    cat("Organization already exists in DB (exact match on RORID):\n")
    print(res_combined)
    # if new information is different from DB entry:
    if (nrow(dplyr::distinct(res_combined, dplyr::pick(org_name:rorid)))>1) {
      sel_opt <- tolower(readline(prompt = "Should the DB entry be updated with the new information? [y/n]: "))
      switch(sel_opt,
             "y" = {
               DBI::dbGetQuery(
                 dbcon,
                 sprintf("UPDATE %s.organization SET (org_name, rorid, country_code) = ($1, nullif($2, 'NA'), $3) WHERE org_id = $4;", schema),
                 params = c(as.list(new_org), matched_exact$org_id)
               )
               cat("Organization entry successfully updated in DB.")
               org_entry <- cbind(org_id = matched_exact$org_id, new_org)
             },
             "n" = {
               cat("Organization entry up to date, no changes made to DB.")
               org_entry <- matched_exact
             },
             stop("Response not recognized. No changes made, run function again.")
      )
    } else {
      cat("Organization entry up to date, no changes made to DB.")
      org_entry <- matched_exact
    }
    return(org_entry[,c('org_id', 'org_name', 'rorid', 'country_code')]) # TODO: fix order, label?
  }


  # check for fuzzy match on name
  name_dist <- stringdist::stringdist(
    tolower(org_table$org_name),
    tolower(new_org$org_name),
    method = "jw")

  matched_fuzzy <- subset(org_table, name_dist < fuzzy_thres)

  if (nrow(matched_fuzzy) > 0) {
    res_combined <- matched_fuzzy %>% dplyr::bind_rows(new_org) %>%
      tibble::add_column(source = c('DB','new'), .before = 1)
    cat("Potential existing entries found in DB (fuzzy name match):\n")
    print(res_combined)
    # already know that there is no exact rorid match, so either insert or update needed
    sel_opt <- tolower(readline(prompt = "Is the organization already in the DB? [y/n]: "))
    switch(sel_opt,

      "y" = {
        # if there are multiple potential matches, ask user to select one
        if (nrow(matched_fuzzy) > 1) {
          sel_row<- readline(prompt = sprintf("Which ROW contains the match? [1-%d]: ", nrow(matched_fuzzy)))
          sel_row <- as.numeric(sel_row)
          if (is.na(sel_row) || !(sel_row %in% 1:nrow(matched_fuzzy))) {
            stop("Response not recognized. No changes made, run function again.")
          }
        } else {
          sel_row <- 1
        }
         # update DB entry or keep original information?
        sel_update <- tolower(readline(prompt = "Should the DB entry be updated with the new information? [y/n]: "))
        switch(sel_update,
          "y" = {
            DBI::dbGetQuery(
              dbcon,
              sprintf("UPDATE %s.organization SET (org_name, rorid, country_code) = ($1, nullif($2, 'NA'), $3) WHERE org_id = $4;", schema),
              params = c(as.list(new_org), matched_fuzzy$org_id)
            )
            cat("Organization entry successfully updated in DB.")
            org_entry <- cbind(org_id = matched_fuzzy$org_id, new_org)
          },
          "n" = {
            cat("Organization entry up to date, no changes made to DB.")
            org_entry <- matched_fuzzy[sel_row,]
          },
          stop("Response not recognized. No changes made, run function again.")
         )
       },

       # if there is no true match, insert the new organization
       "n" = {
         org_entry_id <- DBI::dbGetQuery(
           dbcon,
           sprintf("INSERT INTO %s.organization (org_name, rorid, country_code) VALUES ($1, nullif($2, 'NA'), $3)) RETURNING org_id;", schema),
           params = as.list(new_org)
         )
         cat("New organization added to DB.\n")
         org_entry <- cbind(org_entry_id, new_org)
       },

       stop("Response not recognized. No changes made, run function again.")
    )

    return(org_entry[,c('org_id', 'org_name', 'rorid', 'country_code')]) # TODO: fix order, label?
  }


  # else: insert the new organization
  org_entry_id <- DBI::dbGetQuery(
    dbcon,
    sprintf("INSERT INTO %s.organization (org_name, rorid, country_code) VALUES ($1, nullif($2, 'NA'), $3) RETURNING org_id;", schema),
    params = as.list(new_org)
  )
  cat("New organization added to DB.\n")
  org_entry <- cbind(org_entry_id, new_org)
  return(org_entry)

}



write_orgs_to_DB <- function(author_data_with_ids, funding_data, dbcon, schema, fuzzy_thres = 0.25){
  # TODO: org_label, inst_city???
  # TODO: what if twice the same organization in org_data, but slight differences? reread org_table after first update?

  # TODO: check/complete rorids and names with api?
  affiliations <- author_data_with_ids %>% dplyr::select(org_name:org_country, person_id) %>%
    dplyr::rename(
      rorid = org_rorid,
      country_code = org_country
    ) %>%
    dplyr::mutate(
      org_name = gsub("<br>"," ", org_name),
      country_code = stringr::str_match(country_code, "\\(([^)]+)\\)")[,2]
    ) %>%
    tidyr::nest(.by = c('org_name', 'rorid', 'country_code'))
  funding_insts <- funding_data %>%
    dplyr::rename(
      org_name = inst_name,
      rorid = inst_rorid,
      country_code = inst_country
    ) %>%
    dplyr::mutate(
      org_name = gsub("<br>"," ", org_name),
      country_code = stringr::str_match(country_code, "\\(([^)]+)\\)")[,2]
    ) %>%
    tidyr::nest(.by = c('org_name', 'rorid', 'country_code'))

  org_data <- affiliations %>%
    dplyr::full_join(funding_insts, by = c('org_name', 'rorid', 'country_code'),
                     suffix = c('.aff', '.fund'))

  org_table <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'organization'))

  org_entries <- data.frame(org_id = numeric(0),
                            org_name = character(0),
                            rorid = character(0),
                            country_code = character(0))

  for (k in 1:nrow(org_data)){
    new_org <- org_data[k,c('org_name', 'rorid', 'country_code')]
    cat(sprintf("\nOrganization %d of %d: %s -----------\n", k, nrow(org_data), new_org$org_name))

    org_entries[k,] <- add_organization(new_org, org_table, dbcon, schema, fuzzy_thres = 0.25)
    rownames(org_entries) <- NULL
  }

  return(dplyr::bind_cols(tibble::tibble(org_entries),
                          org_data[, c('data.aff', 'data.fund')]))
}


add_affiliations <- function(org_data_with_ids, dbcon, schema){
  # keep the person_ids in the affiliations / org_data?
  aff_data <- org_data_with_ids %>%
    dplyr::select(org_id, country_code, data.aff) %>%
    tidyr::unnest(cols = c('data.aff')) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('aff_'),
                                \(x) dplyr::if_else(x == "", NA, x))) %>%
    dplyr::rename(department = aff_dep) %>%
    dplyr::mutate(address = gsub("NA,* ", "", paste0(
      aff_street, ", ", aff_plz, " ", aff_city, ", ", country_code
    ))) %>%
    dplyr::mutate(address = dplyr::if_else(nchar(address)<3, NA, address)) %>%
    dplyr::select(person_id, org_id, department, address)

  # get existing entries
  tuple_str <- paste0(
    "(", aff_data$person_id, ",", aff_data$org_id, ")", collapse = ","
  )
  query <- sprintf(
    "SELECT * FROM %s.affiliation WHERE (person_id, org_id) IN (%s)", schema, tuple_str
  )
  aff_table <- DBI::dbGetQuery(
    dbcon,
    query
  )

  # rows in aff_data where (person_id, org_id) are also in aff_table
  aff_existing <- aff_data %>%
    dplyr::inner_join(aff_table, by = c("person_id", "org_id"), suffix = c('.DB','.new'))

  # rows in aff_data where (person_id, org_id) are NOT in aff_table
  aff_new <- aff_data %>% dplyr::anti_join(aff_table, by = c("person_id", "org_id"))

  if (nrow(aff_new)>0){
    # write the new affiliations to DB
    result <- DBI::dbWriteTable(
      dbcon,
      c(schema, 'affiliation'),
      aff_new,
      row.names = FALSE, overwrite = FALSE, append = TRUE
    )
    cat(sprintf('%d new affiliations written to DB.', nrow(aff_new)))
  } else {
    cat('No new affiliations in provided data, no changes made to DB.')
  }

  # warn if there are existing affiliations in DB for these authors + organizations
  if (nrow(aff_existing) > 0) {
    aff_existing <- aff_existing %>%
      # reformatting for output message:
      tidyr::pivot_longer(
        cols = dplyr::matches("\\.(new|DB)$"),
        names_to = c(".value", "source"),
        names_pattern = "^(.*)\\.(new|DB)$") %>%
      dplyr::mutate(entry_created_at = dplyr::if_else(source == 'new', as.POSIXct(NA), entry_created_at),
                    aff_id = dplyr::if_else(source == 'new', NA, aff_id)) %>%
      dplyr::relocate(source, .before = 1)
    cat('There are existing affiliation entries in DB for these authors and organizations:\n')
    print(aff_existing)
    warning("Please check and revise affiliations manually if necessary.")
  }

}


add_funding_sources <- function(ds_data_with_id, org_data_with_ids, dbcon, schema){
  fund_data <- org_data_with_ids %>%
    dplyr::select(org_id, country_code, data.fund) %>%
    tidyr::unnest(cols = c('data.fund')) %>%
    dplyr::mutate(grant_name = dplyr::if_else(grant_name == "", NA, grant_name),
                  ds_id = ds_data_with_id$ds_id) %>%
    dplyr::select(ds_id, org_id, grant_nr, grant_name)


  # get existing entries
  tuple_str <- paste0(
    "(", fund_data$ds_id, ",", fund_data$org_id, ")", collapse = ","
  )
  query <- sprintf(
    "SELECT * FROM %s.funding WHERE (ds_id, org_id) IN (%s)", schema, tuple_str
  )
  fund_table <- DBI::dbGetQuery(
    dbcon,
    query
  )

  # rows in fund_data where (ds_id, org_id) are NOT in ds_table
  fund_new <- fund_data %>% dplyr::anti_join(fund_table, by = c("ds_id", "org_id"))

  # write the new funding entries to DB
  if (nrow(fund_new) > 0) {
    result <- DBI::dbWriteTable(
      dbcon,
      c(schema, 'funding'),
      fund_new,
      row.names = FALSE, overwrite = FALSE, append = TRUE
    )
    cat(sprintf('%d new funding sources written to DB.', nrow(fund_new)))
  } else {
    cat('No new funding sources in provided data, no changes made to DB.')
  }

  # rows in aff_data where (person_id, org_id) are also in aff_table
  fund_existing <- fund_data %>%
    dplyr::inner_join(fund_table, by = c("ds_id", "org_id"), suffix = c('.DB','.new')) %>%
    # reformatting for output message:
    tidyr::pivot_longer(
      cols = dplyr::matches("\\.(new|DB)$"),
      names_to = c(".value", "source"),
      names_pattern = "^(.*)\\.(new|DB)$") %>%
    dplyr::relocate(source, .before = 1)

  # warn if there are existing affiliations in DB for these authors + organizations
  if (nrow(fund_existing) > 0) {
    cat('There are existing funding entries in DB for this dataset and provided organizations:\n')
    print(fund_existing)
    warning("Please check and revise funding sources manually if necessary.")
  }

}


add_related_resources <- function(ds_id, related_data, dbcon, schema){
  rel_data <- related_data
  for (col in c('DOI','XCELLID')){
    if (! col %in% names(related_data)){
      rel_data[,col] <- NA_character_
    }
  }

  # publications
  rel_pubs <- rel_data %>%
    dplyr::filter((is.na(rel_data$XCELLID) | rel_data$XCELLID == "")) %>%
    dplyr::rename(
      pub_info = Citation,
      doi = DOI
    ) %>%
    dplyr::mutate(
      pub_info = gsub("<br>"," ", pub_info),
      pub_info = gsub("\n","", pub_info),
      doi = dplyr::if_else(doi == "", NA_character_, doi),
    ) %>%
    dplyr::select(doi,pub_info)

  pub_table <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'publication'))
  rel_pubs <- rel_pubs %>% dplyr::left_join(pub_table, by = c('doi'), suffix = c('','.DB'))

  # check if publications (DOI) already exist in DB
  rel_pubs_new <- rel_pubs %>%
    dplyr::filter(is.na(pub_id)) #| (pub_info %in% rel_pubs$pub_info)

  # add new publications to db
  if (nrow(rel_pubs_new) > 0){
    # compose sql query: first paste the values together
    values_sql <- paste(
      apply(rel_pubs_new, 1, function(row) {
        stringr::str_glue("({DBI::dbQuoteString(dbcon, row['doi'])}, {DBI::dbQuoteString(dbcon, row['pub_info'])})")
      }),
      collapse = ", "
    )
    # then combine into insert query
    query <- stringr::str_glue("
      INSERT INTO {schema}.publication (doi, pub_info)
      VALUES {values_sql}
      RETURNING pub_id;
    ")

    pub_ids <- DBI::dbGetQuery(dbcon, query)
    rel_pubs[rownames(rel_pubs_new), 'pub_id'] <- pub_ids$pub_id

    cat(sprintf('%d/%d new publications written to DB.', nrow(rel_pubs_new), nrow(rel_pubs)))
    if (nrow(rel_pubs_new)< nrow(rel_pubs)){
      cat(sprintf('%d/%d publications already exist in DB.', nrow(rel_pubs) - nrow(rel_pubs_new), nrow(rel_pubs)))
    }
  }


  # add the relationships to the dataset
  if (nrow(rel_pubs)>0){
    rel_pubs_ds <- rel_pubs %>%
      dplyr::mutate(ds_id = ds_id) %>%
      dplyr::select(ds_id, pub_id)

    result <- DBI::dbWriteTable(
      dbcon,
      c(schema, 'related_publication'),
      rel_pubs_ds,
      row.names = FALSE, overwrite = FALSE, append = TRUE
    )
    cat(sprintf('%d publications related to dataset in DB.', nrow(rel_pubs_ds)))
  }


  # related datasets
  rel_ds_ds <- rel_data %>%
    dplyr::filter((!is.na(rel_data$XCELLID) & rel_data$XCELLID != ""))

  if (nrow(rel_ds_ds)>0){
    # add the relationships to the datasets
    rel_ds_ds <- rel_ds_ds %>%
      dplyr::mutate(
        rel_ds_id = as.integer(rel_data$XCELLID),
        org_ds_id = ds_id
      ) %>% dplyr::select(org_ds_id, rel_ds_id)
    result <- DBI::dbWriteTable(
      dbcon,
      c(schema, 'related_dataset'),
      rel_ds_ds,
      row.names = FALSE, overwrite = FALSE, append = TRUE
    )
    cat(sprintf('%d datasets related to this dataset in DB.', nrow(rel_ds_ds)))
  }

}


add_sites <- function(site_data, dbcon, schema){
  # get the field names for the main site table
  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'site'))
  site_data_main <- site_data %>%
    dplyr::rename(
      site_label = site_code,
      country_code = country,
      description = site_desc
    ) %>%
    dplyr::mutate(country_code = stringr::str_match(country_code, "\\(([^)]+)\\)")[,2]) %>%
    dplyr::select(dplyr::any_of(cols))

  # TODO: check if site already exists in DB
  # by coordinates / postgis
  # by code or name?

  # write the site data to DB


}

add_site_data <- function(){
  # need site_ids
  # find attr ids, map encodings
  # to long format

}


write_sites_to_db <- function(){

}



write_data_to_db <- function(complete_metadata,
                             QWA_data,
                             dbcon, schema){

  complete_metadata <- jsonlite::read_json("./explorations/collected_data_2025-05-20.json",simplifyVector = TRUE)
  dbcon <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = "xcell",
                          host = "pgdboapp.wsl.ch", # new: pgdbxcell.wsl.ch / old: pgdbtapp.wsl.ch or pgdboapp.wsl.ch
                          port = 5432,
                          user = "naegelin", #naegelin or xcell_edit
                          password = keyring::key_get("pgdbt_xcell", username = "naegelin")) #naegelin or xcell_edit

  schema <- 'v3'

  DBI::dbDisconnect(dbcon)

  # LEVEL 0 METADATA -----------------------------------------------------------
  # PERSON table
  author_data <- complete_metadata$author_data
  author_data_with_ids <- write_persons_to_DB(author_data, dbcon, schema, fuzzy_thres = 0.25)

  # DATASET table
  ds_data <- complete_metadata$ds_data
  ds_data_with_id <- add_new_DS_entry(ds_data, author_data_with_ids, dbcon, schema)

  # AUTHOR table
  add_authorships(ds_data_with_id, author_data_with_ids, dbcon, schema)

  # ORGANIZATION table
  funding_data <- complete_metadata$funding_data
  org_data_with_ids <- write_orgs_to_DB(author_data_with_ids, funding_data, dbcon, schema, fuzzy_thres = 0.25)

  # AFFILIATION table
  add_affiliations(org_data_with_ids, dbcon, schema)

  # FUNDING table
  add_funding_sources(ds_data_with_id, org_data_with_ids, dbcon, schema)

  # RELATED PUBS and DS
  related_data <- complete_metadata$doi_data
  add_related_resources(ds_data_with_id$ds_id, related_data, dbcon, schema)



  # LEVEL 1 METADATA -----------------------------------------------------------
  # SITE table





  # SITE: validate, check by coords, name? if exists, compare old data with new data (but leave up for manual? and check for locked/other ds)
  site_data <- complete_metadata$site
  site_data <- site_data %>%
    dplyr::rename(
      site_label = site_code,
      country_code = country,
      description = site_desc
    )
  site_data$elevation = c(100,200)

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'site'))
  site_data <- site_data %>% dplyr::select(dplyr::any_of(cols))
  site_data

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'site'),
                    value = site_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # TREE:
  # if same site, then check for same trees? how?
  tree_data <- complete_metadata$tree
  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'tree'))
  tree_data <- tree_data %>%
    dplyr::rename(
      tree_label = treecode,
      species_code = speciescode,
      site_label = sitecode
    )

  where_clause <- paste0("'", unique(tree_data$site_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT site_id, site_label FROM ", schema, ".site WHERE site_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  tree_data <- tree_data %>% dplyr::left_join(result, by = 'site_label')
  tree_data <- tree_data %>% dplyr::select(dplyr::any_of(cols))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'tree'),
                    value = tree_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)



  # WOODPIECE
  wp_data <- complete_metadata$wp_table

  wp_data <- wp_data %>%
    dplyr::rename(
      wp_label = wpcode,
      tree_label = treecode
    )


  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'woodpiece'))
  where_clause <- paste0("'", unique(wp_data$tree_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT tree_id, tree_label FROM ", schema, ".tree WHERE tree_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  wp_data <- wp_data %>% dplyr::left_join(result, by = 'tree_label')
  wp_data <- wp_data %>% dplyr::select(dplyr::any_of(cols))

  wp_data$collected_at <- now
  wp_data$organ <- 'stem'

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'woodpiece'),
                    value = wp_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # SLIDE
  slide_data <- complete_metadata$slide_table

  slide_data <- slide_data %>%
    dplyr::rename(
      slide_label = slidecode,
      wp_label = wpcode
    )

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'slide'))
  where_clause <- paste0("'", unique(slide_data$wp_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT wp_id, wp_label FROM ", schema, ".woodpiece WHERE wp_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  slide_data <- slide_data %>% dplyr::left_join(result, by = 'wp_label')
  slide_data <- slide_data %>% dplyr::select(dplyr::any_of(cols))

  slide_data$created_at <- now

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'slide'),
                    value = slide_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # IMAGE
  image_data <- complete_metadata$image_table

  image_data <- image_data %>%
    dplyr::rename(
      slide_label = slide_code,
      img_label = image_code
    )

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'image'))
  where_clause <- paste0("'", unique(image_data$slide_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT slide_id, slide_label FROM ", schema, ".slide WHERE slide_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  image_data <- image_data %>% dplyr::left_join(result, by = 'slide_label')
  image_data <- image_data %>% dplyr::select(dplyr::any_of(cols))

  image_data$created_at <- now

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'image'),
                    value = image_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # IMG ANALYSIS

  where_clause <- paste0("'", unique(image_data$img_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT img_id, img_label FROM ", schema, ".image WHERE img_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)


  img_analysis_data <- data.frame(
    img_id = result$img_id, # LOOKUP
    analysis_nr = 1,
    ds_id = 3, # FK: LOOKUP
    created_at = now,
    software = 'ROXAS',
    sw_version = 'vtest' # FROM DATA
  )

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'image_analysis'),
                    value = img_analysis_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # RING
  ring_data <- QWA_data$rings

  ring_key <- result
  ring_key$analysis_nr <- 1

  ring_data <- ring_data %>% dplyr::select(image_code, year) %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label'))


  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'ring'),
                    value = ring_data %>% dplyr::select(-image_code),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  ring_attr <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'ring_attribute'))
  ring_measurements <- QWA_data$rings %>% dplyr::select(image_code, year, mrw)

  ring_measurements <- ring_measurements %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label')) %>%
    tidyr::pivot_longer(
      cols = c('mrw'),
      names_to = 'ring_attr_name',
      values_to = 'val'
    )

  ring_measurements <- ring_measurements %>%
    dplyr::mutate(ring_attr_name = toupper(ring_attr_name)) %>%
    dplyr::left_join(ring_attr, by = c('ring_attr_name' = 'attr_label'))

  ring_measurements <- ring_measurements %>%
    dplyr::select(-ring_attr_name, -unit, -description, -encoding) %>% dplyr::filter(!is.na(val))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'ring_data'),
                    value = ring_measurements %>% dplyr::select(-image_code, -attr_name),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # CELL
  cell_data <- QWA_data$cells %>% dplyr::select(image_code, year, xpix, ypix) %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label'))


  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'cell'),
                    value = cell_data %>% dplyr::select(-image_code),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  cell_attr <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'cell_attribute'))
  cell_measurements <- QWA_data$cells %>% dplyr::select(image_code, year, xpix, ypix, raddistr, la, cwtpi)

  cell_measurements <- cell_measurements %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label')) %>%
    tidyr::pivot_longer(
      cols = c('raddistr', 'la', 'cwtpi'),
      names_to = 'cell_attr_name',
      values_to = 'val'
    )

  cell_measurements <- cell_measurements %>%
    dplyr::mutate(cell_attr_name = toupper(cell_attr_name)) %>%
    dplyr::left_join(cell_attr, by = c('cell_attr_name' = 'attr_label')) %>%
    dplyr::select(-attr_name, -unit, -description, -encoding) %>% dplyr::filter(!is.na(val))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'cell_data'),
                    value = cell_measurements %>% dplyr::select(-image_code, -cell_attr_name, -year),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # DBI::dbDisconnect(dbcon)
}

# tbl_name <- 'person'
# DBI::dbListFields(conn = dbcon, name = c(schema, tbl_name))
# DBI::dbReadTable(conn = dbcon, name = c(schema, tbl_name))






# saxor:
# xcldb_get_constrains <- function(dbcon, sch =  schema_id){
#
#   suppressWarnings(
#     r <- DBI::dbSendQuery(dbcon, "select conrelid::regclass AS table, pg_get_constraintdef(c.oid) AS constrains from   pg_constraint c where  contype in ('p','u') order by contype")
#   )
#
#   dbConstrains <- DBI::dbFetch(r)
#   DBI::dbClearResult(r)
#
#   suppressWarnings(dbConstrains %>%
#                      tibble::as_tibble() %>%
#                      #dplyr::filter( substr(constrains, 1, 6) %in% 'UNIQUE') %>%
#                      dplyr::mutate( constrains = gsub('UNIQUE [(]|[)]|["]|PRIMARY KEY [(]', '', constrains)) %>%
#                      tidyr::separate(constrains, letters, sep = ',') %>%
#                      tidyr::gather(lett, constrains, -table) %>%
#                      dplyr::mutate(constrains = trimws(constrains)) %>%
#                      dplyr::filter(!is.na(constrains), !constrains %in% 'id') %>%
#                      dplyr::select(-lett) %>%
#                      tidyr::separate(table, c('schema', 'table'), sep = '\\.') %>%
#                      dplyr::filter(schema %in% sch) %>%
#                      dplyr::arrange(table)) ->
#     constr.df
#
#   return(constr.df)
# }

# read table
# xcldb_tbl_x <- function(t = 'site', s = 'v1') {
#   dplyr::tbl(dbcon, dplyr::sql( paste0( "select * from ", s, '.', t) ) )
# }

#' Function first check if some id's need to be renamed: check if there are data
#' already in DB and keep id's for that part, and add the new ones
# xcldb_check_append_db

# xcldb_append_data <- function(d_table, table.name, sch =  schema_id){
#
#   if( nrow(d_table) > 0 ){
#
#     # rearange the columns
#     colls <- xcldb_tbl_x(table.name, sch) %>% colnames()
#
#     d_table <- dplyr::select(d_table, colls)
#
#     print(paste0('Adding ', nrow(d_table), ' entries to ', table.name, ' table.'))
#     DBI::dbWriteTable(conn = dbcon, name = c(sch, table.name), value = d_table, overwrite = FALSE, append = TRUE, row.names=FALSE)
#
#   }else{
#
#     print(paste0('No data to be added to ',  table.name, ' table.'))
#
#   }
#
# }
