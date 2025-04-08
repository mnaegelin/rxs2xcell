# get country ISO codes
countries_list <- unname(get_country_codes())

site_table_str <- data.frame(
  sitecode = character(0),
  sitename = character(0),
  country = factor(character(0), levels = countries_list),
  latitude = numeric(0),
  longitude = numeric(0),
  n_tree = integer(0),
  sitedesc = character(0),
  stringsAsFactors = FALSE
)
site_table_cols <- c(
  "sitecode" = "Site Code",
  "sitename" = "Site Name",
  "country" = "Country",
  "latitude" = "Latitude",
  "longitude" = "Longitude",
  "n_tree" = "Nr of Trees",
  "sitedesc" = "Site Description"
)

# Custom JS code for cell rendering
renderer_no_dupl <- "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        var data = instance.getDataAtCol(col);
        var duplicates = data.filter(function(val, index, arr) {
          return arr.indexOf(val) !== index && val === value;
        });
        if (duplicates.length > 0) {
          td.style.background = 'lightcoral';
        }
      }"


site_server <- function(id, main_session, prefilled_meta) {
  moduleServer(id, function(input, output, session) {

    # initialize site_data reactives
    site_data <- reactiveValues(
      df_in = site_table_str,
      df_out = NULL
    )

    # get initial site info from df_meta
    # TODO: warn that manual input here will be lost if df_meta is changed!
    observeEvent(prefilled_meta(), {
      if (!is.null(prefilled_meta()$df)) {
        df_meta <- prefilled_meta()$df
        df_in <- site_data$df_in
        # get unique site codes and nr of trees per site
        sites_trees <- df_meta %>%
          dplyr::group_by(site) %>%
          dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
          dplyr::rename(sitecode = site, n_tree = n)
        # add to site df and update reactive
        site_data$df_in <- df_in %>%
          dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
      }
    })


    # Render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        site_data$df_in,
        rowHeaders = TRUE,
        #stretchH = "all",
        height = '150',
        overflow = "visible",
        colHeaders = c("Site Code", "Site Name", "Country", "Latitude",
                       "Longitude", "Nr of Trees", "Site Description")
        ) %>%
      rhandsontable::hot_col("Site Code", readOnly = TRUE) %>%
      rhandsontable::hot_col("Site Name", renderer = renderer_no_dupl) %>%
      rhandsontable::hot_col("Country", allowInvalid = TRUE) %>%
      rhandsontable::hot_validate_numeric('Latitude', min = -90, max = 90) %>%
      rhandsontable::hot_validate_numeric('Longitude', min = -180, max = 180) %>%
      rhandsontable::hot_col('Nr of Trees', readOnly = TRUE) %>%
      rhandsontable::hot_cols(fixedColumnsLeft = 1)


      # rhandsontable::rhandsontable(
      #   site_data(),
      #   rowHeaders = T,
      #   stretchH = "all",
      #   colHeaders = c('Site Code', 'Site Name', 'Country',
      #                  'Latitude', 'Longitude', 'Nr of Trees')) %>%
      #   rhandsontable::hot_col('country', type = 'select',
      #                          source = unname(countries_list), allowInvalid = TRUE) %>%
      #   rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })



    # Update data frame on table edit
    # observe({
    #   if (!is.null(input$site_table)) {
    #     updated_data <- rhandsontable::hot_to_r(input$site_table)
    #     site_data(updated_data)
    #   }
    # })

    output$testing <- renderPrint({
      input$site_table
    })
  })
}


# site_data <- reactiveVal(value = NULL)
# observeEvent({
#   input$site_table
#   prefilled_meta()},{
#     if (is.null(input$site_table))  {
#       df_site <- data.frame(
#         sitecode = character(0),
#         sitename = character(0),
#         country = character(0),
#         latitude = numeric(0),
#         longitude = numeric(0),
#         n_tree = integer(0),
#         stringsAsFactors = FALSE
#       )
#       if (!is.null(prefilled_meta()$df)){
#         df_meta <- prefilled_meta()$df
#         # get unique site codes and nr of trees per site
#         sites_trees <- df_meta %>%
#           dplyr::group_by(site) %>%
#           dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
#           dplyr::rename(sitecode = site, n_tree = n)
#         # add to site df and update reactive
#         df_site <- df_site %>%
#           dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
#       }
#       df_site$country <- factor(rep(NA_character_,nrow(df_site)),
#                                 levels = countries_list, ordered = TRUE)
#       isolate(site_data(df_site))
#     }
#     # else {
#     #   df_site <- rhandsontable::hot_to_r(input$site_table)
#     #   output$testing <- renderPrint({
#     #     df_site
#     #   })
#     #   if (!is.numeric(df_site$latitude)) {
#     #     isolate(dfOld <- site_data())
#     #     isolate(site_data(dfOld))
#     #   } else {isolate(site_data(df_site)) }
#     # }
#
#   })


#
# observeEvent(prefilled_meta(), {
#   df_meta <- prefilled_meta()$df
#   df_site <- site_data()
#   if (!is.null(df_meta)) {
#     # get unique site codes and nr of trees per site
#     sites_trees <- df_meta %>%
#       dplyr::group_by(site) %>%
#       dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
#       dplyr::rename(sitecode = site, n_tree = n)
#     # add to site df and update reactive
#     df_site <- df_site %>%
#       dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
#     # initialize
#     df_site$country <- factor(rep(NA_character_,nrow(df_site)),
#                               levels = countries_list, ordered = TRUE)
#     site_data(df_site)
#   }
# })

# generate_unique_codes <- function(n, length) {
#   unique_codes <- character(0)
#   while(length(unique_codes) < n) {
#     new_codes <- replicate(n, paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = ""))
#     unique_codes <- unique(c(unique_codes, new_codes))
#     unique_codes <- unique_codes[1:n]  # Ensure we have exactly n unique codes
#   }
#   unique_codes
# }
#
# # Generate 250 unique character codes of length 5
# unique_codes <- generate_unique_codes(250, 5)
#
# countries_list <- unname(get_country_codes())

# DF = data.frame(integer = 1:10,
#                 numeric = rnorm(10),
#                 logical = rep(TRUE, 10),
#                 character = LETTERS[1:10],
#                 factor = factor(letters[1:10], levels = letters[10:1], ordered = TRUE),
#                 factor_allow = factor(rep(NA_character_,10),levels = countries_list, ordered = TRUE),
#                 date = seq(from = Sys.Date(), by = "days", length.out = 10),
#                 stringsAsFactors = FALSE)



