

validate_QWA_data <- function(QWA_data,
                              rm_innermost_ring = TRUE){
  # get a list of all annual rings in cells data (distinct image_code, YEAR),
  # with added cell counts per ring
  df_rings_log <- QWA_data$cells %>%
    dplyr::count(image_code, YEAR, name = 'n_cells')

  # combine with rings data
  df_rings_log <- df_rings_log %>%
    dplyr::full_join(QWA_data$rings,
                     by = c('image_code', 'YEAR')) %>%
    dplyr::select(tree_code, sample_code, dplyr::everything()) %>%
    dplyr::group_by(image_code) %>% # fill in missing tree and sample codes by image
    tidyr::fill(tree_code, sample_code, .direction = 'downup') %>%
    dplyr::ungroup()

  # identify incomplete rings, missed rings
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(incomplete_ring = is.na(MRW),
                  missed_ring = is.na(n_cells),
                  missed_ringV2 = MRW < 10) # TODO: check V1 works, aligns with V2 with example of a missed ring

  # TODO: could we run into issues with years that would be otherwise removed
  # in the next steps automatically?
  # TODO: finalize, output when stopped? when not stopped?
  # check that the data are dated (i.e., YEAR is not NA, and not in future)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(undated = is.na(YEAR) | YEAR > current_year)

  if (sum(df_rings_log$undated) > 0){
    beepr::beep(sound = 2, expr = NULL)
    stop('The following trees have undated samples:',
         paste0(unique(df_rings_log[df_rings_log$undated, 'tree_code']), collapse=', '),
         'Please ensure that all included samples are dated, then restart the process.')
  }

  # check that the cell data include cell wall thickness estimates
  # (i.e., at least some cells per image need to have a nonNAN CWT value)
  # TODO: does it matter which CWT measure we use here?
  # TODO: could also do this on cell files directly?
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(no_CWT = all(is.na(CWTTAN))) %>%
    dplyr::ungroup()

  if (sum(df_rings_log$no_CWT) > 0){
    beepr::beep(sound = 2, expr = NULL)
    stop('The following trees have samples without cell wall thickness estimation:',
         paste0(unique(df_rings_log[df_rings_log$no_CWT, 'tree_code']), collapse=', '),
         'Please ensure that all included samples have CWT estimates, then restart the process.')
  }

  # now we now that undated and no_CWT are ALL FALSE, so we can drop them
  df_rings_log <- df_rings_log %>% dplyr::select(-undated, -no_CWT)

  # identify the innermost year per tree
  # NOTE: because of how ROXAS works, the innermost ring should always be excluded
  # TODO: check with Georg. does this also hold true for ROXAS AI?
  if (rm_innermost_ring){
    df_rings_log <- df_rings_log %>%
      dplyr::group_by(tree_code) %>%
      dplyr::mutate(innermost_year = YEAR == min(YEAR)) %>%
      dplyr::ungroup()
  }
  else {
    df_rings_log <- df_rings_log %>%
      dplyr::mutate(innermost_year = FALSE)
    beepr::beep(sound = 10, expr = NULL)
    warning("Due to a ROXAS quirk, the innermost ring per tree should generally\n",
            "always be excluded from further analysis. You have chosen to\n",
            "override this step.\n")
  }

  # filter out data from years with identified issues
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(invalid_rm = incomplete_ring | missed_ring | innermost_year)

  years_to_remove <- df_rings_log[df_rings_log[['invalid_rm']],
                                  c('tree_code','image_code','YEAR')]
  df_cells_valid <- QWA_data$cells %>%
    dplyr::anti_join(years_to_remove, by=c('image_code','YEAR'))

  beepr::beep(sound = 1, expr = NULL)
  message("QWA data has been validated successfully!\n",
          'In total, ', nrow(years_to_remove),
          ' invalid years were removed from the data from the treecodes:\n',
          paste(unique(years_to_remove$tree_code), collapse=', '))
  # FCT OUTPUT
  return(
    setNames(
      list(df_cells_valid, df_rings_log),
      c('cells','rings'))
  )
}

remove_double_rings <- function(QWA_data){
  # DOUBLE RINGS
  # find double rings (i.e., the same year in two or more (sub-)samples due to
  # images or samples overlapping)
  # TODO: group by subsample or sample?
  # for years (grouped by tree) where there are multiple valid rings, keep only the one with the most cells
  df_rings_log <-

  df_rings_log <- QWA_data$rings %>%
    dplyr::group_by(tree_code, YEAR) %>%
    dplyr::filter(!invalid_rm) %>%
    dplyr::mutate(overlap = dplyr::n() > 1,
                  overlap_rm = overlap & (n_cells < max(n_cells))) %>%
    dplyr::ungroup()

  years_to_remove <- df_rings_log[df_rings_log[['overlap_rm']],
                                  c('tree_code','image_code','YEAR')]
  df_cells_clean <- QWA_data$cells %>%
    dplyr::anti_join(years_to_remove, by=c('image_code','YEAR'))

  beepr::beep(sound = 1, expr = NULL)
  message("Double rings have been removed successfully!\n",
          'In total, ', nrow(years_to_remove),
          ' double rings were removed from the data from the treecodes:\n',
          paste(unique(years_to_remove$tree_code), collapse=', '))

  return(
    setNames(
      list(df_cells_clean, df_rings_log),
      c('cells','rings'))
  )
}


plot_tree_coverage <- function(tree, df_rings,
                               show_plot = TRUE,
                               save_plot = FALSE, path_out = './') {
  # TODO: check that we have the relevant columns, only one tree
  # assert correct format for other inputs

  df_plot <- df_rings %>% dplyr::filter(tree_code == tree) %>%
    dplyr::select(tree_code, sample_code, image_code, YEAR, n_cells,
                  invalid_rm, overlap_rm) %>%
    dplyr::mutate(removed = dplyr::if_else(invalid_rm, 'invalid',
                                           dplyr::if_else(overlap_rm, 'double_ring', NA)))

  df_summary <-  df_plot %>%
    dplyr::group_by(sample_code, image_code) %>%
    dplyr::summarise(min_year = min(YEAR),
                     max_year = max(YEAR),
                     .groups = "drop") %>%
    dplyr::arrange(min_year)

  # PLOTTING
  # create a plot with the years covered by each image as horizontal bars
  p_cov <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = df_summary,
      ggplot2::aes(x = min_year-0.4, xend = max_year+0.4,
                   y = image_code, color = sample_code),
      position = ggplot2::position_nudge(y=-0.2), linewidth = 3
    ) +
    ggplot2::scale_color_manual(
      values = rep(c("grey70", "grey90"), nrow(df_summary)),
      guide = 'none')

  # add individual years with dots colored by number of cells
  p_cov <- p_cov +
    ggplot2::geom_point(
      data = df_plot,
      ggplot2::aes(x = YEAR, y = image_code, fill = n_cells),
      size = 3, shape = 21, stroke = 0
    ) +
    ggplot2::scale_fill_steps(low='lightskyblue', high='blue4',
                              name = "N cells")

  # add crosses for the years with values in removed, colored by the reason
  p_cov <- p_cov +
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      data = df_plot %>% dplyr::filter(!is.na(removed)),
      ggplot2::aes(x = YEAR, y = image_code, color = factor(removed)),
      shape = 4, size = 2, stroke = 1
    ) +
    ggplot2::scale_color_manual(values = c('red4','firebrick1'),
                                breaks = c('invalid', 'double_ring'),
                                labels = c('invalid ring', 'double ring'),
                                name = 'Removed years')

  # styling
  p_cov <- p_cov +
    ggplot2::labs(
      title = paste("Year coverage for tree", tree),
      x = "Year", y = "Image") +
    ggplot2::theme_minimal()

  # show plot
  if (show_plot){
    print(p_cov)
  }

  # save plot
  if (save_plot){
    # Save the plot to png
    plot_name <- file.path(path_out, paste0(tree,'_coverage.png')) # TODO: make path safe
    # make plot size dynamically dependent on nr of rows (images) and total
    # range of years, with some hard limits to avoid extremes (all in px)
    # TODO: check if this is the best way to set the size
    image_width <- min(
      max((max(df_summary$max_year)-min(df_summary$min_year))*75+800, 2000),
      6000)
    image_height <- min(
      max(nrow(df_summary)*100+200, 750),
      2000)

    ggplot2::ggsave(plot_name, p_cov, bg='white',
                    width = image_width, height = image_height, units='px')

    message(tree, ': coverage plot saved to ', plot_name)
  }
}

create_coverage_plots <- function(df_rings,
                                  show_plot = TRUE,
                                  save_plot = TRUE, path_out = './'){
  tree_codes <- unique(df_rings$tree_code)
  for (tree in tree_codes){
    plot_tree_coverage(tree, df_rings,
                       show_plot = show_plot,
                       save_plot = save_plot, path_out = path_out)
  }
}

# manual exclusions?
#
df_rings_log <- QWA_data$rings %>% dplyr::mutate(user_rm = FALSE)
# # EITHER: write to file and edit
# # write.csv(df_rings_log, 'rings_log.csv')
# # OPEN IN EDITOR AND SET user_rm TO TRUE FOR THE RINGS TO BE REMOVED
# # df_rings_log <- read.csv('rings_log.csv')
# # OR: manually set the user_rm column
years_to_exclude = list(
# c('image_code', YEAR),
  c('POG_PISY_02B_4_1', 1962),
  c('POG_PISY_02B_4_1', 1964),
  c('POG_PISY_02B_4_2', 1961)
)

yte_vals = lapply(years_to_exclude, \(x) paste(x, collapse ='_'))
df_rings_log = df_rings_log %>% dplyr::mutate(user_rm = paste(image_code, YEAR, sep='_') %in% yte_vals)




  # OUTLIERS

  # EXTRA MEASURES






flag_problem_rings <- function(df_rings_raw, df_cells_raw,
                               max_val_year = 2500,
                               min_val_MRW = 10){

  # TODO: check input vals

  # 1. get a list of all yearrings in cells data (distinct image_code, YEAR),
  # with added cell counts per ring
  df_rings_flags <- df_cells_raw %>%
    dplyr::count(tree_code, image_code, YEAR, name = 'n_cells')

  # 2. combine with the raw rings data and add a column to log whether a
  # yearring is removed from the data
  df_rings_flags <- df_rings_flags %>%
  dplyr::left_join(df_rings_raw,
                   by = c('tree_code', 'image_code', 'YEAR')) %>%
    dplyr::mutate(removed = FALSE,
                  removed_bc = NA)

  # 3. flag which (cell) years have no corresponding row in rings output file
  # NOTE: There are sometimes cells from incomplete rings, e.g. at the edge of
  # a subsample. These cells are given a year value, and cell measurements are
  # written to the cells output files, but the corresponding row in the rings
  # output does not exist. Using a join on the raw rings df in the previous step
  # would give us NA values for the ring measurements of these years.
  df_rings_flags <- df_rings_flags %>%
    dplyr::mutate(no_rings_data = is.na(MRW))

  # 3.&4. flag the rings which have invalid (i.e., in-future) or NA year values,
  # and those which have an MRW value below the threshold
  df_rings_flags <- df_rings_flags %>%
    dplyr::mutate(inv_year = YEAR > max_val_year | is.na(YEAR),
                  small_MRW = MRW < min_val_MRW)

  # 5. flag the rings which have no CWT estimates
  # TODO: is noCWT flag given on an year or image basis?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(no_CWT = all(is.na(CWTTAN))) %>%
    dplyr::ungroup()

  # 6. flag the duplicated rings
  # NOTE: here is usually overlap between images, meaning that the same year can
  # be represented in two or more subsamples. Of all duplicates within a tree
  # (sample?) we then flag all but the ring with the max cell count for removal
  # TODO: across samples? or only subsamples?
  # TODO: what if the max count ring will already be removed due to other reasons?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(tree_code, YEAR) %>%
    dplyr::mutate(duplicate = dplyr::n() > 1,
                  duplicate_fewer_cells = duplicate & (n_cells < max(n_cells))) %>%
    dplyr::ungroup()

  # 7. flag the innermost year
  # The very first yearring from the innermost sample from a tree is usually
  # too close to the pith and flagged for removal
  # TODO: always removed?
  # TODO: overall or only after other checks?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(tree_code) %>%
    dplyr::mutate(innermost_year = YEAR == min(YEAR)) %>%
    dplyr::ungroup()

  return(df_rings_flags)
}


clean_raw_data <- function(df_rings_flags, df_cells_raw, flags_remove){
  ####
  # Remove flagged data

  # Set based on which flags we should remove data
  # if set to TRUE, the corresponding column in df_rings_flags will be used to
  # filter out the flagged data



  # 1 remove the noisy data based on the selected flags
  sel_flags <- names(flags_remove[flags_remove])

  for (flag in sel_flags) {
    df_rings_flags[
      (!df_rings_flags['removed']) & # not yet removed
        (df_rings_flags[flag])       # with the flag set to TRUE
      , c('removed','removed_bc')] <- list(TRUE,flag)
  }

  # df_rings_flags <- df_rings_flags %>%
  #   dplyr::mutate(
  #     removed = rowSums(dplyr::across(dplyr::all_of(sel_flags))) > 0)

  years_to_remove <- df_rings_flags[df_rings_flags[['removed']],
                                   c('tree_code','image_code','YEAR')]
  df_cells_clean <- df_cells_raw %>%
    dplyr::anti_join(years_to_remove, by=c('tree_code','image_code','YEAR'))

  if (sum(df_rings_flags$removed) > 0){
    message('In total, ', nrow(years_to_remove),
            ' years were removed from the data from the treecodes\n',
             paste(unique(years_to_remove$tree_code), collapse=', '),
            '\ndue to the following flags (see fct_output$rings for details):',
            paste0(capture.output(table(df_rings_flags$removed_bc)), collapse = "\n"))
  }
  # for (flag in names(flags_remove)) {
  #   flag <- 'no_rings_data'
  #   df_rings_flags[df_rings_flags[[flag]],c('tree_code','image_code','YEAR')]
  # }
  #

  return(
    setNames(
      list(df_rings_flags, df_cells_clean),
      c('rings','cells'))
  )
}








plot_coverage <- function(tree, df_rings_clean, df_meta,
                          show_plot = TRUE, save_plot = TRUE, path_plots = './') {
  # create the data for the plot
  df_plot <- df_rings_clean %>%
    dplyr::filter(tree_code == tree) %>%
    dplyr::left_join(df_meta[c('image_code', 'tree_code', 'sample','subsample')],
                     by=c('tree_code', 'image_code')) %>%
    dplyr::select(image_code, sample, subsample, YEAR, n_cells, removed)

  df_summary <-  df_plot %>%
    dplyr::group_by(sample, subsample, image_code) %>%
    dplyr::summarise(min_year = min(YEAR),
                     max_year = max(YEAR),
                     .groups = "drop") %>%
    dplyr::arrange(min_year)

  # PLOTTING
  # Create a plot with each subsample coverage as bars
  p_cov <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = df_summary,
      ggplot2::aes(x = min_year-0.5, xend = max_year+0.5,
                   y = image_code, color = sample),
      position = ggplot2::position_nudge(y=-0.2), linewidth = 3
    ) +
    ggplot2::scale_color_manual(
      values=rep(c("grey75", "grey90"), nrow(df_summary)),
      guide='none')

  # Add individual years with points colored by number of cells
  p_cov <- p_cov +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_point(
      data = df_plot,
      ggplot2::aes(x = YEAR, y = image_code, color = n_cells),
      size=2
    ) +
    ggplot2::scale_colour_steps(low='lightskyblue', high='blue4',
                                name = "N cells")

  # Highlight the removed years with crosses
  p_cov <- p_cov +
    ggplot2::geom_point(
      data = df_plot %>% dplyr::filter(removed == TRUE),
      ggplot2::aes(x = YEAR, y = image_code, shape = "removed"), # dummy mapping for legend
      color = 'red', size = 2, stroke = 1
    ) +
    ggplot2::scale_shape_manual(values = c("removed" = 4), name=NULL)

  # Styling
  p_cov <- p_cov +
    ggplot2::labs(
      title = paste("Year coverage for tree", tree),
      x = "Year", y = "Image") +
    ggplot2::theme_minimal()

  if (show_plot){
    print(p_cov)
  }

  if (save_plot){
    # Save the plot to png
    plot_name <- file.path(path_plots, paste0(tree,'.png')) # TODO: make path safe
    # make plot size dynamically dependent on nr of rows (images) and total
    # range of years, with some hard limits to avoid extremes (all in px)
    image_width <- min(
      max((max(df_summary$max_year)-min(df_summary$min_year))*75+800, 2000),
      6000)
    image_height <- min(
      max(nrow(df_summary)*100+200, 750),
      2000)

    ggplot2::ggsave(plot_name, p_cov, bg='white',
                    width = image_width, height = image_height, units='px')

    message(tree, ': coverage plot saved to ', plot_name)
  }

}


# calculate additional measures (see rxs complete)
complete_cell_measures <- function(df_cells_clean){

  # df_cells_compl <- clean_data$cells %>%
  #   dplyr::mutate(CWTmax = pmax(CWTPI, CWTBA, CWTLE, CWTRI, na.rm=TRUE), # TODO: these can be sign flipped if outliers -> how to handle in max?
  #                 LR = sqrt(LA / pi),
  #                 CWAmax = 2 * (pi * (LR + CWTmax)^2 - LA),
  #                 # replace too high values with NA
  #                 # NOTE: CWA from Roxas can have negative values to indicate outliers
  #                 CWA = ifelse(abs(CWA) > CWAmax, NA, CWA),
  #                 RWD = ifelse(abs(CWA) > CWAmax, NA, RWD),
  #                 # new:
  #                 TCA = ifelse(CWA > 0, LA + CWA, ifelse(is.na(CWA), NA, -(LA-CWA))), # if CWA negative, also make it negative?
  #                 a = 2*sqrt(ASP*LA/pi),
  #                 b = a/ASP,
  #                 DH = sqrt((2*a^2*b^2)/(a^2+b^2)), # TODO: why not from Roxas?
  #                 RWD2 = CWTRAD/DRAD,
  #   )

  # TODO: replace all negatives with NA ?
  # TODO check these additional calculations

  return(df_cells_clean)
}






# TODO: what to do with the faulty data: remove? flag only? tell to restart? allow for dating manually?
# saxor / homogenizer remove only the cells with no rings or na years (but should not exist anyway)
# warn about no dates, no CWT, ask to start over with updated output files or remove from sel treecodes for future steps
# homogenzier later removes any flagged cells
# track with flags in diagnostics table on a yearly basis



# in theory, after this, data could be uploaded
# however, we might want to add EWLW estimation based measures from Profile()
# additional Diagnostics, and manual YTE

