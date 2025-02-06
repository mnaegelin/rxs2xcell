#' Validate the raw QWA data
#'
#' Initial checks to ensure the quality of the raw QWA data.
#' This function checks for the following issues:
#' - undated images (i.e. YEAR is NA or in the future)
#' - images without cell wall thickness estimates
#' If any images with with these issues are found, the function will stop and
#' ask the user to correct the raw data before proceeding.
#'
#' Next, the function identifies (and removes?) the rings with the following issues:
#' - incomplete rings
#' - missed rings
#' - innermost rings
#' Here, by incomplete rings we mean those at the borders of images, where some
#' cells where recognized but a MRW could not be estimated, so there is no
#' corresponding entry in the rings data. Missed rings are those that may have
#' been manually added in ROXAS, leading to an entry in the rings data. But such
#' rings contain no actual cells, so there is no entry in the cells data.
#' Innermost ring is the ring closest to the pith (for all samples from a tree).
#' Due to the way ROXAS works, this ring should generally be excluded from
#' further analysis.
#'
#' @param QWA_data
#' @param rm_innermost_ring
#' @returns validated QWA_data.
#' @export
validate_QWA_data <- function(QWA_data,
                              remove_issues = TRUE,
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

  # now we know that undated and no_CWT are ALL FALSE, so we can drop them
  df_rings_log <- df_rings_log %>% dplyr::select(-undated, -no_CWT)

  # identify the innermost year per tree
  # NOTE: because of how ROXAS works, the innermost ring should always be excluded
  # TODO: check with Georg. does this also hold true for ROXAS AI?
  if (rm_innermost_ring){
    df_rings_log <- df_rings_log %>%
      dplyr::group_by(tree_code) %>%
      dplyr::mutate(innermost_ring = YEAR == min(YEAR)) %>%
      dplyr::ungroup()
  }
  else {
    df_rings_log <- df_rings_log %>%
      dplyr::mutate(innermost_ring = FALSE)
    beepr::beep(sound = 10, expr = NULL)
    warning("Due to a ROXAS quirk, the innermost ring per tree should generally\n",
            "always be excluded from further analysis. You have chosen to\n",
            "override this step.\n")
  }

  # TODO: filter or flag
  # filter out (or flag) data from years with identified issues
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(invalid_ring = incomplete_ring | missed_ring | innermost_ring)

  years_to_remove <- df_rings_log[df_rings_log[['invalid_ring']],
                                  c('tree_code','image_code','YEAR')]

  if (remove_issues){
    df_cells_valid <- QWA_data$cells %>%
      dplyr::anti_join(years_to_remove, by=c('image_code','YEAR'))

    beepr::beep(sound = 1, expr = NULL)
    message("QWA data have been validated successfully!\n",
            'In total, ', nrow(years_to_remove),
            ' invalid years were removed from the treecodes:\n',
            paste(unique(years_to_remove$tree_code), collapse=', '))
  }
  else{
    df_cells_valid <- QWA_data$cells
    beepr::beep(sound = 1, expr = NULL)
    message("QWA data have been validated successfully!\n",
            'In total, ', nrow(years_to_remove),
            ' invalid years were flagged for the treecodes:\n',
            paste(unique(years_to_remove$tree_code), collapse=', '))
  }

  return(
    setNames(
      list(df_cells_valid, df_rings_log),
      c('cells','rings'))
  )
}

#' Remove double rings
#'
#' Images from the same sample (core) may have some overlap, meaning that there
#' are duplicated annual rings (double rings) in the data. This function
#' identifies and removes these double rings, keeping only the ring with the
#' most cells.
#'
#' @param QWA_data
#' @returns QWA_data with double rings removed (or flagged).
#' @export
remove_double_rings <- function(QWA_data,
                                remove_doubles = TRUE){

  # check QWA data were validated (that QWA_data$rings has a column invalid_ring)
  if (!'invalid_ring' %in% colnames(QWA_data$rings)){
    beepr::beep(sound = 2)
    stop("The provided dataframe does not contain validation information.\n",
         "Please run validate_QWA_data() first.")
  }

  # find double rings (i.e., the same year in two or more (sub-)samples due to
  # images or samples overlapping)
  # TODO: group by tree or sample?
  # for years (grouped by tree) where there are multiple valid rings,
  # keep only the one with the most cells
  df_rings_log <- QWA_data$rings %>%
    dplyr::group_by(tree_code, YEAR) %>%
    dplyr::mutate(overlap = dplyr::n() > 1) %>%
    dplyr::ungroup()

  # find the double rings which do NOT have the max number of cells (while
  # excluding invalid rings from possible candidates), and thus will be removed/flagged
  df_overlap_rm <- df_rings_log %>%
    dplyr::filter(overlap, !invalid_ring) %>%
    dplyr::arrange(tree_code, YEAR, desc(n_cells)) %>%
    dplyr::group_by(tree_code, YEAR) %>%
    dplyr::slice(-1) %>% # take all but the first (which has max n_cell)
    dplyr::select(tree_code, image_code, YEAR) %>%
    dplyr::mutate(double_ring = TRUE)

  df_rings_log <- df_rings_log %>%
    dplyr::left_join(df_overlap_rm, by = c("tree_code", "image_code", "YEAR")) %>%
    dplyr::mutate(double_ring = ifelse(is.na(double_ring), FALSE, double_ring))

  years_to_remove <- df_rings_log[df_rings_log[['double_ring']],
                                  c('tree_code', 'image_code','YEAR')]
  # TODO: flag or remove
  if (remove_doubles){
  df_cells_clean <- QWA_data$cells %>%
    dplyr::anti_join(years_to_remove, by=c('image_code','YEAR'))
  } else {
    df_cells_clean <- QWA_data$cells
  }

  beepr::beep(sound = 1, expr = NULL)
  message("Double rings have been flagged/removed successfully!\n",
          'In total, ', nrow(years_to_remove),
          ' double rings were removed from the treecodes:\n',
          paste(unique(years_to_remove$tree_code), collapse=', '))

  return(
    setNames(
      list(df_cells_clean, df_rings_log),
      c('cells','rings'))
  )
}


#' Plot overview of annual coverage of images
#'
#' For a single tree, each image covers a span of years. This function allows
#' to visualize which annual rings are covered by which image and which rings
#' have been flagged due to issues or double rings.
#'
#' @param tree the treecode for which the plot should be created
#' @param df_rings the dataframe containing the rings data (including flags)
#' @param show_plot should the plot be shown
#' @param save_plot should the plot be written to disk under path_out
#' @param path_out path where the plot should be saved.
#'
#' @export
plot_tree_coverage <- function(tree, df_rings,
                               show_plot = TRUE,
                               save_plot = FALSE, path_out = './') {

  # assert that the df_rings underwent validation and double rings checks
  beepr::beep_on_error(
    checkmate::assert_names(colnames(df_rings), must.include = c('invalid_ring', 'double_ring')),
    sound = 2
  )

  # check that df_rings contains data from the provided tree code
  df_plot <- df_rings %>% dplyr::filter(tree_code == tree)
  if (nrow(df_plot) < 1){
    beepr::beep(sound=2)
    stop("The provided dataframe does not contain data from tree ", tree)
  }
  # TODO: additional input checks

  df_plot <- df_plot %>%
    dplyr::select(tree_code, sample_code, image_code, YEAR, n_cells,
                  invalid_ring, double_ring) %>%
    dplyr::mutate(removed = dplyr::if_else(invalid_ring, 'invalid ring',
                                           dplyr::if_else(double_ring, 'double ring', NA)))

  df_summary <- df_plot %>%
    dplyr::group_by(sample_code, image_code) %>%
    dplyr::summarise(min_year = min(YEAR),
                     max_year = max(YEAR),
                     .groups = "drop") %>%
    dplyr::arrange(min_year) %>%
    dplyr::mutate(image_code = factor(image_code, levels=image_code)) # correct y axis order

  # PLOTTING
  # create a plot with the years covered by each image as horizontal bars
  p_cov <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = df_summary,
      ggplot2::aes(x = min_year-0.4, xend = max_year+0.4,
                   y = image_code),
      position = ggplot2::position_nudge(y=-0.2), linewidth = 3,
      color = "grey75"
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(sample_code), scales = 'free_y') +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "grey25"))


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
                                breaks = c('invalid ring', 'double ring'),
                                name = 'Removed years')

  # axes labels and title
  p_cov <- p_cov +
    ggplot2::labs(
      title = paste("Year coverage for tree", tree),
      x = "Year", y = "Image")


  # show plot
  if (show_plot){
    print(p_cov)
  }

  # save plot
  if (save_plot){
    # save the plot to png
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


#' Create coverage plots for all trees
#'
#' Function to create coverage plots for all unique treecodes in the provided
#' rings df (usually QWA_data$rings) in a for loop. Plots can be shown and/or
#' saved to a specified path.
#'
#' @param df_rings rings dataframe
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



# Manual exclusions
remove_problem_rings <- function(QWA_data, years_to_exclude){
  df_rings_log <- QWA_data$rings %>%
    dplyr::left_join(years_to_exclude %>% dplyr::mutate(manual_excl = TRUE),
                    by = c('image_code','YEAR')) %>%
    dplyr::mutate(manual_excl = ifelse(is.na(manual_excl), FALSE, manual_excl))

  # remove the years with manual_excl == TRUE
  df_cells_clean <- QWA_data$cells %>%
    dplyr::anti_join(years_to_exclude, by=c('image_code','YEAR'))

  beepr::beep(sound = 1, expr = NULL)
  message("Double rings have been flagged/removed successfully!\n",
          'In total, ', nrow(v),
          ' double rings were removed.')

  return(
    setNames(
      list(df_cells_clean, df_rings_log),
      c('cells','rings')
    ))
}



# outliers
remove_outliers <- function(QWA_data){
  # negative values?
  # too high values?
  df_cells <- QWA_data$cells %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ if_else(.x < 0, NA_real_, .x)))

  # TODO: also for rings?
  df_rings <- QWA_data$rings
  return(df_cells_clean)
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

