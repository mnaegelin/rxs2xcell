#' Validate the raw QWA data
#'
#' Initial checks to ensure the quality of the raw QWA data.
#' This function checks for the following issues:
#' - undated images (i.e. YEAR is NA or in the future)
#' - images without cell wall thickness estimates
#' If any images with with these issues are found, the function will stop and
#' ask the user to correct the raw data before proceeding.
#'
#' Next, the function identifies the rings with the following issues:
#' - incomplete rings
#' - missing rings
#' - duplicate rings
#'
#' Here, by incomplete rings we mean those at the inner (pith) and outer (bark)
#' borders of an images, where some cells are recognized but a MRW can NOT
#' (outer) or NOT ACCURATELY (inner) be estimated.
#' NOTE: Because ROXAS uses the outer ring boundary to estimate MRW, the
#' innermost ring generally has an MRW estimate (that is not based on the true
#' ring boundary but rather the image border), while the outermost ring has no
#' MRW value (except if it is actually complete either because it is at the
#' at the bark or because the user removed the incomplete ring manually in ROXAS).
#' Therefore, we always flag the innermost ring  per image as incomplete,
#' while the outermost ring is incomplete if and only if it has no MRW.
#' TODO: what if the user has manually removed the outermost ring in ROXAS?
#'
#' Missing rings are for years that have no discernible ring in the image, but
#' have been manually added in ROXAS during cross-dating, leading to an entry
#' in the rings data but no corresponding no entries (cells) in the cells data.
#'
#' Duplicate rings are those that are present in multiple images due to them
#' overlapping. All years which have cells in more than one image are flagged,
#' with the exception of the (complete) year with the highest number of cells
#' for each overlap, which is the one that would usually be selected for further
#' analysis when building chronologies.
#'
#' @param QWA_data
#' @param rm_innermost_ring
#' @returns validated QWA_data.
#' @export
validate_QWA_data <- function(QWA_data){
  # get a list of all annual rings in cells data (distinct image_code, YEAR),
  # with added cell counts per ring
  df_rings_log <- QWA_data$cells %>%
    dplyr::count(image_code, YEAR, name = 'n_cells')

  # combine with rings data
  df_rings_log <- df_rings_log %>%
    dplyr::full_join(QWA_data$rings,
                     by = c('image_code', 'YEAR')) %>%
    dplyr::select(tree_code, woodpiece_code, slide_code, dplyr::everything()) %>%
    dplyr::group_by(image_code) %>% # fill any missing tree/woodpiece codes by image
    tidyr::fill(tree_code, woodpiece_code, slide_code, .direction = 'downup') %>%
    dplyr::ungroup()

  # check that the data are dated (i.e., YEAR is not NA, and not in future)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(undated = is.na(YEAR) | YEAR > current_year)

  if (sum(df_rings_log$undated) > 0){
    beepr::beep(sound = 2, expr = NULL)
    stop('The following woodpieces have not been properly dated:',
         paste0(unique(df_rings_log[df_rings_log$undated, 'woodpiece_code']), collapse=', '),
         'Please ensure that all included images are dated, then restart the process.')
  }

  # check that the cell data include cell wall thickness estimates
  # (i.e., at least some cells per image need to have a nonNAN CWT value)
  # TODO: this is only relevant for conifers, so add check / warning for angiosperms
  # TODO: does it matter which CWT measure we use here?
  # TODO: could also do this on cell files directly?
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(no_CWT = all(is.na(CWTTAN))) %>%
    dplyr::ungroup()

  if (sum(df_rings_log$no_CWT) > 0){
    beepr::beep(sound = 2, expr = NULL)
    stop('The following woodpieces have images without cell wall thickness estimation:',
         paste0(unique(df_rings_log[df_rings_log$no_CWT, 'woodpiece_code']), collapse=', '),
         'Please ensure that all included images have CWT estimates, then restart the process.')
  }

  # now we know that undated and no_CWT are ALL FALSE, so we can drop them
  df_rings_log <- df_rings_log %>% dplyr::select(-undated, -no_CWT)

  # identify incomplete rings, missing rings
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(innermost_ring = YEAR == min(YEAR),
                  outermost_ring = YEAR == max(YEAR)) %>%
    dplyr::ungroup()

  df_rings_log <- df_rings_log %>%
    dplyr::mutate(incomplete_ring = (outermost_ring & is.na(MRW)) | innermost_ring,
                  missing_ring = is.na(n_cells),
                  no_MRW_other = is.na(MRW) & !(outermost_ring | innermost_ring), # TODO: check if this ever occurs and for what reason
                  missing_ringV2 = MRW < 10) # TODO: check if V2 always aligns other def

  df_rings_log <- df_rings_log %>% dplyr::select(-innermost_ring, -outermost_ring)

  # check that YEAR is consecutive sequence within each image
  # TODO: these checks are probably be overkill, since these issues should not arise
  # in ROXAS normally. BUT keep for now and see if we find any in existing datasets
  df_rings_log <- df_rings_log %>% dplyr::group_by(image_code) %>%
    dplyr::mutate(year_diff = YEAR - dplyr::lag(YEAR),
                  year_diff = tidyr::replace_na(year_diff, 1)) %>%
    dplyr::ungroup()

  #any duplicated years in the rings files?
  if (sum(df_rings_log$year_diff == 0) > 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop('The following images have duplicate years:\n',
         paste0(unique(df_rings_log[df_rings_log$year_diff == 0, 'image_code']), collapse=', '),
         '\nPlease ensure that all included samples are properly dated, then restart the process.')
  }

  # any gaps in the dating?
  if (sum(df_rings_log$year_diff > 1) > 0) {
    beepr::beep(sound = 2, expr = NULL)
    warning('The following images have gaps in the dating, missing years were added:\n',
            paste0(unique(df_rings_log[df_rings_log$year_diff > 1, 'image_code']), collapse=', '))
    # fill in missing years
    df_rings_log <- df_rings_log %>%
      dplyr::group_by(image_code) %>%
      tidyr::complete(YEAR = tidyr::full_seq(YEAR, 1),
                      fill = list(missing_ring = TRUE), explicit = FALSE) %>%
      dplyr::ungroup()
  }

  df_rings_log <- df_rings_log %>% dplyr::select(-year_diff)

  # identify duplicate rings (i.e., the same year present in two or more images
  # from the same woodpiece due to the images overlapping
  # NOTE: for years (grouped by woodpiece) where there are multiple valid rings,
  # we keep only the one with the most cells
  # TODO: avoid switching too often?
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(woodpiece_code, YEAR) %>%
    dplyr::mutate(overlap = dplyr::n() > 1) %>%
    dplyr::ungroup()

  # find the duplicate rings with the max number of cells (while excluding
  # incomplete or missing rings from possible candidates)
  # TODO: if there are no max candidates (e.g all incomplete), then all are
  # flagged as duplicates atm - is this the desired behavior?
  df_overlap <- df_rings_log %>%
    dplyr::filter(overlap) %>%
    dplyr::arrange(woodpiece_code, YEAR, desc(n_cells))

  df_overlap_max <- df_overlap %>%
    dplyr::filter(!incomplete_ring, !missing_ring)  %>%
    dplyr::group_by(woodpiece_code, YEAR) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duplicate_max_ncell = TRUE) %>%
    dplyr::select(image_code, YEAR, duplicate_max_ncell)

  # the other duplicate rings are flagged as duplicates
  df_overlap <- df_overlap %>%
    dplyr::left_join(df_overlap_max, by = c('image_code', 'YEAR')) %>%
    dplyr::mutate(duplicate_ring = ifelse(is.na(duplicate_max_ncell), TRUE, FALSE)) %>%
    dplyr::select(image_code, YEAR, duplicate_max_ncell, duplicate_ring)

  df_rings_log <- df_rings_log %>%
    dplyr::left_join(df_overlap, by = c("image_code", "YEAR")) %>%
    dplyr::mutate(duplicate_ring = ifelse(is.na(duplicate_ring), FALSE, duplicate_ring))

  # TODO: clean up, which columns do we actually need?

  # output summary
  issue_counts <-  df_rings_log %>%
    dplyr::summarise(dplyr::across(c(incomplete_ring:overlap,duplicate_ring),
                                   ~sum(.x,na.rm=TRUE)))
  message('QWA data have been validated successfully!\n',
          'The following issues were found:')
  message(paste0(capture.output(print(as.data.frame(issue_counts),
                                row.names = FALSE)), collapse='\n'))

  return(
    setNames(
      list(QWA_data$cells, df_rings_log),
      c('cells','rings'))
  )
}


#' Plot overview of annual coverage of images
#'
#' For a single tree, each image covers a span of years. This function allows
#' to visualize which annual rings are covered by which image and which rings
#' have been flagged due to which issues.
#'
#' @param woodpiece the woodpiece (core) for which the plot should be created
#' @param df_rings the dataframe containing the rings data (including flags)
#' @param show_plot should the plot be shown
#' @param save_plot should the plot be written to disk under path_out
#' @param path_out path where the plot should be saved.
#'
#' @export
plot_woodpiece_coverage <- function(woodpiece, df_rings,
                                    save_plot = FALSE, path_out = './') {
  # assert that the df_rings underwent validation and double rings checks
  beepr::beep_on_error(
    checkmate::assert_names(colnames(df_rings),
                            must.include = c('incomplete_ring', 'missing_ring',
                                             'duplicate_ring')),
    sound = 2
  )

  df_plot <- df_rings %>% dplyr::filter(woodpiece_code == woodpiece)

  # check that df_rings contains data from the provided woodpiece code
  if (nrow(df_plot) < 1){
    beepr::beep(sound=2)
    stop("The provided dataframe does not contain data from woodpiece ", woodpiece)
  }
  # TODO: additional input checks?

  # reformat data for plot
  df_plot <- df_plot %>%
    dplyr::mutate(incomplete_ring = dplyr::case_when(incomplete_ring ~ YEAR),
                  missing_ring = dplyr::case_when(missing_ring ~ YEAR),
                  duplicate_ring = dplyr::case_when(duplicate_ring ~ YEAR)) %>%
    # correct order of images
    dplyr::arrange(YEAR) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)))

  # PLOTTING
  p_cov <- df_plot %>%
    ggplot2::ggplot(ggplot2::aes(x=YEAR, y=image_code, group=image_code)) +
    # add horizontal lines for the years covered by each image
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = image_code),
      tooltip = 'Click to open image!',
      linewidth = 4, color = "grey90", lineend = 'round',
      position = ggplot2::position_nudge(y=-0.1)) +
      #onclick = "window.open(\"https://davidgohel.github.io/ggiraph/\")") +
    # add points colored by the number of cells for each year
    ggplot2::geom_point(ggplot2::aes(x=YEAR, y=image_code, fill=n_cells),
                        shape=21, size = 3, stroke = 0.1) +
    ggplot2::scale_fill_steps(low='lightskyblue', high='blue4',
                              name = "N cells") +
    # add symbols for the flagged years
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes(x=missing_ring, y=image_code, col="missing"),
      shape=0, stroke = 0.6, size=7, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=duplicate_ring, y=image_code, col="duplicate"),
      shape=1, stroke = 0.7, size = 6, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=incomplete_ring, y=image_code, col="incomplete"),
      shape=23, stroke = 0.7, size=4, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c('red4','firebrick1','darkorange2'),
                                breaks = c('incomplete', 'duplicate', 'missing'),
                                name = 'Ring flags') +
    # group images of each slide together
    ggplot2::facet_grid(rows = ggplot2::vars(slide_code),scales = 'free_y') +
    # theme and labels
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "grey25")) +
    ggplot2::labs(
      title = paste("Year coverage for woodpiece", woodpiece),
      x = "Year", y = "Image")

  # save plot
  if (save_plot){
    # save the plot to png
    plot_name <- file.path(path_out, paste0(woodpiece,'_coverage.png')) # TODO: make path safe
    # make plot size dynamically dependent on nr of rows (images) and total
    # range of years, with some hard limits to avoid extremes (all in px)
    # TODO: check if this is the best way to set the size
    image_width <- min(
      max((max(df_plot$YEAR)-min(df_plot$YEAR))*75+800, 2000),
      6000)
    image_height <- min(
      max(length(unique(df_plot$image_code))*100+200, 750),
      2000)

    ggplot2::ggsave(plot_name, p_cov, bg='white',
                    width = image_width, height = image_height, units='px')

    message(woodpiece, ': coverage plot saved to ', plot_name)
  }

  return(p_cov)
}


#' Create coverage plots for all trees
#'
#' Function to create coverage plots for all unique treecodes in the provided
#' rings df (usually QWA_data$rings) in a for loop. Plots can be shown and/or
#' saved to a specified path.
#'
#' @param df_rings rings dataframe
create_coverage_plots <- function(df_rings,
                                  save_plot = TRUE, path_out = './'){
  woodpiece_codes <- unique(df_rings$woodpiece_code)
  for (wp in woodpiece_codes){
    plot_woodpiece_coverage(wp, df_rings,
                            save_plot = save_plot, path_out = path_out)
  }
}



# Manual flags
# TODO: finalize
add_user_flags <- function(QWA_data, years_to_flag){
  df_rings_log <- QWA_data$rings %>%
    dplyr::left_join(years_to_flag %>% dplyr::mutate(manual_flag = TRUE),
                    by = c('image_code','YEAR')) %>%
    dplyr::mutate(manual_flag = ifelse(is.na(manual_flag), FALSE, manual_flag))

  return(
    setNames(
      list(QWA_data$cells, df_rings_log),
      c('cells','rings')
    ))
}



# handle outliers
# TODO: finalize
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
# TODO: finalize
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

