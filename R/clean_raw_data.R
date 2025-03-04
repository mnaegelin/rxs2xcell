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
    dplyr::group_by(image_code, year) %>%
    dplyr::summarise(cno = dplyr::n(),
                     no_cwt = all(is.na(cwttan)),
                     .groups = 'drop')

  # combine with rings data
  df_rings_log <- df_rings_log %>%
    dplyr::full_join(QWA_data$rings,
                     by = c('image_code', 'year')) %>%
    dplyr::select(tree_code, woodpiece_code, slide_code, dplyr::everything()) %>%
    dplyr::group_by(image_code) %>% # fill any missing tree/woodpiece codes by image
    tidyr::fill(tree_code, woodpiece_code, slide_code, .direction = 'downup') %>%
    dplyr::ungroup()

  # check that the data are dated (i.e., YEAR is not NA, and not in future)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(undated = is.na(year) | year > current_year)

  if (sum(df_rings_log$undated) > 0){
    beepr::beep(sound = 2, expr = NULL)
    stop('The following woodpieces have not been properly dated:\n',
         paste0(unique(df_rings_log[df_rings_log$undated, 'woodpiece_code']), collapse=', '),
         '\nPlease ensure that all included images are dated, then restart the process.')
  }

  # check that the cell data include cell wall thickness estimates
  # (i.e., at least some cells per image need to have a nonNAN CWT value)
  # TODO: this is only relevant for conifers, so add check / warning for angiosperms
  # TODO: does it matter which CWT measure we use here?
  # TODO: could also do this on cell files directly?
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(no_cwt = all(no_cwt)) %>%
    dplyr::ungroup()

  if (sum(df_rings_log$no_cwt) > 0){
    beepr::beep(sound = 2, expr = NULL)
    warning('The following woodpieces have images without cell wall thickness estimation: \n',
         paste0(unique(df_rings_log[df_rings_log$no_cwt, 'woodpiece_code']), collapse=', '),
         '\nIf the data is from conifers, please ensure that all included images have CWT estimates.')
  }

  # now we know that undated and no_cwt are ALL FALSE, so we can drop them
  df_rings_log <- df_rings_log %>% dplyr::select(-undated, -no_cwt)

  # identify incomplete rings, missing rings
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(innermost_ring = year == min(year),
                  outermost_ring = year == max(year)) %>%
    dplyr::ungroup()

  df_rings_log <- df_rings_log %>%
    dplyr::mutate(incomplete_ring = (outermost_ring & is.na(mrw)) | innermost_ring,
                  missing_ring = is.na(cno),
                  no_MRW_other = is.na(mrw) & !(outermost_ring | innermost_ring), # TODO: check if this ever occurs and for what reason
                  missing_ringV2 = mrw < 10) # TODO: check if V2 always aligns other def

  # TODO: set raddist and rraddist to NA for incomplete rings (are only already NA for outermost incomplete rings)
  # TODO: additional check:




  df_rings_log <- df_rings_log %>% dplyr::select(-innermost_ring, -outermost_ring)

  # check that YEAR is consecutive sequence within each image
  # TODO: these checks are probably be overkill, since these issues should not arise
  # in ROXAS normally. BUT keep for now and see if we find any in existing datasets
  df_rings_log <- df_rings_log %>% dplyr::group_by(image_code) %>%
    dplyr::mutate(year_diff = year - dplyr::lag(year),
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
      tidyr::complete(year = tidyr::full_seq(year, 1),
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
    dplyr::group_by(woodpiece_code, year) %>%
    dplyr::mutate(overlap = dplyr::n() > 1) %>%
    dplyr::ungroup()

  # find the duplicate rings with the max number of cells (while excluding
  # incomplete or missing rings from possible candidates)
  # TODO: if there are no max candidates (e.g all incomplete), then all are
  # flagged as duplicates atm - is this the desired behavior?
  df_overlap <- df_rings_log %>%
    dplyr::filter(overlap) %>%
    dplyr::arrange(woodpiece_code, year, desc(cno))

  df_overlap_max <- df_overlap %>%
    dplyr::filter(!incomplete_ring, !missing_ring)  %>%
    dplyr::group_by(woodpiece_code, year) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duplicate_max_ncell = TRUE) %>%
    dplyr::select(image_code, year, duplicate_max_ncell)

  # the other duplicate rings are flagged as duplicates
  df_overlap <- df_overlap %>%
    dplyr::left_join(df_overlap_max, by = c('image_code', 'year')) %>%
    dplyr::mutate(duplicate_ring = ifelse(is.na(duplicate_max_ncell), TRUE, FALSE)) %>%
    dplyr::select(image_code, year, duplicate_max_ncell, duplicate_ring)

  df_rings_log <- df_rings_log %>%
    dplyr::left_join(df_overlap, by = c("image_code", "year")) %>%
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
    stats::setNames(
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
#'
#' @export
plot_woodpiece_coverage <- function(woodpiece, df_rings) {
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
    dplyr::mutate(incomplete_ring = dplyr::case_when(incomplete_ring ~ year),
                  missing_ring = dplyr::case_when(missing_ring ~ year),
                  duplicate_ring = dplyr::case_when(duplicate_ring ~ year)) %>%
    # correct order of images
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)),
                  slide_code = factor(slide_code, levels = rev(unique(slide_code))))

  # PLOTTING
  p_cov <- df_plot %>%
    ggplot2::ggplot(ggplot2::aes(x=year, y=image_code, group=image_code)) +
    # add horizontal lines for the years covered by each image
    # ggplot2::geom_line(linewidth = 4, color = "grey90", lineend = 'round',
    #                    position = ggplot2::position_nudge(y=-0.1)) +
    # with interactive functionality for use in shiny
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = image_code),
      tooltip = 'Click to open image!',
      linewidth = 4, color = "grey90", lineend = 'round',
      position = ggplot2::position_nudge(y=-0.1)) +
      #onclick = "window.open(\"https://davidgohel.github.io/ggiraph/\")") +
    # add points colored by the number of cells for each year
    ggplot2::geom_point(ggplot2::aes(x=year, y=image_code, fill=cno),
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

  # make plot size dynamically dependent on nr of rows (images) and total
  # range of years, with some hard limits to avoid extremes (all in px)
  # TODO: check if this is the best way to set the size
  image_width <- min(
    max((max(df_plot$year)-min(df_plot$year))*75+800, 2000),
    6000)
  image_height <- min(
    max(length(unique(df_plot$image_code))*100+200, 750),
    2000)

  return(
    list(p = p_cov,
         image_width = image_width,
         image_height = image_height)
  )
}


#' Create coverage plots for all trees
#'
#' Function to create coverage plots for all unique treecodes in the provided
#' rings df (usually QWA_data$rings) in a for loop. Plots can be shown and/or
#' saved to a specified path.
#'
#' @param df_rings rings dataframe
#' @param save_plot should the plot be written to disk under path_out
#' @param path_out path where the plot should be saved.
#' @export
create_coverage_plots <- function(df_rings,
                                  save_plot = TRUE, path_out = './'){
  woodpiece_codes <- unique(df_rings$woodpiece_code)
  for (wp in woodpiece_codes){

    covplot_list <- plot_woodpiece_coverage(wp, df_rings)

    # save the plot to png
    plot_name <- file.path(path_out, paste0(wp,'_coverage.png')) # TODO: make path safe
    ggplot2::ggsave(plot_name, covplot_list$p, bg='white',
                    width = covplot_list$image_width,
                    height = covplot_list$image_height, units='px')
    message(woodpiece, ': coverage plot saved to ', plot_name)
  }
}



# Manual flags
# TODO: finalize
add_user_flags <- function(QWA_data, years_to_flag){
  df_rings_log <- QWA_data$rings %>%
    dplyr::left_join(years_to_flag %>% dplyr::mutate(manual_flag = TRUE),
                    by = c('image_code','year')) %>%
    dplyr::mutate(manual_flag = ifelse(is.na(manual_flag), FALSE, manual_flag))

  return(
    stats::setNames(
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

  return(
    stats::setNames(
      list(df_cells, df_rings),
      c('cells','rings')
    ))
}



# calculate additional measures (see rxs complete)
# TODO: finalize
complete_cell_measures <- function(df_cells_clean){

  df_cells_compl <- df_cells_clean %>%
    # need MRW for some calculations
    dplyr::left_join(QWA_data$rings %>%
                       dplyr::select(image_code, YEAR, MRW), by=c('image_code', 'YEAR')) %>%

    dplyr::mutate(

      # helpers to find CWA, RWD outliers (CWAmax is 2 * the area that would be achieved by the max valid CWT of the four walls)
      CWTmax = pmax(CWTPI, CWTBA, CWTLE, CWTRI, na.rm=TRUE), # automatically neglects the negative outliers
      LR = sqrt(LA / pi),
      CWAmax = 2 * (pi * (LR + CWTmax)^2 - LA), # why the factor 2?
      # CWA, RWD: replace too high values with NA (so some negative outliers might be sign flipped rather than removed)
      # NOTE: CWA from Roxas can have negative values to indicate outliers
      CWA = ifelse(abs(CWA) > CWAmax, NA, CWA),
      RWD = ifelse(abs(CWA) > CWAmax, NA, RWD),

      # new:
      TCA = ifelse(CWA > 0, LA + CWA, ifelse(is.na(CWA), NA, -(LA-CWA))), # if CWA negative, also make it negative? LA seems to be always > 0

      # DH recalculation:
      # mostly agrees with ROXAS output, except for rounding errors. is it not available for earlier versions?
      # helper:
      a = 2*sqrt(ASP*LA/pi),
      b = a/ASP,
      # new / replace???
      DH = sqrt((2*a^2*b^2)/(a^2+b^2)), # TODO: why not from Roxas?

      # new:
      RWD2 = CWTRAD/DRAD,

      # Add CWT-based density:
      # assume a circular lumen area with homogenous cell wall thickness around it;
      # for latewood-like cells, take overall average CWT,
      # for earlywood-like cells, only consider CWTTAN, which avoids pit artefacts
      # helper:
      WA = ifelse(RTSR < 1, (LR + CWTTAN)^2 * pi - LA, (LR + CWTALL)^2 * pi - LA),
      # new:
      DCWT = WA / (LA + WA),

      # get ring width at tangential position of each cell ??
      # new:
      RADDISTR.ST = RRADDISTR * MRW / 100,

      # Add mean CWT: mean of radial and tangential CWT if Mork index latewood-like,
      # in earlywood-like cells take CWTTAN
      # new:
      CWTALL.ADJ = ifelse(RTSR < 1,  CWTTAN,  CWTALL),
      CDRAD = DRAD + 2*CWTTAN,
      CDTAN = DTAN + 2*CWTRAD,
      CDRATIO = CDRAD/CDTAN

    ) %>%





    # remove unwanted parameters
    dplyr::select(-MRW, -MAX.CWT, -LR, -MAX.CWA, -a, -b, -WA, -RW.CELL)



  return(df_cells_compl)
}





# TODO: add EW/LW estimation (standard implementation)

# Add SECTOR100
# TODO: does this work with the duplicates / flags?
# TODO: should be as.numeric(as.character()) ???
# dplyr::group_by(woodpiece_code, YEAR) %>%
#   dplyr::mutate(
#     # TO.SECTORS = as.numeric(cut(RADDISTR.ST, b=seq(from= 0, to=100, by= 100/(NSECTOR[1])), labels=1:(NSECTOR[1]))),
#     SECTOR100 = as.numeric(cut(RRADDISTR,
#                                b = seq(from=0, to=100, by= 1),
#                                labels = 1:100,
#                                include.lowest = T)))  %>%
#   dplyr::ungroup() %>%
#   # round for data just above the RRADDISTR of class 100 otherwise just leave NA
#   dplyr::mutate(SECTOR100 = if_else(RRADDISTR > 100 & RRADDISTR <= 101, 100, SECTOR100)) %>%

# tbl_YEAR <- tbl_rxs_hmgz %>%
#   dplyr::filter(!is.na(RTSR)) %>%  # remove cells that do not have a measured CWT
#   dplyr::group_by(plot_treecode, YEAR, slide, SECTOR100) %>%
#   dplyr::summarise(RTSR.MEAN = mean(RTSR, na.rm = TRUE),
#                    MRW = mean(MRW, na.rm = TRUE), .groups = 'drop') %>%
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::mutate(ROLLMEAN = zoo::rollmean(RTSR.MEAN, 9, fill = c(NA, NA, 10)),
#                 TO.EWLW = ifelse(SECTOR100 <= max_na_inf(SECTOR100[ROLLMEAN <= mork]), "EW", "LW")) %>%
#   # filter(plot_treecode=="YAM_2756", YEAR ==1940) %>%
#   # ggplot(aes(x=SECTOR100,y=ROLLMEAN, group = paste0(plot_treecode,YEAR), colour= TO.EWLW)) + geom_line()
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::summarise(MRW = mean(MRW),
#                    EWW = ifelse(any(TO.EWLW == "EW", na.rm = TRUE),
#                                 max_na_inf(SECTOR100[TO.EWLW == "EW"] * MRW/100),
#                                 0),
#                    LWW = MRW - EWW, .groups = 'drop')
#
# tbl_out <- tbl_rxs_hmgz %>%
#   dplyr::left_join(tbl_YEAR %>% select(-MRW),  by = c("plot_treecode", "YEAR", "slide")) %>%
#   dplyr::mutate(TO.EWLW = ifelse(RADDISTR.ST >= EWW, "LW", "EW"))




# TODO: what to do with the faulty data: remove? flag only? tell to restart? allow for dating manually?
# saxor / homogenizer remove only the cells with no rings or na years (but should not exist anyway)
# warn about no dates, no CWT, ask to start over with updated output files or remove from sel treecodes for future steps
# homogenzier later removes any flagged cells
# track with flags in diagnostics table on a yearly basis



# in theory, after this, data could be uploaded
# however, we might want to add EWLW estimation based measures from Profile()
# additional Diagnostics, and manual YTE

