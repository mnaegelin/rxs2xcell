# TODO: i think we should first also check that all the read df contain the correct columns sets
# with the right data types (or allowing conversion) for the upload into db
# though with the read_delim, all cols except ID ar converted to numeric

# TODO: make it so we could also import analyses in polar/cal-Cartesian coordinates?

flag_problem_rings <- function(df_rings_raw, df_cells_raw,
                               max_val_year = 2500,
                               min_val_MRW = 10){

  # TODO: check input vals

  # 1. get a list of all yearrings in cells data (distinct image_code, YEAR),
  # with added cell counts per ring
  df_rings_flags <- df_cells_raw %>%
    dplyr::count(tree_code, image_code, YEAR, name = 'n_cells')

  # 2. flag which (cell) years have no corresponding row in rings output file
  # NOTE: There are sometimes cells from incomplete rings, e.g. at the edge of
  # a subsample. These cells are given a year value, and cell measurements are
  # written to the cells output files, but the corresponding row in the rings
  # output does not exist. Using a join on the raw rings df, we can filter
  # these years.
  # NOTE: There should be no years in the rings data that have no corresponding
  # rows in the cells data, but just in case, we use an inner_join
  df_rings_flags <- df_rings_flags %>%
    dplyr::left_join(df_rings_raw,
                     by = c('tree_code', 'image_code', 'YEAR')) %>%
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
  df_rings_flags <- df_rings_flags %>%
    dplyr::mutate(
      removed = rowSums(dplyr::across(dplyr::all_of(sel_flags))) > 0)

  years_to_remove <- df_rings_flags[df_rings_flags[['removed']],
                                   c('tree_code','image_code','YEAR')]
  df_cells_clean <- df_cells_raw %>%
    dplyr::anti_join(years_to_remove, by=c('tree_code','image_code','YEAR'))


  # TODO: warn, summary
  warning('In total, ', nrow(years_to_remove),
          ' years were removed from the data from the following treecodes:\n',
           paste(paste0(' ', unique(years_to_remove$tree_code)), collapse='\n'),
          '\ndue to one or more of the following flgas:\n',
           paste(paste0(' ', sel_flags), collapse='\n'),
          '\nSee fct_output$rings for more details.')
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



plot_coverage <- function(tree_code, df_rings_clean, df_meta, path_plots = './') {
  # create the data for the plot
  df_rings_clean <- clean_data$rings
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

