# TODO: i think we should first also check that all the read df contain the correct columns sets
# with the right data types (or allowing conversion) for the upload into db
# though with the read_delim, all cols except ID ar converted to numeric

# TODO: make it so we could also import analyses in polar/cal-Cartesian coordinates?

clean_raw_data <- function(df_raw_cells,
                           df_raw_rings,
                           max_val_year = 2500,
                           min_val_MRW = 10){

  # TODO: check input

  # 1.0. specify the columns we want, and at same time harmonizing column names of
  # different ROXAS versions
  selcols_cells <- c(
    'YEAR', 'CID', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
    'LA', 'ASP', 'MAJAX', 'KH',
    'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
    'RTSR', 'CTSR', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
    # not included cols are:
    # ID, RADDIST, ANGLE, XCAL, YCAL (superfluous)
    # NBRNO, NBRID, NBRDST (relates to groups of cells)
    # AOI (relates to areas of interest)
    # DH ??
  )
  # TODO: why not DH? is manually calcualted later
  selcols_rings <- c(
    'YEAR', 'RA', 'MRW', 'CWTTAN'
  )
  # TODO: if we really only need these -> already subset on import?
  # here, any variant name mappings can be defined using the format
  # current_name = 'old_name', current_name = 'older_name', etc.
  colname_variants <- c(
    TB2 = 'BEND',
    TB2 = 'CRI',
    LA = 'CA'
  )

  # 1.1. rename and subset columns
  # TODO: catch errors an give message
  df_cells_clean <- df_raw_cells %>%
    dplyr::rename(dplyr::any_of(colname_variants)) %>%
    dplyr::select(tree_code, image_code, all_of(selcols_cells))

  df_rings_clean <- df_raw_rings %>%
    dplyr::rename(dplyr::any_of(colname_variants)) %>%
    dplyr::select(tree_code, image_code, all_of(selcols_rings))

  # 1.2. get a list of all yearrings (distinct image_code, YEAR),
  # with added cell counts per ring
  df_rings_flags <- df_cells_clean %>%
    dplyr::count(tree_code, image_code, YEAR, name = 'n_cells')

  # 1.3. flag which (cell) years have no corresponding row in rings output file
  # There are sometimes cells from incomplete rings, e.g. at edge of subsample.
  # These cells are given a year value, and cell measurements are written to
  # the cells output files, but the corresponding row in the rings output
  # does not exist. Using a join on the raw rings df, we can filter out
  # these cells.
  # NOTE: There should be no years in the rings df that have no cells in the cells
  # output, but just in case, we use an inner_join
  df_rings_flags <- df_rings_flags %>%
    dplyr::left_join(df_rings_clean,
                     by = c('tree_code', 'image_code', 'YEAR')) %>%
    dplyr::mutate(no_rings_data = is.na(MRW))

  # 1.4. flag the rings which have invalid (i.e., in-future) or NA year values,
  # and those which have an MRW value below the threshold
  df_rings_flags <- df_rings_flags %>%
    dplyr::mutate(inv_year = YEAR > max_val_year | is.na(YEAR),
                  small_MRW = MRW < min_val_MRW)

  # 1.5. flag the rings which have no CWT estimates
  #TODO: is noCWT flag given on an year or image basis?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(image_code) %>%
    dplyr::mutate(no_CWT = all(is.na(CWTTAN))) %>%
    dplyr::ungroup()

  # 1.6. flag the duplicated rings
  # There is usually overlap between images, meaning that the same yearring can
  # be represented in two or more subsamples. Of all duplicates within a tree
  # (sample?) we then flag all but the ring with the max cell count for removal
  # TODO: across samples? or only subsamples?
  # TODO: what if the max count ring will already be removed due to other reasons?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(tree_code, YEAR) %>%
    dplyr::mutate(duplicate = dplyr::n() > 1,
                  duplicate_fewer_cells = duplicate & (n_cells < max(n_cells))) %>%
    dplyr::ungroup()

  # 1.7. flag the inner border
  # The very first yearring from the innermost sample from a tree is usually
  # too close to the pith and flagged for removal
  # TODO: always removed?
  # TODO: overall or only after other checks?
  df_rings_flags <- df_rings_flags %>%
    dplyr::group_by(tree_code) %>%
    dplyr::mutate(innermost_year = YEAR == min(YEAR)) %>%
    dplyr::ungroup()

  ####
  # Remove flagged data

  # Set based on which flags we should remove data
  # if set to TRUE, the corresponding column in df_rings_flags will be used to
  # filter out the flagged data
  flags_remove <- c(
    no_rings_data = TRUE,
    inv_year = TRUE,
    no_CWT = TRUE,
    duplicate_fewer_cells = TRUE,
    innermost_year = TRUE
  )

  # 2.1 remove the noisy data based on the selected flags
  sel_flags <- names(flags_remove[flags_remove])
  df_rings_flags <- df_rings_flags %>%
    dplyr::mutate(
      removed = rowSums(dplyr::across(dplyr::all_of(sel_flags))) > 0)

  years_to_remove <- df_rings_flags[df_rings_flags[['removed']],
                                   c('tree_code','image_code','YEAR')]
  df_cells_clean <- df_cells_clean %>%
    dplyr::anti_join(years_to_remove, by=c('tree_code','image_code','YEAR'))


  # TODO: warn, summary
  # for (flag in names(flags_remove)) {
  #   flag <- 'no_rings_data'
  #   df_rings_flags[df_rings_flags[[flag]],c('tree_code','image_code','YEAR')]
  # }
  #

  return(
    list(cells = df_cells_clean,
         rings = df_rings_flags)
         )
}






# prepare the plot
df_plot <- clean_data[[2]] %>%
  dplyr::left_join(df_meta[c('image_code', 'tree_code', 'sample','subsample')],
                   by=c('tree_code', 'image_code')) %>%
  dplyr::select(tree_code, image_code, sample, subsample, YEAR, n_cells, removed)

df_plot_summary <- df_plot %>%
  dplyr::group_by(tree_code, sample, image_code) %>%
  dplyr::summarise(min_year = min(YEAR),
                   max_year = max(YEAR),
                   .groups = "drop") %>%
  dplyr::arrange(tree_code, min_year)


unique_treecodes <- unique(df_raw_cells$tree_code)

p1 <- ggplot2::ggplot() +
  # Add the year ranges as bars
  ggplot2::geom_segment(
    data = df_plot_summary %>% dplyr::filter(tree_code == current_treecode),
    ggplot2::aes(x = min_year, xend = max_year, y = image_code),
    color = "grey50", size = 2
  )
# Add individual years with points colored by number of cells
p1 <- p1 + ggplot2::geom_point(
  data = df_plot %>% dplyr::filter(tree_code == current_treecode),
  ggplot2::aes(x = YEAR, y = image_code, color = n_cells)
  ) +
  ggplot2::scale_colour_gradient(low = "lightblue", high = "blue")
# Highlight excluded years
p1 <- p1 + ggplot2::geom_point(
  data = df_plot %>% dplyr::filter(tree_code == current_treecode,
                                   removed == TRUE),
  ggplot2::aes(
    x = YEAR,
    y = image_code
  ),  color = "red",shape=0)

p1


p1 + ggplot2::geom_point(
  data = df_plot,
  ggplot2::aes(
    x = YEAR,
    y = image_code,
    color = removed,
  ),
  size = 3
) +
ggplot2::scale_color_manual(
    values = c("FALSE" = "blue", "TRUE" = "red"),
    labels = c("Included", "Excluded")
  ) +
ggplot2::labs(
    title = "Year Coverage by Subsamples",
    x = "Year",
    y = "Sample and Subsample",
    color = "Exclude Flag"
  ) +
ggplot2::theme_minimal()


current_treecode <- unique_treecodes[1]

p <- df_plot %>% dplyr::filter(tree_code == current_treecode) %>%
  ggplot2::ggplot(., ggplot2::aes(x = YEAR, y = tree_code,
                                  colour = n_cells, label = sample,
                                  vjust = as.numeric(subsample) - 1)) +
  ggplot2::scale_color_gradient(low = "grey80", high = "grey10") +
  ggplot2::geom_text() +
  ggplot2::geom_text(data = df_plot %>% dplyr::filter(tree_code == current_treecode, removed ==TRUE),
                     ggplot2::aes(x = YEAR, y = tree_code,
                                  label = "X", vjust = as.numeric(subsample) - 1),
                     colour = "red", alpha = 0.5) +
  ggplot2::ggtitle(current_treecode) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())





# Example dataframe
df <- data.frame(
  sample_code = c("S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
  subsample = c(1, 1, 1, 2, 2, 1, 1, 2, 2),
  YEAR = c(2001, 2002, 2003, 2002, 2003, 2000, 2001, 2001, 2002),
  measurement = c(10, 20, 30, 25, 35, 15, 25, 20, 30),
  exclude_flag = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
)

# Summarize the year range for each subsample
df_summary <- df %>%
  dplyr::group_by(sample_code, subsample) %>%
  dplyr::summarize(
    min_year = min(YEAR),
    max_year = max(YEAR),
    .groups = "drop"
  )

# Create a Gantt-style plot
ggplot2::ggplot() +
  # Add the year ranges as bars
  ggplot2::geom_segment(
    data = df_summary,
    ggplot2::aes(
      x = min_year,
      xend = max_year,
      y = interaction(sample_code, subsample),
      yend = interaction(sample_code, subsample)
    ),
    color = "gray80", size = 2
  ) +
  # Add individual years with points, highlighting excluded rows
  ggplot2::geom_point(
    data = df,
    ggplot2::aes(
      x = YEAR,
      y = interaction(sample_code, subsample),
      color = exclude_flag
    ),
    size = 3
  ) +
  ggplot2::scale_color_manual(
    values = c("FALSE" = "blue", "TRUE" = "red"),
    labels = c("Included", "Excluded")
  ) +
  ggplot2::labs(
    title = "Year Coverage by Subsamples",
    x = "Year",
    y = "Sample and Subsample",
    color = "Exclude Flag"
  ) +
  ggplot2::theme_minimal()


      # rings2remove <- rings %>%
  # dplyr::filter(isInvalid == TRUE) %>% dplyr::left_join(roxas_full, by = c("plot_treecode", "YEAR", "ID"))

# for (current_treecode in unique_treecodes) {
#   p1 <- df_raw_cells %>%
#     dplyr::filter(tree_code == current_treecode) %>%
#     dplyr::group_by(image_id, YEAR, tree_code, sample, subsample) %>%
#     dplyr::summarize(n.rings = length(unique(files_rings)), n.cells = dplyr::n()) %>%
#     ggplot2::ggplot(., ggplot2::aes(x = YEAR, y = plot_treecode, colour = n.cells, label = slide, vjust = as.numeric(image) - 1)) +
#     ggplot2::scale_color_gradient(low = "grey80", high = "grey10") +
#     ggplot2::geom_text() +
#     ggplot2::geom_text(data = rings2remove %>% dplyr::filter(plot_treecode == current_treecode), ggplot2::aes(x = YEAR, y = plot_treecode, label = "X", vjust = as.numeric(image) - 1), colour = "red", alpha = 0.5) +
#     ggplot2::ggtitle(current_treecode) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
#
#   # Export the plot
#   filename <- file.path(path_out, current_treecode, "dbl_rings.png")
#   grDevices::png(
#     filename = filename,
#     width = 1500, height = 150
#   )
#   print(p1)
#   grDevices::dev.off()
#   message("The ring coverage plot for ", current_treecode, " has been exported to: ", filename)


# calculate additional measures (see rxs complete)

# identify overlapping years from different images, remove the one with fewer cells, create visualisation




# # TODO: what to do with the faulty data: remove? flag only? tell to restart? allow for dating manually?
# # saxor / homogenizer remove only the cells with no rings or na years (but should not exist anyway)
# # warn about no dates, no CWT, ask to start over with updated output files or remove from sel treecodes for future steps
## homogenzier later removes any flagged cells
# # track with flags in diagnostics table on a yearly basis



# in theory, after this, data could be uploaded
# however, we might want to add EWLW estimation based measures from Profile()
# additional Diagnostics, and manual YTE

