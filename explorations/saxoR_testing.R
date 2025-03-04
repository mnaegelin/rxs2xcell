# install.packages("BlandAltmanLeh")
library(BlandAltmanLeh)
# (load QWA data)

# complete_measures -------------------------------------------------------------

# checking DH:
a <- 2*sqrt(QWA_data$cells$ASP*QWA_data$cells$LA/pi)
b <- a/QWA_data$cells$ASP
# new / replace???
DH <- sqrt((2*a^2*b^2)/(a^2+b^2)) # TODO: why not from Roxas?
sum(abs(QWA_data$cells$DH - round(DH, 2)))
# is off, since ASP and LA in QWA are rounded

# looking at unrounded
xlsx_data <- read_excel("../QWA_Arzac2024/POG/POG_02B/POG_PISY_02B_1_1_Output.xlsx", sheet = "Cells",
                        skip=2)

ax <- 2*sqrt(xlsx_data$Asp*xlsx_data$LA/pi)
bx <- ax/xlsx_data$Asp
DHx <- sqrt((2*ax^2*bx^2)/(ax^2+bx^2))
DHorg <- QWA_data$cells %>% dplyr::filter(image_code == 'POG_PISY_02_B_1_1') %>% pull(DH)
max(abs(DHorg - DHx))
# differences seem to be rounding errors only


# checking RRADDISTR.ST
tmp <- QWA_data$cells %>%
  dplyr::mutate(CWTmax1 = pmax(CWTPI, CWTBA, CWTLE, CWTRI, na.rm=TRUE),
                CWTmax2 = max(CWTPI, CWTBA, CWTLE, CWTRI, na.rm=TRUE)) # this takes overall max (would need rowwise())
sum(tmp$LA < 0)
tmp$RW.CELL <- tmp$RADDISTR / tmp$RRADDISTR * 100
tmp <- tmp %>% dplyr::left_join(QWA_data$rings %>% dplyr::select(image_code, YEAR, MRW), by=c('image_code', 'YEAR'))
tmp <- tmp %>% dplyr::mutate(RADDISTR.ST1 = RADDISTR / RW.CELL * MRW,RADDISTR.ST = RRADDISTR * MRW / 100) # the same
# RADDISTR is the distance of cell center to inner ring boundary in microns
# RRADDISTR is the relative distance of cell center to inner ring boundary (in percent)
# RADDISTR.ST is the distance of cell center to inner ring boundary in microns, standardized by MRW ??
# so a cell that is 50% of the way from the inner ring boundary would have a RADDISTR.ST of 50% of MRW
# a cell that is 99% of the way from the inner ring boundary would have a RADDISTR.ST of 99% of MRW
# EVEN IF the actual distance (RADISTR) is much larger / smaller than MRW (if the ring is wider / narrower at this point)


# to.band
bandwidth <- 30
tmp <- tmp %>% dplyr::mutate(TO.Band = bandwidth * round((RADDISTR.ST - bandwidth/2.01)/bandwidth, 0), # why 2.01?
                             TO.Band = ifelse(RRADDISTR == 0, 0, TO.Band) # no negatives for cells on inner boundary???
)
tmp1 <-  tmp %>% dplyr::filter(image_code == 'POG_PISY_02_B_1_1', YEAR == 2010)
toband <- cut(tmp1$RADDISTR.ST, breaks = seq(0, 480, by = 30), labels = seq(0, 467, by = 30), right = FALSE) # MRW is 467
toband1 <- as.numeric(as.character(toband1))
toband1 - tmp1$TO.Band
# so it uses the RADDISTR.ST to bin the cells into bands of the specified width
# (so e.g. for bandwidth 30, if RADDISTR.ST is >=30 and <60, then TO.Band is 30 asf)


# to.sector
# bins into n sectors based on RRADDISTR
# TODO: i think the conversion of cut factor to as.numeric might be problematic?
# dplyr::mutate(TO.Sector = as.numeric(cut(RRADDISTR,
#                                          breaks = seq(from = 0, to = 100, by = 100/n_sector),
#                                          labels = 1:n_sector,
#                                          include.lowest = T)) ) %>%
# SECTOR100 is this specifically for n_sector = 100


# EW LW calculations
# EWLW uses SECTOR100
# the other EWLW use to.Band10 (specifically calculated bands there with bandwith = 10)
# band/sectorwise calculations based on rolling means to determine the boundary between EW and LW


# recover other coordinates for cells ------------------------------------------
k <- 1
file_cells <- df_structure$fname_cells[k]
spatial_calibr <- as.numeric(df_meta$spatial_resolution[k]) # TODO: fix
origin_calibr <- strsplit(df_meta$origin_calibrated[k],' / ') |> lapply(as.numeric) |> unlist()
names(origin_calibr) <- c('x', 'y')

df_raw <- readr::read_delim(file_cells, delim = "\t",
                            col_types = readr::cols(.default="d", ID="c"))

df_check <- df_raw %>%
  dplyr::mutate(r_calc = sqrt(XCAL^2 + YCAL^2),
                angl_calc = 180 - atan(XCAL/YCAL) * 180/pi,
                xcal_calc = RADDIST * sin((180 - ANGLE) * pi/180),
                ycal_calc = RADDIST * cos((180 - ANGLE) * pi/180),
                r_diff = r_calc - RADDIST,
                angl_diff = angl_calc - ANGLE,
                xcal_diff = xcal_calc - XCAL,
                ycal_diff = ycal_calc - YCAL) %>%
  dplyr::select(RADDIST, ANGLE, XCAL, YCAL,
                r_calc, angl_calc, xcal_calc, ycal_calc,
                r_diff, angl_diff, xcal_diff, ycal_diff)

bland.altman.plot(df_check$RADDIST, df_check$r_calc)
bland.altman.plot(df_check$ANGLE, df_check$angl_calc)
bland.altman.plot(df_check$XCAL, df_check$xcal_calc)
bland.altman.plot(df_check$YCAL, df_check$ycal_calc)
# all differences are very small, so the calculations seem to be correct

# this looks ok (outliers if diffs are very small or 0)
(df_raw$XPIX - dplyr::lag(df_raw$XPIX)) / (df_raw$XCAL - dplyr::lag(df_raw$XCAL))
(df_raw$YPIX - dplyr::lag(df_raw$YPIX)) / (df_raw$YCAL - dplyr::lag(df_raw$YCAL))


#PIX origin in cal system (in pixels)
df_raw$XCAL*spatial_calibr - df_raw$XPIX # -2500
df_raw$YCAL*spatial_calibr - df_raw$YPIX # +150000

df_check <- df_raw %>%
  dplyr::mutate(xpix_calc = XCAL*spatial_calibr + origin_calibr['x'],
                ypix_calc = YCAL*spatial_calibr + origin_calibr['y'],
                xcal_calc = (XPIX - origin_calibr['x'])/spatial_calibr,
                ycal_calc = (YPIX - origin_calibr['y'])/spatial_calibr) %>%
  dplyr::select(XCAL, YCAL, XPIX, YPIX,
                xpix_calc, ypix_calc, xcal_calc, ycal_calc)

bland.altman.plot(df_check$XPIX, df_check$xpix_calc)
bland.altman.plot(df_check$YPIX, df_check$ypix_calc)
bland.altman.plot(df_check$XCAL, df_check$xcal_calc)
bland.altman.plot(df_check$YCAL, df_check$ycal_calc)
# so XPIX - 0 * resolution should give us XCAL - ??



# NA columns -------------------------------------------------------------------
df_cells_all <- collect_cells_data(df_structure)
df_cells_all <- df_cells_all %>% dplyr::left_join(df_structure %>% dplyr::select(image_code, woodpiece_code), by='image_code')

# count the number of NA values per column, for each woodpiece_code
df_cells_all %>%
  dplyr::group_by(woodpiece_code) %>%
  dplyr::summarize_all(~all(is.na(.)))

# TODO: when to do this check? when importing? before upload?


# check that CWTtan is generally larger than CWTrad ----------------------------
ggplot2::ggplot(df_cells_all) +
  ggplot2::geom_histogram(ggplot2::aes(x = CWTRAD, y = ..density..), fill = 'lightblue', alpha=0.5) +
  ggplot2::geom_density(ggplot2::aes(x = CWTTAN, y = ..density..), fill = 'pink', alpha=0.5)
ggplot2::ggplot(df_cells_all) +
  ggplot2::geom_histogram(ggplot2::aes(x = CWTRAD-CWTTAN, y = ..density..), fill = 'lightblue', alpha=0.5)

# doesn't really seem to be a big difference?
# TODO: ask georg again?



# ring aggregations derivable from cells? --------------------------------------
# cell measures aggregations to ring measures
helper_read_rings <- function(filename){
  df_ring <- readr::read_delim(filename, delim = "\t",
                              col_types = readr::cols(.default="d", ID="c")) %>%
    dplyr::rename_with(tolower)
  return(df_ring)
}

df_rings_all <- df_structure %>%
  dplyr::select(tree_code, woodpiece_code, slide_code, image_code, fname_rings) %>%
  dplyr::mutate(raw_data = purrr::map(fname_rings, helper_read_rings)) %>%
  tidyr::unnest(raw_data) %>%
  dplyr::select(-fname_rings)

df_cells_all <- collect_cells_data(df_structure)

df_agg <- df_cells_all %>%
  # replace negative values with NA
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) dplyr::if_else(x<0, NA, x))) %>%
  dplyr::group_by(image_code, year) %>%
  dplyr::summarise(cno = dplyr::n(),
                   cta = sum(la, na.rm = TRUE)/1000000,
                   kh = sum(kh, na.rm =TRUE),
                   minla = min(la, na.rm = TRUE),
                   maxla = max(la, na.rm = TRUE),
                   dplyr::across(c(la, cwtpi:ctsr, drad:rwd), \(x) mean(abs(x), na.rm = TRUE)),
                   # ra vs aoiar, cd, rcta,
                   .groups = 'drop') %>%
  dplyr::left_join(df_rings_all, by=c('image_code', 'year'), suffix=c('_cells', '_rings'))

sum(abs(df_agg$cno_cells - df_agg$cno_rings), na.rm = TRUE)
# so cell counts are correct

bland.altman.plot(df_agg$cta_cells, df_agg$cta_rings)
bland.altman.plot(df_agg$minla_cells, df_agg$minla_rings)
bland.altman.plot(df_agg$maxla_cells, df_agg$maxla_rings)
bland.altman.plot(df_agg$la, df_agg$mla)
bland.altman.plot(df_agg$cwtpi_cells, df_agg$cwtpi_rings)
bland.altman.plot(df_agg$cwtba_cells, df_agg$cwtba_rings)
bland.altman.plot(df_agg$cwtle_cells, df_agg$cwtle_rings)
bland.altman.plot(df_agg$cwtri_cells, df_agg$cwtri_rings)
bland.altman.plot(df_agg$cwttan_cells, df_agg$cwttan_rings)
bland.altman.plot(df_agg$cwtrad_cells, df_agg$cwtrad_rings)
bland.altman.plot(df_agg$cwtall_cells, df_agg$cwtall_rings)
bland.altman.plot(df_agg$rtsr_cells, df_agg$rtsr_rings)
bland.altman.plot(df_agg$ctsr_cells, df_agg$ctsr_rings)
bland.altman.plot(df_agg$drad_cells, df_agg$drad_rings)
bland.altman.plot(df_agg$dtan_cells, df_agg$dtan_rings)
bland.altman.plot(df_agg$tb2_cells, df_agg$tb2_rings)
bland.altman.plot(df_agg$cwa_cells, df_agg$cwa_rings)
bland.altman.plot(df_agg$rwd_cells, df_agg$rwd_rings)
# all differences are small enough to be rounding errors

bland.altman.plot(df_agg$kh_cells*10000000, df_agg$kh_rings*10000000)
bland.altman.plot(df_agg$kh_rings / (df_agg$aoiar/1000000), df_agg$ks)

bland.altman.plot(df_agg$raoiar, df_agg$aoiar/df_agg$ra)

bland.altman.plot(df_agg$cd, df_agg$cno_cells/(df_agg$aoiar))
df_agg$rcta
round(df_agg$cta_cells / df_agg$aoiar*100,3)

# TODO: there are some cases where aoiar is negative
# (probably due to weird manipulations by user in ROXAS), and cd, rcta, ks, raoiar
# are then set to 0 / not valid -> replace both with NA?
# otherwise these two agree




# image level measurements: which can be recalculated? -------------------------
output_file <- file.path(path_in, paste0('/POG/POG_02B/POG_PISY_02B_1_1_Output_Summary.txt'))

helper_read_all <- function(filename){
  filename <- sub("\\Cells.txt$", "Summary.txt", filename)
  df_img <- readr::read_delim(filename, delim = "\t",
                              col_types = readr::cols(.default="d", ID="c"))
  return(df_img)
}

df_img_all <- df_structure %>%
  dplyr::select(tree_code, woodpiece_code, slide_code, image_code, fname_cells) %>%
  dplyr::mutate(raw_data = purrr::map(fname_cells, helper_read_all)) %>%
  tidyr::unnest(raw_data) %>%
  dplyr::select(-fname_cells)

df_img_agg <- df_rings_all %>% dplyr::group_by(image_code) %>%
  dplyr::summarise(RNO = dplyr::n(),
                   CNO = sum(cno, na.rm = TRUE),
                   XA = sum(ra, na.rm = TRUE),
                   XW = sum(mrw, na.rm = TRUE)/1000,
                   CD = CNO / XA,
                   CTA = sum(cta),
                   RCTA = CTA/XA*100) %>%
dplyr::left_join(df_img_all, by=c('image_code'), suffix=c('_rings', '_img'))

bland.altman.plot(df_img_agg$RNO_rings, df_img_agg$RNO_img)
bland.altman.plot(df_img_agg$CNO_rings, df_img_agg$CNO_img) # some differences
bland.altman.plot(df_img_agg$XA_rings, df_img_agg$XA_img)
bland.altman.plot(df_img_agg$XW_rings, df_img_agg$XW_img)
bland.altman.plot(df_img_agg$CD_rings, df_img_agg$CD_img)
bland.altman.plot(df_img_agg$CNO_img/df_img_agg$XA_img, df_img_agg$CTA_img) # ??
bland.altman.plot(df_img_agg$CTA_rings, df_img_agg$CTA_img) # some differences
bland.altman.plot(df_img_agg$RCTA_rings, df_img_agg$RCTA_img) # ??
# potentially some differences arise because image values may include or exclude
# incomplete rings, or expand to outside the defined AOI to the image borders



# innermost ring check ---------------------------------------------------------
file_cells <- files$fname_cells[2]
file_rings <- files$fname_rings[2]
cells <- readr::read_delim(file_cells, delim = "\t",
                           col_types = readr::cols(.default="d", ID="c"))
rings <- readr::read_delim(file_rings, delim = "\t",
                           col_types = readr::cols(.default="d", ID="c"))
if (removeInnermost==1)   # only remove data from innermost ring if innermost ring border very likely corresponds to upper image edge (Y=0)
{
  cells.innermost <- cells[(cells$YEAR %in% min(as.numeric(rings$YEAR))),]   # extract cell data of innermost ring
  if (nrow(cells.innermost)>0)   #exclude innermost rings with no cells
  {
    cells.innermost <- cells.innermost[,c("XPIX", "YPIX", "RADDISTR", "RRADDISTR")]   # trim data
    SCAL = mean(cells.innermost$RADDISTR / cells.innermost$YPIX, na.rm=TRUE)   # automatically estimate the spatial calibration (micron/pixel)
    if (SCAL==Inf)
    {
      SCAL = mean((cells.innermost$RADDISTR+1) / (cells.innermost$YPIX+1), na.rm=TRUE)   # automatically estimate the spatial calibration (micron/pixel); +1 to avoid exceptional division by 0!
    }
    cells.innermost$RW <- 100 * cells.innermost$RADDISTR            / cells.innermost$RRADDISTR   # estimate the ring width at the position of every cell
    cells.innermost$ringposY <- cells.innermost$YPIX - ((cells.innermost$RRADDISTR / 100) * cells.innermost$RW / SCAL)   # get the position of the ring border at every cell in Y coordinates starting from the upper image edge
    # same: cells.innermost$ringposY <- cells.innermost$YPIX - cells.innermost$RADDISTR/SCAL
    cells.innermost <- cells.innermost[order(cells.innermost$XPIX),] # sort cell data from innermost ring by X coordinate
    RP <- median(cells.innermost$ringposY, na.rm=TRUE) * SCAL  # get the median Y position (microns) of the innermost ring (the image edge is Y=0)
    RP10 <- median(cells.innermost$ringposY[1:round(nrow(cells.innermost)/10)], na.rm=TRUE) * SCAL   # get the median Y position (microns) of the innermost ring for the first 10% of cells by X position
    RP90 <- median(cells.innermost$ringposY[(round(nrow(cells.innermost)/10*9)):nrow(cells.innermost)], na.rm=TRUE) * SCAL # get the median Y position (microns) of the innermost ring for the last 10% of cells by X position

    plot(cells.innermost$XPIX, cells.innermost$ringposY * SCAL)

    if (RP<=10 & abs(RP90-RP10)<=10)   # if median distance of innermost ring border from upper image edge is roughly zero microns and the Y positions of the two left and right ends of the ring border are roughly the same, assume it that the innermost ring border corresponds to the upper image edge
    {
      cells <- cells[!(cells$YEAR %in% min(as.numeric(rings$YEAR))),]
      rings <- rings[!(rings$YEAR %in% min(as.numeric(rings$YEAR))),]
    }
  }
}



