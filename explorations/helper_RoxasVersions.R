# note: potentially first open fe drive in finder?

folders_PF <- dir('/Volumes/fe/dendro/Dendrosciences_All',
                        pattern = "PatrickFonti", recursive = FALSE, full.names = TRUE)

# finding ROXAS settings files on the drive
# this is much more efficient thatn list.files, but still takes quite a long time
# even for only two folders
# finds over 4500 files, but only 2000 have unique image names
files_sett <- vector()
for (path in folders_PF[3:4]) {
  # Construct the command
  command <- sprintf('find "%s" -type f -name "*_ROXAS_Settings.txt"', path)

  # Execute the command
  result <- system(command, intern = TRUE) # Set intern = TRUE to capture output

  files_sett <- c(files_sett, result)
}

############


example_files <- c(
  # 3.0.655
  '/Volumes/fe/dendro/Dendrosciences_All/PatrickFonti_LOTanatomy_2021_PF/S22/ROXAS/2_Roxas_corrected/LOT_S22AL1/LOT_LADE_S22AL1_1_1_ROXAS_Settings.txt',
  # 3.0.634
  '/Volumes/fe/Dendro/Dendrosciences_All/PatrickFonti_ChrissPappas_Canada_2021_PF/SBL_BEPA/SBL_BEPA_01_1_1_ROXAS_Settings.txt',
  # 3.0.620
  '/Volumes/fe/dendro/Dendrosciences_All/PatrickFonti_Chile_2021_PF/JPG/MIR04_R1_additional/MIR04_R1_1add_1_ROXAS_Settings.txt',
  # 3.0.608
  '../QWA_Arzac2024/STO/STO_20/STO_PISY_20_1_1_ROXAS_Settings.txt',
  # 3.0.590
  '/Volumes/fe/dendro/Dendrosciences_All/PatrickFonti_diverses_2022_PF/Polar Ural/JPG for Roxas/PU_L13_1_done/PU_L13-1_01_1_ROXAS_Settings.txt',
  # 3.0.575
  '/Volumes/fe/dendro/Dendrosciences_All/PatrickFonti_LOTanatomy_2021_PF/N08/ROXAS/2_Roxas_corrected/LOT_N08AL6/LOT_N08AL6_1_1_ROXAS_Settings.txt',
  # 3.0.285
  '/Volumes/fe/dendro/Dendrosciences_All/PatrickFonti_diverses_2022_PF/Polar Ural/JPG for Roxas/unknown/PU_L13_9_06_1_ROXAS_Settings.txt'
)

# check columns cells files
cellvars <- list()
for (k in 1:length(example_files)){
  file_k <- example_files[k]
  cellvars[[k]] <- strsplit(readLines(sub('ROXAS_Settings', 'Output_Cells', file_k), n = 1), "\t")[[1]]
}


# looks like they all have the same set of columns (34)
all_cellvars <- Reduce(union, cellvars)
min_cellvars <- Reduce(intersect, cellvars)

# check columns rings files
ringvars <- list()
for (k in 1:length(example_files)){
  file_k <- example_files[k]
  ringvars[[k]] <- strsplit(readLines(sub('ROXAS_Settings', 'Output_Rings', file_k), n = 1), "\t")[[1]]
}

# looks like they all have the same set of columns (37)
# plus potential AOE vars
all_ringvars <- Reduce(union, ringvars)
min_ringvars <- Reduce(intersect, ringvars)

# settings files:
extract_roxas_settings(example_files[1])
extract_roxas_settings(example_files[2])
extract_roxas_settings(example_files[3])
extract_roxas_settings(example_files[4])
extract_roxas_settings(example_files[5])
extract_roxas_settings(example_files[6])
extract_roxas_settings(example_files[7])

#looks like it works correctly. only difference is how dates are written (21.11.2018 vs 26/10/2024)
