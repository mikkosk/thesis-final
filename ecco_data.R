path <- "C:/Users/mikko"

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(cowplot)
library(rjson)

setwd(path)


cleanVol <- function(df, removeId, isClustered) {
  ## Clean if not clustered version available
  
  if(!isClustered) {
    cleaned <- df %>% rename(estc_id = system_control_number) %>%
      filter(!grepl(removeId, id_primary)) %>%
      # Create field for which Spectator number the reuse falls on based on offset headers
      mutate(startSpec = gsub("^(\\D*)(\\d*)(.*)", "\\2", c(primaryStartHeader))) %>%
      mutate(endSpec = gsub("^(\\D*)(\\d*)(.*)", "\\2", c(primaryEndHeader)))
       
    cleaned$startSpec <- as.numeric(cleaned$startSpec)
    cleaned$endSpec <- as.numeric(cleaned$endSpec)
    cleaned <- count_clusters(cleaned)
    return (cleaned)
  }
  return (df)
}

# FOr some reason in some reuses there have been entries from wrong ESTC ID, so that is what the removeId is for.

## This allows handling multiple editions of the data and saving clustered data so you do not have to run multiple times the slow clustering algorithm.
## If starting with data provided by COMHIS, start by running it through OffsetCalc.ipynb found in this folderand save them with filename spectator.
## You can follow the folder paths in the functions or set your own. You can also save the clustered data and check the clustered variable as TRUE so it will not run the
## cluster algorithm again.

setCurrentVersion <- function(year, fixedOffset, clustered, removeId) {
  offset <- ifelse(fixedOffset, "fixedOffset_", "")
  offsetFolder <- ifelse(fixedOffset, "/fixedOffset", "")
  clustered_path <- ifelse(clustered, "_clustered", "")
  print(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator1", clustered_path, ".csv", sep = ""))
  vol1 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator1", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol2 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator2", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol3 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator3", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol4 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator4", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol5 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator5", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol6 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator6", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol7 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator7", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
  vol8 <<- cleanVol(read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator8", clustered_path, ".csv", sep = ""),stringsAsFactors = FALSE), removeId, clustered)
}
currentEdition <- "1720"
clustered <- TRUE
offset_fixed <- TRUE
setCurrentVersion(currentEdition, offset_fixed, clustered, "509")


volumes_array <- list(vol1, vol2, vol3, vol4, vol5, vol6, vol7, vol8)

## Full ESTC data
estc_data <- allData

#Id pairs between ECCO and ESTC
ecco_to_estc <- read.csv("ECCO_data/idpairs_rich_ecco_eebo_estc.csv",stringsAsFactors = FALSE)
#text_index <- fromJSON(file = "ECCO_data/text_index.json")
#indexes_vector <- names(text_index)
#indexes_df <- data.frame(indexes_vector)

# Add ESTC ids
# Add information if secondary entry is included in pure Spectator determined in metadata analysis
spectatorReuse <- bind_rows(vol1, vol2, vol3, vol4, vol4, vol5, vol5, vol6, vol7, vol8) %>%
  mutate(included_in_pure = if_else(estc_id %in% pure_only$id, TRUE, FALSE))

## Filtered reuse data where clusters before 1711 and pure editions of the Spectator have been removed.
vol1_filtered <<- filter_clusters(vol1)
vol2_filtered <<- filter_clusters(vol2)
vol3_filtered <<- filter_clusters(vol3)
vol4_filtered <<- filter_clusters(vol4)
vol5_filtered <<- filter_clusters(vol5)
vol6_filtered <<- filter_clusters(vol6)
vol7_filtered <<- filter_clusters(vol7)
vol8_filtered <<- filter_clusters(vol8)
filtered_volumes_array <- list(vol1_filtered, vol2_filtered, vol3_filtered, vol4_filtered, vol5_filtered, vol6_filtered, vol7_filtered, vol8_filtered)

spectatorReuseFiltered <- bind_rows(vol1_filtered, vol2_filtered, vol3_filtered, vol4_filtered, vol4_filtered, vol5_filtered, vol5_filtered, vol6_filtered, vol7_filtered, vol8_filtered) %>%
  mutate(included_in_pure = if_else(estc_id %in% pure_only$id, TRUE, FALSE))

vol1_cleaned <<- clean_clusters(vol1)
vol2_cleaned <<- clean_clusters(vol2)
vol3_cleaned <<- clean_clusters(vol3)
vol4_cleaned <<- clean_clusters(vol4)
vol5_cleaned <<- clean_clusters(vol5)
vol6_cleaned <<- clean_clusters(vol6)
vol7_cleaned <<- clean_clusters(vol7)
vol8_cleaned <<- clean_clusters(vol8)
cleaned_volumes_array <- list(vol1_cleaned, vol2_cleaned, vol3_cleaned, vol4_cleaned, vol5_cleaned, vol6_cleaned, vol7_cleaned, vol8_cleaned)

spectatorReuseCleaned <- bind_rows(vol1_cleaned, vol2_cleaned, vol3_cleaned, vol4_cleaned, vol4_cleaned, vol5_cleaned, vol5_cleaned, vol6_cleaned, vol7_cleaned, vol8_cleaned) %>%
  mutate(included_in_pure = if_else(estc_id %in% pure_only$id, TRUE, FALSE))