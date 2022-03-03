getHitsPerNumber <- function (volume, num, ylim) {
  # Let's clear reuses which do not start and end inside a actual number
  volume <- volume %>% filter(!is.na(startSpec)) %>% filter(!is.na(endSpec))
  
  
  vol_min_number <- volume %>% dplyr::summarise(min = min(startSpec)) %>% as.numeric
  vol_max_number <- volume %>% dplyr::summarise(max = max(endSpec)) %>% as.numeric

  print(vol_min_number)
  print(vol_max_number)
  if((is.infinite(vol_min_number) | is.infinite(vol_max_number))) {
    return (list(ggplot(), data.frame()))  
  }
  
  vol_reused_numbers <- data.frame(c(vol_min_number:vol_max_number),integer(vol_max_number - vol_min_number + 1)) %>%
    dplyr::rename(number = 1, number_hits = 2)

  for(row in 1:nrow(volume)) {
    ## Need to remove the min numer and add one to get the row. For example, if row 1 is number 22 and we want to find it . (22 - 22 + 1) = 1
    start <- volume$startSpec[row] + 1 - vol_min_number
    end <- volume$endSpec[row] + 1 - vol_min_number

    vol_reused_numbers$number_hits[start:end] <- vol_reused_numbers$number_hits[start:end] + 1
  }

  hits_fig <- ggplot(data = vol_reused_numbers, aes(x = number, y = number_hits)) +
    xlim(vol_min_number, vol_max_number) + geom_bar(stat="identity") +
    ## CAREFUL: WILL REMOVE ROWS IF THEY DO NOT FIT
    ylim(0, ylim) + 
    labs(y="Hits", x="Characters in groups of five thousand") +
    ggtitle(paste("Volume", num))
  return(list(hits_fig, vol_reused_numbers))
}

calc_reused_num <- function(vols, ylim, vols_name, group_name, decadeStart = NULL, decadeEnd = NULL) {
 figs <- list()
 allNumbersList <- list()
  
  for(i in 1:8) {
    vol <- vols[[i]]
    if(!is.null(decadeStart)) {
      vol <- vol %>% filter(publication_year >= decadeStart) %>% filter(publication_year < decadeEnd)  
    }
    result <- getHitsPerNumber(vol, i, ylim)
    newFig <- result[[1]]
    volData <- result[[2]]
    allNumbersList[[i]] <- volData
    figs[[i]] <- newFig
    
    file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/fig_reused_numbers_", group_name, "_", vols_name, "_", i,".png", sep="")
    png(file=file,width=1200, height=700)
    print(newFig)
    dev.off()
  }
  
  numbers <- do.call(rbind, allNumbersList) %>% group_by(number) %>% summarise(number_hits = sum(number_hits))
  
  hits_figs <- plot_grid(plotlist = figs)
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/fig_reused_numbers_", group_name, "_", vols_name, "_", group_name, ".png", sep="")
  png(file=file,width=1200, height=700)
  print(hits_figs)
  dev.off()
  
  return (numbers)
}

# Normal vols
under2500vols <- lapply(volumes_array, function(x) filter(x, estc_id %in% under2500_entries$estc_id))
under5000vols <- lapply(volumes_array, function(x) filter(x, estc_id %in% under5000_entries$estc_id))
distinct_title_vols <- lapply(volumes_array, function(x) filter(x, estc_id %in% distinct_reuse_entries$estc_id))
no_pure_vols <- lapply(volumes_array, function(x) filter(x,!(estc_id %in% pure_only$id)))
all_vols <- volumes_array

# Filtered clusters vols
under2500vols_filtered <- lapply(filtered_volumes_array, function(x) filter(x, estc_id %in% under2500_entries$estc_id))
under5000vols_filtered <- lapply(filtered_volumes_array, function(x) filter(x, estc_id %in% under5000_entries$estc_id))
distinct_title_vols_filtered <- lapply(filtered_volumes_array, function(x) filter(x, estc_id %in% distinct_reuse_entries$estc_id))
no_pure_vols_filtered <- lapply(filtered_volumes_array, function(x) filter(x,!(estc_id %in% pure_only$id)))
               
# Numbers from normal         
under2500Numbers <- calc_reused_num(under2500vols, 1000, "all_clusters", "under2500")
under5000Numbers <- calc_reused_num(under5000vols, 1000, "all_clusters", "under5000")
distinctTitleNumbers <- calc_reused_num(distinct_title_vols, 1000, "all_clusters", "distinct_title")
noPureNumbers <- calc_reused_num(no_pure_vols, 3000, "all_clusters", "no_pure")
allNumbers <- calc_reused_num(all_vols, 3500, "all_clusters", "all")


# Numbers from filtered
under2500NumbersFiltered <- calc_reused_num(under2500vols_filtered, 1000, "filtered_clusters", "under2500")
under5000NumbersFiltered <- calc_reused_num(under5000vols_filtered, 1000, "filtered_clusters", "under5000")
distinctTitleNumbersFiltered <- calc_reused_num(distinct_title_vols_filtered, 1000, "filtered_clusters", "distinct_title")
noPureNumbersFiltered <- calc_reused_num(no_pure_vols_filtered, 3000, "filtered_clusters", "no_pure")

combinedUnder2500 <- merge(under2500Numbers, under2500NumbersFiltered, by="number")
combinedNoPure <- merge(noPureNumbers, noPureNumbersFiltered, by="number")



