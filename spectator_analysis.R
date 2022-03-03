estc_only <- spectator %>% filter(is.na(bernard))
estc_and_bernard <- spectator %>% filter(!is.na(bernard))
pure_only <- spectator %>% filter(pure == TRUE)

distinct_estc <- distinct(estc_only, id, .keep_all = TRUE)
distinct_all <- spectator %>% distinct(id, .keep_all = TRUE)

namedColors <- as.character(palette[1:18])
place <- distinct(spectator, publication_place_752) %>% cbind(namedColors)
print(namedColors)

createLocationPalatte <- function(list){
  list <- distinct(list,publication_place_752)$publication_place_752
  list <- sort(list)
  newPalette <- c()
  for (l in list) {
    newColor <- place[which(place$publication_place_752 == l), ]$namedColors
    newPalette <- c(newPalette, as.character(newColor[1]))
  }
  print(newPalette)
  return(newPalette)
}

broader_location <- function(df){
  final <- df
  final$broader_location <- final$country_752
  final <- final %>% 
    mutate(broader_location = ifelse(country_752 %in% c("England", "Scotland", "Ireland"), country_752 , "Other or unknown")) %>%
    mutate(broader_location = ifelse(country_752 %in% c("England"), "Provincal", broader_location)) %>%
    mutate(broader_location = ifelse(publication_place_752 == "London", "London", broader_location))
  return(final)
}

