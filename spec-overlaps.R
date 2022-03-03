count_clusters <- function(vol) {
  print("0")
  pw <- pairwise_overlaps(vol$text_start_primary,vol$text_end_primary)
  print("1")
  diag(pw) <- 0
  print("2")
  cl <- chinwhisp(pw) 
  print("3")
  vol$clus <- cl
  print("4")
  
  return (vol)
}

clean_clusters <- function(vol) {
  clus_bef_1711 <- vol %>% filter(publication_year < 1711) %>% distinct(clus)
  vol <- vol %>% filter(!(clus %in% clus_bef_1711$clus))
  
  return (vol)
}

filter_clusters <- function(vol) {
  clus_bef_1711 <- vol %>% filter(publication_year < 1711) %>% distinct(clus)
  vol <- vol %>% filter(!(clus %in% clus_bef_1711$clus)) %>% filter(!(estc_id %in% pure_only$id))
  
  return (vol)
}

