# Run reused numbers before

numbersBib <- merge(data.frame(number = c(1:635)), noPureNumbers, by="number") %>%
  rename("unfil" = number_hits) %>%
  merge(noPureNumbersFiltered, by="number") %>%
  rename("fil" = number_hits)

BibNumbers <- c(177, 68, 219, 565, 336, 624, 439, 464, 519)

differenceGroup <- function(number) {
  if(number < 50) {
    return ("Under50")
  }
  if(number < 200) {
    return ("Under200")
  }
  return ("Over200")

}

numbersBibFilter <- numbersBib %>%
  #filter(d1730 != 0 & d1770 != 0) %>%
  arrange(desc(unfil)) %>%
  mutate(posUnfil = row_number()) %>%
  mutate(posUnfil = ifelse(unfil == 0, 0, posUnfil)) %>%
  arrange(desc(fil)) %>%
  mutate(posFil = row_number()) %>%
  mutate(posFil = ifelse(fil == 0, 635, posFil)) %>%
  mutate(difference = abs(posUnfil - posFil)) %>%
  filter(number %in% BibNumbers) %>%
  mutate(differenceG = lapply(difference, differenceGroup))

bible_group_fig <- ggplot(numbersBibFilter, aes(colour=as.character(differenceG))) + geom_segment(aes(x=1, xend=2, y=posUnfil, yend=posFil))
print(bible_group_fig)


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/bible_filter_compare.png", sep="")
png(file=file,width=1200, height=700)
print(bible_group_fig)
dev.off()

titles <- spectatorReuseFiltered %>% distinct(title)
unfil <- spectatorReuseFiltered %>% filter(publication_year < 1770) %>% filter(publication_year >= 1720)
fil <- spectatorReuseFiltered %>% filter(publication_year < 1800) %>% filter(publication_year >= 1770)

