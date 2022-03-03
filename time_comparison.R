# Run reused numbers before

noPureNumbersDecades <- merge(data.frame(number = c(1:635)), calc_reused_num(no_pure_vols_filtered, 1000, "filtered_clusters", "no_use", decadeStart=1720, decadeEnd=1770), by="number") %>%
  rename("pre1770" = number_hits) %>%
  merge(calc_reused_num(no_pure_vols_filtered, 1000, "filtered_clusters", "no_use",  decadeStart=1770, decadeEnd=1800), by="number") %>%
  rename("post1770" = number_hits)


noPureNumbersDecadesFilter <- noPureNumbersDecades %>%
  #filter(d1730 != 0 & d1770 != 0) %>%
  arrange(desc(pre1770)) %>%
  mutate(posPre = row_number()) %>%
  mutate(posPre = ifelse(pre1770 == 0, 0, posPre)) %>%
  mutate(top50 = ifelse((row_number() < 50), TRUE, FALSE)) %>%
  arrange(desc(post1770)) %>%
  mutate(posPost = row_number()) %>%
  mutate(posPost = ifelse(post1770 == 0, 635, posPost)) %>%
  mutate(top50 = ifelse((row_number() < 50 | top50 == TRUE), TRUE, FALSE)) %>%
  filter(top50 == TRUE)  %>%
  mutate(difference = abs(posPre - posPost))  %>%
  mutate(differenceG = lapply(difference, differenceGroup))


time_comparison_fig <- ggplot(noPureNumbersDecadesFilter) + geom_segment(aes(x=1, xend=2, y=posPre, yend=posPost, color = as.character(differenceG)))
print(time_comparison_fig)

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/time_compare.png", sep="")
png(file=file,width=1200, height=700)
print(time_comparison_fig)
dev.off()


titles <- spectatorReuseFiltered %>% distinct(title)
pre1770 <- spectatorReuseFiltered %>% filter(publication_year < 1770) %>% filter(publication_year >= 1720)
post1770 <- spectatorReuseFiltered %>% filter(publication_year < 1800) %>% filter(publication_year >= 1770)

medianDiff <- median(noPureNumbersDecadesFilter$difference)
meanDiff <- mean(noPureNumbersDecadesFilter$difference)

