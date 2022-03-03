# Run reused numbers before

pureShortComp<- merge(data.frame(number = c(1:635)), noPureNumbersFiltered) %>%
  rename("noPure" = number_hits) %>%
  merge(under2500NumbersFiltered) %>%
  rename("under2500" = number_hits)


pureShortCompFilter <- pureShortComp %>%
  arrange(desc(noPure)) %>%
  mutate(posNo = row_number()) %>%
  mutate(posNo = ifelse(noPure == 0, 0, posNo)) %>%
  mutate(top50 = ifelse((row_number() < 50), TRUE, FALSE)) %>%
  arrange(desc(under2500)) %>%
  mutate(posUnd = row_number()) %>%
  mutate(posUnd = ifelse(under2500 == 0, 635, posUnd)) %>%
  mutate(top50 = ifelse((row_number() < 50 | top50 == TRUE), TRUE, FALSE)) %>%
  filter(top50 == TRUE)  %>%
  mutate(difference = abs(posNo - posUnd)) %>%
  mutate(differenceG = lapply(difference, differenceGroup))

compFig <- ggplot(pureShortCompFilter) + geom_segment(aes(x="All data", xend="Under 2500", y=posNo, yend=posUnd, colour=(as.character(differenceG))))

print(compFig)


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/short_v_no_pure.png", sep="")
png(file=file,width=1200, height=700)
print(compFig)
dev.off()

