draw4 <- function(data, title) {
  
  data <- broader_location(data) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800) %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(broader_location, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position="fill", stat="identity") +
           scale_fill_manual(values = palette)+
           theme(text = element_text(size = 30), legend.title=element_blank()) + 
           scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
           scale_y_continuous(name="Percentage of publishing", labels = scales::percent))
  
}


final4 <- draw4(estc_and_bernard)

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig4_broader_location.png", sep="")
png(file=file,
    width=1200, height=700)

print(final4)

dev.off()

