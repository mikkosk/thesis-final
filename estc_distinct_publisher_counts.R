draw3 <- function(data, position) { 
  data <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    distinct(actor_id, publication_decade, .keep_all = TRUE) %>%
    group_by(publication_decade) %>%
    dplyr::summarise(n=n())  %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800)
  
  figure <- ggplot(data = data, aes(publication_decade, y = n)) +
    geom_bar(position=position, stat="identity") +
    theme(text = element_text(size = 20)) + 
    scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
    scale_y_continuous(name="Publishers") +
    theme(legend.title=element_blank())
  
  return(figure)
}


final3 <- draw3(estc_and_bernard, "stack")



file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig3_distinct_publishers.png", sep="")
png(file=file,
    width=1200, height=700)

print(final3)

dev.off()

