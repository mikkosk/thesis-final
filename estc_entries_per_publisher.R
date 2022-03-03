draw2 <- function(data, position, title) { 
  data <- data %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade < 1800)
  
  publishers <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    group_by(publication_decade) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    dplyr::summarise(pub = n()) %>%
    #mutate(type = "pub") %>%
    filter(!is.na(publication_decade)) %>%
    ungroup()
  
  entries <- data %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(publication_decade) %>%
    dplyr::summarise(ent = n()) %>%
    #mutate(type = "ent") %>%
    filter(!is.na(publication_decade)) %>%
    ungroup()

  alt <- merge(publishers, entries, by="publication_decade")
  alt$entries_per_publisher <- alt$ent / alt$pub
  
  figure <- ggplot(data = alt, aes(publication_decade, y = entries_per_publisher)) +
    geom_point() +
    geom_smooth(fill=NA) +
    ggtitle(title) +
    theme(text = element_text(size = 20)) + 
    scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
    scale_y_continuous(name="Entries per publisher") +
    theme(legend.title=element_blank())
  
  return(figure)
}


fig2.1 <- draw6(estc_and_bernard, "stack", "ESTC and Bernard")

fig2.2 <- draw6(allData, "stack", "Whole ESTC")

final2 <- fig2.1 + fig2.2


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig2_entries_per_publisher.png", sep="")
png(file=file,
    width=1200, height=700)

print(final2)

dev.off()

