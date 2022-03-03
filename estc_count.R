# Step 1
js <- spectator %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Plain ESTC")
eon <- estc_only %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Only ESTC")
eabn <- estc_and_bernard %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "ESTC and Bernard")
pon <- pure_only %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Pure Spectator")
tat <- tatler %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Tatler")
all <- allData %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Whole ESTC")

# Step 2
draw1 <- function(data, title) {
  data <- data %>% filter(publication_decade > 1700) %>% group_by(publication_decade, set) %>% dplyr::summarise(n=n()) %>% filter(publication_decade < 1800)
  return(ggplot(data=data, aes(publication_decade, y = n, color=set)) +
           geom_smooth(fill=NA) +
           geom_point() + ggtitle(title) +
           theme(text = element_text(size = 20)) + 
           scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
           scale_y_continuous(name="Entries") +
           theme(legend.position = "none")
         )
}

# Step 3


fig1.1 <- draw1(eabn, "ESTC & Bernard")

fig1.2 <- draw1(pon, "Pure Spectator only")

fig1.3 <- draw1(all, "Whole ESTC")


final1 <- fig1.1 + fig1.2 + fig1.3

# Step 4

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig1_estc_counts.png", sep="")
png(file=file,
    width=1200, height=700)

print(final1)

dev.off()

