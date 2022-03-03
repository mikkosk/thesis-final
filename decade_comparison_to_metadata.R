#Decades from ESTC data
estc_decades <- estc_data[c("id", "publication_decade")] %>% 
  rename(estc_id = id, group = publication_decade) %>% 
  distinct(estc_id, .keep_all = TRUE)

#Reuse by decades
values_by_decades <- spectatorReuseFiltered %>% dplyr::left_join(distinct(estc_decades, estc_id, .keep_all = TRUE)) %>% 
  filter(group < 1800) %>%
  filter(group > 1700) %>%
  group_by(group) %>% 
  dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n(), distinct=n_distinct(estc_id))

#Full ESTC data and Spectator ESTC entries by decade
distinct_spectator_count_by_decade <- spectator %>% distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(n = n())
distinct_estc_count_by_decade <- estc_data %>% filter(publication_decade > 1660) %>% filter(publication_decade < 1810) %>% distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(n = n())

#Figure 5 of the thesis
colors <- c("Reuse hits" = "#FFCE03", "Reuse characters" = "#FD9A01", "Distinct ESTC entries reusing the Spectator" = "#F00505","Distinct Spectator entries by metadata" = "#187DE9", "Distinct ESTC entries" = "#00308F")
decade_comparison_fig <- ggplot() + 
  geom_line(data = values_by_decades %>% mutate(n = n / first(n) * 100), size=2, aes(x = group, y = n, color="Reuse hits")) +
  geom_line(data = values_by_decades %>% mutate(sum_length = sum_length / first(sum_length) * 100), size=2, aes(x = group, y = sum_length, color="Reuse characters")) +
  geom_line(data = values_by_decades %>% mutate(distinct = distinct / first(distinct) * 100), size=2, aes(x = group, y = distinct, color="Distinct ESTC entries reusing the Spectator")) +
  geom_line(data = distinct_spectator_count_by_decade %>% mutate(n = n / first(n) * 100), size=2, aes(x = publication_decade, y = n, color="Distinct Spectator entries by metadata")) +
  geom_line(data = distinct_estc_count_by_decade %>% mutate(n = n / first(n) * 100), size=2, aes(x = publication_decade, y = n, color="Distinct ESTC entries")) +
  scale_colour_manual(values = colors) +
  ggtitle("Indexed progress during 18th century") +
  theme(text = element_text(size = 30), legend.title=element_blank()) + 
  scale_x_continuous(name="Decade", limits=c(1710, 1790), breaks=seq(1710,1790,20)) +
  scale_y_continuous(name="Indexed value (base 100)")

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig5_decade_comparison.png", sep="")
png(file=file,
    width=1200, height=700)
print(decade_comparison_fig)
dev.off()

#Figure with no indexing
hit_fig <- ggplot() + geom_bar(data = values_by_decades, aes(x = group, y = n), stat = "identity") + ggtitle("Hits amount / decade")
sum_fig <- ggplot() + geom_bar(data = values_by_decades, aes(x = group, y = sum_length), stat = "identity") + ggtitle("Characters reused / decade")
distinct_fig <- ggplot() + geom_bar(data = values_by_decades, aes(x = group, y = distinct), stat = "identity") + ggtitle("Reused distinct ESTC / decade")
median_fig <- ggplot() + geom_bar(data = values_by_decades, aes(x = group, y = median_length), stat = "identity") + ggtitle("Median length / decade")
mean_fig <- ggplot() + geom_bar(data = values_by_decades, aes(x = group, y = mean), stat = "identity") + ggtitle("Mean length / decade")

decade_comparison_no_ind_fig <- hit_fig + sum_fig + distinct_fig + median_fig + mean_fig

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/decade_comparison_no_index.png", sep="")
png(file=file,
    width=1200, height=700)
print(decade_comparison_fig)
print(decade_comparison_no_ind_fig)
dev.off()
