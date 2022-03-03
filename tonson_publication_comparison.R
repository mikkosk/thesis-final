milton_entries <- allData %>% group_by(id) %>% filter(any(actor_id == "17226855")) %>% ungroup()
dryden_entries <- allData %>% group_by(id) %>% filter(any(actor_id == "68937979")) %>% ungroup()
shakespeare_entries <- allData %>% group_by(id) %>% filter(any(actor_id == "96994048")) %>% ungroup()
addison_entries <- allData %>% group_by(id) %>% filter(any(actor_id == "7413288")) %>% ungroup()
spec_entries <- pure_only

all_entries <- allData %>% distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(n = n()) %>% drop_na()
tonson_entries <- allData %>% filter(actor_id %in%tonson_actor_ids$actor_id) %>% distinct(id, .keep_all = TRUE) %>% group_by(publication_decade)%>% 
  dplyr::summarise(n = n()) %>% na.omit()

getTonsonPercentages <- function(data, group) {
  final <- data %>% group_by(id) %>% mutate(tonson = any(actor_id %in% tonson_actor_ids$actor_id)) %>%
    distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(percentage = sum(tonson)/n()) %>%
    mutate(group = group) %>% na.omit()
}

spec_tonson <- getTonsonPercentages(spec_entries, "Spectator")
shakespeare_tonson <- getTonsonPercentages(shakespeare_entries, "Shakespeare")
addison_tonson <- getTonsonPercentages(addison_entries, "Addison")
milton_tonson <- getTonsonPercentages(milton_entries, "Milton")
dryden_tonson <- getTonsonPercentages(dryden_entries, "Dryden")

fullBarData <- spec_tonson %>% rbind(shakespeare_tonson) %>% rbind(addison_tonson) ## %>% rbind(dryden_tonson) %>% rbind(milton_tonson)

colors <- c("Spectator" = "#1E90FF", "Shakespeare" = "#999999", "Addison" = "#333333", "Dryden" = "#555555", "Milton" = "#777777")
linetypes <- c("All entries" = "longdash", "Tonson entries" = "solid")

tonson_copys_fig <- ggplot() + 
  geom_col(data = fullBarData, position = "dodge", aes(x = publication_decade, y = percentage, fill = group)) +
  theme(text = element_text(size = 20)) + 
  scale_x_continuous(name="Decade", limits=c(1701, 1770), breaks=seq(1710, 1760, 10)) +
  scale_y_continuous(name="Percentage of entries by Tonsons", labels = scales::percent) +
  scale_fill_manual(values = colors) +
  theme(legend.title=element_blank())

print(tonson_copys_fig)


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig9_tonson_copyright_dev.png", sep="")
png(file=file,width=1200, height=700)
print(tonson_copys_fig)
dev.off()
