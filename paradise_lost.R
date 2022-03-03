## Cluster data if not saved in clustered form
## ECCO data for Paradise Lost needed
path <- "C:/Users/mikko"
# ecco_pl_orig <- read.csv(paste("ECCO_data/", "pl1.csv", sep = ""),stringsAsFactors = FALSE)
# additional_data <- ecco_to_estc[, c("estc_id", "document_id")]
# ecco_pl <- ecco_pl_orig %>% mutate(document_id = str_pad(id_secondary, 10, side = 'left', pad = "0")) %>% merge(additional_data, by="document_id", all.x = TRUE)
# paradise_clustered <- count_clusters(ecco_pl) 
# 
#write.csv(paradise_clustered, "ECCO_data/pl1_clustered.csv")

paradise_clustered <- read.csv(paste("ECCO_data/", "pl1_clustered.csv", sep = ""),stringsAsFactors = FALSE)

clusters_bef_1667 <- paradise_clustered %>% filter(publication_year < 1667) %>% distinct(clus, .keep_all = TRUE)
paradise_filtered <- paradise_clustered %>% filter(!(clus %in% clusters_bef_1667$clus))

## DECADE COMPARISON. RUN decade_comparison_to_metadata.R before
pd_by_decades <- paradise_filtered %>% dplyr::left_join(distinct(estc_decades, estc_id, .keep_all = TRUE)) %>% 
  filter(group < 1800) %>%
  filter(group > 1660) %>%
  group_by(group) %>% 
  dplyr::summarise(sum_length = sum(length), n=n(), distinct=n_distinct(estc_id))


colorsPL <- c("Reuse hits Spectator" = "orange", "Reuse characters Spectator" = "red", "Reuse hits PL" = "darkred","Reuse characters PL" = "blue", "Distinct ESTC entries" = "green")
pd_comparison_fig <- ggplot() + 
  geom_line(data = values_by_decades %>% mutate(n = n / first(n) * 100), aes(x = group, y = n, color="Reuse hits Spectator")) +
  geom_line(data = values_by_decades %>% mutate(sum_length = sum_length / first(sum_length) * 100), aes(x = group, y = sum_length, color="Reuse characters Spectator")) +
  geom_line(data = pd_by_decades %>% mutate(n = n / first(n) * 100), aes(x = group, y = n, color="Reuse hits PL")) +
  geom_line(data = pd_by_decades %>% mutate(sum_length = sum_length / first(sum_length) * 100), aes(x = group, y = sum_length, color="Reuse characters PL")) +
  geom_line(data = distinct_estc_count_by_decade %>% mutate(n = n / first(n) * 100), aes(x = publication_decade, y = n, color="Distinct ESTC entries")) +
  ylim(0, 2000) +
  xlim(1670, 1790) +
  scale_colour_manual(values = colorsPL) +
  ggtitle("PL indexed progress during 18th century compared to ESTC and Spectator")


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/paradise_lost_indexed.png", sep="")
png(file=file,width=1200, height=700)
print(pd_comparison_fig)
dev.off()

## ProminentEntries 
prominent_pl <- paradise_filtered %>% group_by(estc_id, title) %>% dplyr::summarise(n = n(), sum = sum(length))

## Clusters used by Spectator
pl_spec_uses <- paradise_filtered %>% filter(estc_id %in% pure_only$id)
paradise_clus_comp <- paradise_filtered %>% mutate(spec_cluster = clus %in% pl_spec_uses$clus) %>%
  mutate(publication_decade = publication_year - (publication_year %% 10)) %>%
  filter(!(estc_id %in% pure_only$id)) %>%
  filter(!grepl("addison | milton", tolower(author))) %>%
  filter(!grepl("paradise lost", tolower(title)))

prominent_pl_clus <- paradise_clus_comp %>% group_by(estc_id, title) %>% dplyr::summarise(n = n(), sum = sum(length))

pl_cluster_decades <- paradise_clus_comp %>% group_by(publication_decade) %>%
  dplyr::summarise(n = n(), sum = sum(length), spec_sum = sum(length[spec_cluster == TRUE]), spec_n = sum(spec_cluster == TRUE)) %>%
  mutate(percentage_sum = spec_sum / sum * 100) %>%
  mutate(percentage_n = spec_n / n * 100)  %>%
  filter(publication_decade < 1800) %>% 
  filter(publication_decade > 1660)

pl_entries_with_cluster_decades <- paradise_clus_comp %>%
  group_by(estc_id) %>%
  mutate(uses_clusters = any(spec_cluster == TRUE)) %>%
  ungroup() %>%
  group_by(estc_id, uses_clusters, publication_decade) %>%
  dplyr::summarise(length = sum(length)) %>%
  group_by(publication_decade) %>%
  dplyr::summarise(n = n(), cluster_n = sum(uses_clusters == TRUE), sum = sum(length), cluster_sum = sum(length[uses_clusters == TRUE])) %>%
  mutate(percentage_n = cluster_n / n * 100, percentage_sum = cluster_sum / sum * 100)  %>%
  filter(publication_decade < 1800) %>% 
  filter(publication_decade > 1700)


pl_cluster_1710s <- paradise_clus_comp %>% filter(publication_year > 1709) %>% filter(publication_year < 1720) %>% group_by(publication_year) %>%
  dplyr::summarise(n = n(), sum = sum(length), spec_sum = sum(length[spec_cluster == TRUE]), spec_n = sum(spec_cluster == TRUE)) %>%
  mutate(percentage_sum = spec_sum / sum * 100) %>%
  mutate(percentage_n = spec_n / n * 100)

pl_1710s_fig <- ggplot() +
  geom_line(data = pl_cluster_1710s, aes(x=publication_year, y=percentage_n)) 
print(pl_1710s_fig)

pl_decades_fig <- ggplot() +
  geom_col(data = pl_cluster_decades, aes(x=publication_decade, y=percentage_n)) +
  theme(text = element_text(size = 20)) + 
  scale_y_continuous(name="Percentage of reuse cases") +
  scale_x_continuous(name="Decade", limits=c(1660,1800), breaks=seq(1670, 1790, 10)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(20, 50))
print(pl_decades_fig)

pl_entries_with_cluster_decades_fig <- ggplot() +
  geom_line(data = pl_entries_with_cluster_decades, aes(x=publication_decade, y=percentage_n))
print(pl_entries_with_cluster_decades_fig)

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig7_paradise_lost_decades.png", sep="")
png(file=file,width=1200, height=700)
print(pl_decades_fig)
dev.off()

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/paradise_lost_cluster_decades.png", sep="")
png(file=file,width=1200, height=700)
print(pl_entries_with_cluster_decades_fig)
dev.off()

## Spec cluster percentage
pl_spec_users <- paradise_clus_comp %>% 
  group_by(estc_id) %>% 
  filter(any(spec_cluster)) %>%
  dplyr::summarise(mean_length = mean(text_length), n = n(), sum = sum(length), spec_n = sum(spec_cluster == TRUE), spec_sum = sum(length[spec_cluster == TRUE])) %>%
  mutate(n_percent = spec_n / n * 100, sum_percent = spec_sum / sum * 100, in_spec = estc_id %in% spectatorReuseFiltered$estc_id) %>%
  group_by(in_spec) %>% dplyr::summarise(mean_length_of_entry = mean(mean_length), n = n(), mean_n = mean(n_percent), mean_sum = mean(sum_percent))


## Of the Spectator reuse data, how much of those reusing Spec clusters only use those aka only use Paradise Lost
## Filter Addison and Milton
spec_pl_uses <- spectatorReuse_no_pure %>% filter(estc_id == 'R13460') %>% distinct(clus)
remove_clusters <- spectatorReuse_no_pure %>% filter(publication_year < 1711) %>% distinct(clus) %>% filter(!clus %in% spec_pl_uses$clus)
spec_pl_data <- spectatorReuse_no_pure %>% group_by(estc_id) %>% filter(!any(clus %in% remove_clusters$clus)) %>% ungroup()

spec_only_pl <- spec_pl_data %>%
  filter(!included_in_pure) %>%
  filter(!grepl("addison | milton", tolower(author_name))) %>%
  filter(!grepl("paradise lost", tolower(title))) %>%
  mutate(pl_cluster = clus %in% spec_pl_uses$clus) %>%
  group_by(estc_id) %>%
  mutate(not_only_pl = any(!pl_cluster), has_pl_cluster = any(pl_cluster), publication_decade = publication_year - (publication_year %% 10)) %>%
  dplyr::ungroup() %>%
  distinct(estc_id, not_only_pl, has_pl_cluster, publication_decade) %>%
  group_by(publication_decade) %>%
  dplyr::summarise(all = n(), using_both = sum(not_only_pl == TRUE)) %>%
  filter(publication_decade < 1800) %>%
  filter(publication_decade > 1660) %>%
  mutate(percentage = using_both / all * 100)

paradise_entries <- paradise_filtered %>% mutate(spec_cluster = clus %in% pl_spec_uses$clus) %>%
  mutate(publication_decade = publication_year - (publication_year %% 10)) %>%
  filter(!grepl("milton", tolower(author))) %>%
  filter(!grepl("paradise lost", tolower(title)))
  
paradise_most_prominent <- paradise_entries %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n())

