actor_information <- estc_data[, c("actor_id", "id")] %>% rename(estc_id = id)

country_information <- estc_data[, c("country_752", "id")] %>% rename(estc_id = id)

## I have filtered here those entries which only match with the print information and those containing Cato by Addison and Distressed MOther by Phillips
## since they are not actual reuse cases

reuse_with_actor_ids <- spectatorReuseFiltered  %>% left_join(actor_information, by="estc_id") %>%
  left_join(country_information, by="estc_id") %>% mutate(publication_decade = publication_year - (publication_year %% 10))%>% filter(!grepl("print", tolower(text_primary))) %>%
  filter(!grepl("cato", tolower(title))) %>% filter(!grepl("mother", tolower(title))) 

shakespeare <- allData %>% filter(grepl("Shakes", actor_name_primary))

tonson_entries_in_reuse <- reuse_with_actor_ids %>% filter(!grepl("print", tolower(text_primary))) %>% filter(actor_id %in% tonson_actor_ids$actor_id) %>% distinct(estc_id, .keep_all = TRUE) %>%
  filter(!grepl("cato", tolower(title))) %>% filter(!grepl("mother", tolower(title))) 

#121291376
j_r_tonson_entries_in_reuse <- reuse_with_actor_ids %>% filter(actor_id == "121291376")	%>% distinct(estc_id, .keep_all = TRUE)

tonson_ids <- spectatorReuseFiltered %>% filter(estc_id %in% tonson_entries_in_reuse$estc_id) %>% distinct(estc_id)

reused_by_tonson <- spectatorReuseFiltered %>% filter(estc_id %in% tonson_ids$estc_id) %>%
  group_by(estc_id, title) %>%
  dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

all_tonsons_values <- spectatorReuseFiltered %>% filter(estc_id %in% tonson_ids$estc_id) %>%
  dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

compare_all_and_tonsons <- all_tonsons_values %>% mutate(group = "Tonsons") %>% rbind(spectatorReuseFiltered %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n()) %>% mutate(group = "All"))

tonsons_in_estc <- allData %>% filter(actor_id %in% tonson_actor_ids$actor_id) 

tonsons_in_estc_after_1711 <- allData %>% filter(actor_id %in% tonson_actor_ids$actor_id)  %>% filter(publication_year > 1711)

lengths <- ecco_to_estc[,c("document_id", "text_length")]

prominent_reused_by_tonson <- spectatorReuseFiltered %>% 
  mutate(document_id = str_pad(id_secondary, width = 10, side = "left", pad = "0")) %>%
  filter(estc_id %in% tonsons_in_estc$id) %>%
  filter(!estc_id %in% addison$id) %>%
  merge(lengths, by="document_id", all.x = TRUE) %>%
  group_by(estc_id, title, text_length) %>%
  dplyr::summarise(sum = sum(length), n=n()) %>% 
  mutate(percentage = sum / text_length * 100)

spectatorSums <- spectatorReuseFiltered %>% group_by(estc_id) %>% dplyr::summarise(sum = sum(length))
row.names(spectatorSums) <- spectatorSums$estc_id

distinct_names = allData[, c("actor_id", "actor_name_primary")] %>% distinct(actor_id, .keep_all = TRUE)

start_years = allData %>% group_by(actor_id) %>% dplyr::summarise(start_year = min(publication_year, na.rm = TRUE))

actors_with_tonsons <- allData %>% filter(grepl("publisher", actor_roles_all)) %>% group_by(actor_id) %>%
  mutate(is_tonson_entry = id %in% tonsons_in_estc_after_1711$id) %>%
  group_by(actor_id) %>%
  dplyr::summarise(n = n(), with_tonson = sum(is_tonson_entry)) %>% ## left_join(distinct_names, by="actor_id") %>%
  mutate(w_tonson_per = with_tonson / n * 100) %>% select(-n)

get_biggest_publishers <- function(reuseData, yearStart, yearEnd, sum_data) {
  result <- allData %>% filter(publication_year > yearStart) %>% filter(publication_year < yearEnd) %>% filter(grepl("publisher", actor_roles_all)) %>%
    group_by(actor_id) %>% 
    dplyr::summarise(n = n(), books_reusing = sum(id %in% reuseData$estc_id), max = max(publication_year)) %>%
    mutate(percentage_in_reuse = books_reusing/n * 100 ,tonson = actor_id %in% tonson_actor_ids$actor_id) %>% filter(n > 100) %>% 
    left_join(distinct_names, by="actor_id") %>%
    left_join(sum_data, by="actor_id") %>% 
    left_join(actors_with_tonsons, by="actor_id") %>%
    left_join(start_years, by="actor_id")
  
  return(result)
}

get_prominent_publishers <- function(reuseData, yearStart, yearEnd) {
  reuses_by_id <- reuseData %>% group_by(estc_id) %>% dplyr::summarise(n = n(), sum = sum(length)) %>% mutate(id = estc_id)
  result <- allData %>% filter(publication_year > yearStart) %>% filter(publication_year < yearEnd) %>% filter(grepl("publisher", actor_roles_all)) %>%
    filter(id %in% reuseData$estc_id) %>%
    left_join(reuses_by_id, by = "id") %>%
    group_by(actor_id) %>% 
    dplyr::summarise(reuses = sum(n), length_sum = sum(sum)) %>% left_join(distinct_names, by="actor_id") %>% left_join(actors_with_tonsons, by="actor_id")
  return(result)
  
}

paradise_pub <- paradise_filtered ## %>% filter(!grepl("paradise lost", tolower(title)))

## Who are the most important publishers in Spectator data
prominent_publishers_in_spec <- get_prominent_publishers(spectatorReuseFiltered, 1711, 1800)

## Who are the most important publishers in Paradise Lost data 
prominent_publishers_in_pl <- get_prominent_publishers(paradise_pub, 1711, 1800)

## How much of whole publishing use Spectator
biggest_publishers_spec <- get_biggest_publishers(spectatorReuseFiltered, 1720, 1770, prominent_publishers_in_spec[, c("actor_id", "reuses", "length_sum")])

## How much of whole publishing use Paradise Lost
biggest_publishers_pl <- get_biggest_publishers(paradise_pub, 1711, 1800, prominent_publishers_in_pl[, c("actor_id", "reuses", "length_sum")])


#Scatter plots on correlation of reuse and work with Tonson
spec_scat_data <- biggest_publishers_spec %>% filter(w_tonson_per != 0) %>%
  mutate(overFive = w_tonson_per > 5) %>%
  group_by(overFive) %>%  
  dplyr::summarise(mean = mean(percentage_in_reuse))
  
spec_scat_fig <- ggplot(spec_scat_data, aes(x=overFive, y=mean)) + geom_col()
print(spec_scat_fig)

scat_mass_data <- biggest_publishers_spec %>%
  filter(!actor_id %in% tonson_actor_ids$actor_id) %>% 
  filter(w_tonson_per != 0) %>% mutate(reuse_mean = length_sum  / books_reusing) %>%
  mutate(overFive = w_tonson_per > 5) %>%
  group_by(overFive) %>%  
  dplyr::summarise(mean = mean(reuse_mean, na.rm = TRUE))

spec_scat_reuse_mass_fig <- ggplot(scat_mass_data, aes(x=overFive, y=mean)) + geom_col()
print(spec_scat_reuse_mass_fig)

pl_scat_fig <- ggplot(biggest_publishers_pl %>% filter(w_tonson_per != 0), aes(x=w_tonson_per, y=percentage_in_reuse)) + geom_point() +xlim(0, 20)+ 
  geom_smooth(method=lm)
print(pl_scat_fig)


#Spectator reuse by Tonsons and close
# This double counts if in many groups, but should not double count if many from same group
reuse_counts_by_id <- spectatorReuseFiltered %>% group_by(estc_id) %>% dplyr::summarise(n = n(), sum = sum(length))
reused_by <- reuse_with_actor_ids %>%
  mutate(published_by = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% actors_with_tonsons$actor_id, "Close", "Other"))) %>%
  ##filter(publication_year > 1729) %>% filter(publication_year < 1740) %>%
  filter(country_752 == "England") %>%
  group_by(estc_id, published_by) %>%
  dplyr::summarise() %>%
  left_join(reuse_counts_by_id, by="estc_id")%>%
  filter(sum > 1000000) %>%
  group_by(published_by) %>%
  dplyr::summarise(books = n(), reuse_cases = sum(n), sum = sum(sum)) 


editions_per_decade <- pure_only %>%
  filter(country_752 == "England") %>% distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(editions = n())

spectator_editions_by <- pure_only  %>%
  mutate(published_by = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% actors_with_tonsons$actor_id, "Close", "Other"))) %>%
  filter(publication_year > 1720) %>% filter(publication_year < 1800) %>%
  filter(country_752 == "England") %>%
  group_by(published_by, publication_decade) %>%
  distinct(id, .keep_all = TRUE) %>%
  dplyr::summarise(n = n()) %>%
  left_join(editions_per_decade, by="publication_decade") %>% mutate(percentage = n/editions)

spectator_editions_all <- pure_only  %>%
  mutate(published_by = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% actors_with_tonsons$actor_id, "Close", "Other"))) %>%
  filter(publication_year > 1711) %>% filter(publication_year < 1770) %>%
  filter(country_752 == "England") %>%
  group_by(published_by) %>%
  distinct(id, .keep_all = TRUE) %>%
  dplyr::summarise(n = n())

editions_count <- pure_only  %>%
  mutate(published_by = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% actors_with_tonsons$actor_id, "Close", "Other"))) %>%
  filter(publication_year > 1711) %>% filter(publication_year < 1770) %>%
  filter(country_752 == "England") %>%
  distinct(id, .keep_all = TRUE) %>%
  dplyr::summarise(n = n())

## Mean sum of publisher reusing
reuse_counts_by_actor_id <- reuse_with_actor_ids %>% group_by(actor_id, estc_id) %>% dplyr::summarise() %>% left_join(reuse_counts_by_id, by="estc_id") %>%
  group_by(actor_id) %>%
  dplyr::summarise(books = n(), reuses = sum(n), sum = sum(sum))
  
publishers_only_using <- reuse_with_actor_ids %>%
  mutate(published_by = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% actors_with_tonsons$actor_id, "Close", "Other"))) %>%
  group_by(actor_id, published_by)  %>% dplyr::summarise() %>%
  left_join(reuse_counts_by_actor_id, by="actor_id") %>%
  group_by(published_by) %>%
  dplyr::summarise(mean_n = mean(reuses), mean_sum = mean(sum), meadian_n = median(reuses), median_sum = median(sum))

reuse_counts_by_id_and_name <- spectatorReuseFiltered %>% group_by(estc_id, title, publication_year) %>% dplyr::summarise(n = n(), sum = sum(length))

## Check how use grows by decade
tonson_entries_by_decade <- allData %>% filter(actor_id %in% tonson_actor_ids$actor_id) %>% distinct(id, .keep_all = TRUE) %>% 
  group_by(publication_decade) %>% dplyr::summarise(n = n()) %>% filter(publication_decade > 1710 & publication_decade < 1780) %>%
  mutate(index = n / n[1])

tonson_reuse_by_decade <- reuse_with_actor_ids %>% filter(publication_decade > 1710 & publication_decade < 1780) %>% filter(actor_id %in% tonson_actor_ids$actor_id) %>% group_by(estc_id, publication_decade) %>%
  dplyr::summarise() %>% left_join(reuse_counts_by_id, by="estc_id") %>% group_by(publication_decade) %>% 
  dplyr::summarise(books = n(), reuse_cases = sum(n), reuse_amount = sum(sum)) %>% mutate(reuse_index = books / books[1]) %>%
  left_join(tonson_entries_by_decade, by="publication_decade")

books_by_tonsons <- ggplot() + geom_col(data = tonson_reuse_by_decade, aes(x=publication_decade, y=books)) +
  theme(text = element_text(size = 20)) + 
  scale_x_continuous(name="Decade", limits = c(1710, 1770)) +
  scale_y_continuous(name="Books") +
  theme(legend.title=element_blank())

print(books_by_tonsons)

tonson_entries_in_reuse_sums <- reuse_with_actor_ids %>% filter(!grepl("print", tolower(text_primary))) %>% filter(actor_id %in% tonson_actor_ids$actor_id) %>% distinct(estc_id, .keep_all = TRUE) %>%
  filter(!grepl("cato", tolower(title))) %>% filter(!grepl("mother", tolower(title))) %>% group_by(estc_id, title, publication_year) %>% dplyr::summarise() %>%
  left_join(reuse_counts_by_id, by="estc_id")

tonson_full_editions_per_decade <- pure_only %>% filter(actor_id %in% tonson_actor_ids$actor_id) %>%
  distinct(id, .keep_all = TRUE) %>% group_by(publication_decade) %>% dplyr::summarise(n = n())

reuse_entries_by_publisher <- reuse_with_actor_ids %>% group_by(actor_id) %>% distinct(estc_id, .keep_all = TRUE)


file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/used/fig_8tonson_reuse_books_by_decade.png", sep="")
png(file=file,width=1200, height=700)
print(books_by_tonsons)
dev.off()

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/spectator_scatter_fig.png", sep="")
png(file=file,width=1200, height=700)
print(spec_scat_fig)
dev.off()

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/spectator_scatter_fig_reuse_mass.png", sep="")
png(file=file,width=1200, height=700)
print(spec_scat_reuse_mass_fig)
dev.off()
