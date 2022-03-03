## Reuse data without the entries that were deemed pure Spectator in metadata analysis
spectatorReuse_no_pure <- spectatorReuse %>% filter(!included_in_pure)

## Actor ids of the Tonson publishing family
tonson_actor_ids <- spectator %>% filter(grepl("Tonson", actor_name_primary)) %>% distinct(actor_id)

## Unique entires reusing the Spectator. Grouped by final work field and ID if former missing
final_work_fields <- estc_data[c('id', 'finalWorkField')] %>% rename(estc_id = id)
unique_spectatorReuse <- spectatorReuse %>% distinct(estc_id) %>% dplyr::left_join(final_work_fields, by="estc_id") %>% mutate(finalWorkField = ifelse(is.na(finalWorkField), estc_id, finalWorkField)) %>% distinct(finalWorkField, .keep_all = TRUE)

distinct_titles <- spectatorReuse %>% distinct(estc_id, .keep_all = TRUE) %>% group_by(title) %>% dplyr::summarise(n = n())

## Searching distinct titles from reuse data and only using the most prominent one to see if the weigths of character reuse shift since there are not
## so many editions of the same book. Of course you can't say that having multiple editions of same book does not represent anything, but this is an interesting
## comparison point. 
## This is not perfect. Many titles are used multiple times, but this will make the differents considerably smaller
distinct_reuse_entries <- spectatorReuse %>% group_by(title, estc_id) %>%
  dplyr::summarise(sum = sum(length)) %>% ungroup() %>% group_by(title) %>% filter(sum == max(sum)) %>% ungroup() %>%
  distinct(estc_id, .keep_all = TRUE)

under5000_entries <- spectatorReuse %>% group_by(estc_id, title) %>% 
  dplyr::summarise(reuse_sum = sum(length)) %>% filter(reuse_sum < 5000)

under2500_entries <- spectatorReuse %>% group_by(estc_id) %>% 
  dplyr::summarise(reuse_sum = sum(length)) %>% filter(reuse_sum < 2500)

differenceGroup <- function(number) {
  if(number < 50) {
    return ("Under50")
  }
  if(number < 200) {
    return ("Under200")
  }
  return ("Over200")
  
}