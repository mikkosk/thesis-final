#Reuse data grouped by id
data_by_id <- spectatorReuseFiltered %>% group_by(id_secondary, title) %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

#Decades from ESTC data for entries
decades <- estc_data[c("id", "publication_decade")] %>% 
  rename(estc_id = id) %>% 
  distinct(estc_id, .keep_all = TRUE)

#Counties from ESTC data to entries
countries <- estc_data[c("id", "country_752")] %>%
  rename(estc_id = id, country = country_752) %>%
  distinct(estc_id, .keep_all = TRUE)

#Joseph Addison's Evidences of christianity in Spectator reuse data
evidences <- spectatorReuseFiltered %>% filter(grepl("evidences of the christian", tolower(title))) %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

# Pleasing moralist in reuse data
pleasing_moralist <- spectatorReuseFiltered %>% filter(grepl("pleasing instructor", tolower(title))) %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

#Pure editions reuse
spectator_editions = spectatorReuseCleaned %>% filter(included_in_pure) %>% filter(estc_id != 'T152252') %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

#Pure editions compared to everything else
spectator_editions_v_all = spectatorReuseCleaned %>% group_by(included_in_pure) %>% dplyr::summarise(sum = sum(length))

## Paradise lost estc
paradise_estc <- allData %>% filter(grepl("paradise lost", tolower(title))) %>% group_by(publication_decade) %>%
  dplyr::summarise(n=n())

## Paradise Lost in reuse
paradise_spec_ecco <- spectatorReuseFiltered %>% filter(grepl("paradise lost", tolower(title))) %>% group_by(estc_id, title, publication_year) %>%
  dplyr::summarise(n=n(), sum = sum(length))

#If Spectator edition in ESTC can be found from ECCO
spectator_editions_in_ecco <- data.frame(estc_id = unique(pure_only$id)) %>% mutate(in_ecco = estc_id %in% spectatorReuse$estc_id)

#Example of reuse lengths
length_examples <- spectatorReuseFiltered %>% filter(grepl('T97990|T89166', estc_id)) %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length))
