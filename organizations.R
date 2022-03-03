draw_organization_fig <- function(data, position, title) { 
  data <- data %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade < 1800) %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    filter(!is.na(actor_id)) %>%
    filter(actor_id != "") %>%
    group_by(publication_decade) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    group_by(publication_decade, is_organization) %>%
    dplyr::summarise(n = n())
  
  figure <- ggplot(data = data, aes(fill=is_organization, y = n, x = publication_decade)) +
    geom_bar(position=position, stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle(title)
  
  return(figure)
}

fig_org_1 <- draw_organization_fig(spectator, "fill", "Plain data")

fig_org_2 <- draw_organization_fig(estc_only, "fill", "ESTC only")

fig_org_3 <- draw_organization_fig(estc_and_bernard, "fill", "ESTC and Bernard")

fig_org_4 <- draw_organization_fig(pure_only, "fill", "Pure Spectator")

fig_org_5 <- draw_organization_fig(tatler, "fill", "Tatler")

fig_org_6 <- draw_organization_fig(allData, "fill", "Whole ESTC")

final_org <- fig_org_1 + fig_org_2 + fig_org_3 + fig_org_4 + fig_org_5 + fig_org_6

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/cleaned_thesis_code/graphs/unused/organizations.png", sep="")
png(file=file,
    width=1200, height=700)


print(final_org)

dev.off()


