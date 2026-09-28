d <- read_analysis_data()
articles <- copy(d$articles)
links <- copy(d$author_article)

complete_articles <- articles[article_year >= 2001 & article_year <= 2025]
annual <- data.table(article_year = 2001:2025)[
  complete_articles[, .(articles = uniqueN(article_id)), by = article_year], on = "article_year"
]
annual[is.na(articles), articles := 0L]
annual[, moving_average_3y := frollmean(articles, n = 3, align = "center")]
write_table(annual, "06_articulos_por_anio.csv")

complete_articles[, period := period_5(article_year)]
quinquennial <- complete_articles[!is.na(period), .(articles = uniqueN(article_id)), by = period]

article_years <- articles[article_year <= 2025, .(article_id, article_year)]
author_years <- links[article_years, on = "article_id", nomatch = 0]
first_year <- author_years[, .(first_article_year = min(article_year)), by = author_id]
period_def <- data.table(
  period = factor(period_levels, levels = period_levels),
  start = c(2001, 2006, 2011, 2016, 2021),
  end = c(2005, 2010, 2015, 2020, 2025)
)
productivity <- period_def[, {
  active_ids <- first_year[first_article_year <= end, author_id]
  participations <- author_years[author_id %in% active_ids & article_year >= start & article_year <= end,
                                 .(articles = uniqueN(article_id)), by = author_id]
  participations <- merge(data.table(author_id = active_ids), participations, by = "author_id", all.x = TRUE)
  participations[is.na(articles), articles := 0L]
  .(active_researchers = length(active_ids),
    author_article_participations = sum(participations$articles),
    mean_articles_per_active_researcher = mean(participations$articles),
    median_articles_per_active_researcher = as.numeric(median(participations$articles)))
}, by = .(period, start, end)]
quinquennial <- productivity[quinquennial, on = "period"]
write_table(quinquennial, "07_articulos_por_quinquenio.csv")

p_annual <- ggplot(annual, aes(article_year, articles)) +
  geom_line(color = "grey72", linewidth = .55) +
  geom_point(color = "grey58", size = 1.5) +
  geom_line(aes(y = moving_average_3y), color = main_color, linewidth = 1.15, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(2001, 2025, 4)) +
  scale_y_continuous(breaks = pretty_breaks(5), expand = expansion(mult = c(0, .08))) +
  labs(title = "Número de artículos publicados por año (todo el DCP)",
       subtitle = "La línea destacada es la media móvil de tres años",
       x = NULL, y = "Artículos") + theme_blog

p_productivity <- ggplot(productivity, aes(period, mean_articles_per_active_researcher)) +
  geom_col(fill = main_color, width = .58) +
  geom_text(aes(label = number(mean_articles_per_active_researcher, accuracy = .1)),
            vjust = -.45, size = 3.4, color = dark_gray) +
  geom_text(aes(label = paste0("n=", active_researchers)), y = .25,
            size = 2.8, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = pretty_breaks(5), expand = expansion(mult = c(0, .12))) +
  labs(title = "Productividad por investigador activo",
       subtitle = "Promedio de artículos por quinquenio; n indica investigadores activos",
       x = NULL, y = "Artículos por investigador") + theme_blog

save_figure(p_annual | p_productivity, "02_cuanto_publican.png", width = 12, height = 5.5)

stats_cuanto <- data.table(
  indicador = c("articulos_total", "articulos_hasta_2025",
                "articulos_2001_2005", "articulos_2021_2025",
                "promedio_por_investigador_activo_2001_2005",
                "promedio_por_investigador_activo_2021_2025"),
  valor = c(
    nrow(articles), articles[article_year <= 2025, .N],
    quinquennial[period == "2001–05", articles], quinquennial[period == "2021–25", articles],
    quinquennial[period == "2001–05", mean_articles_per_active_researcher],
    quinquennial[period == "2021–25", mean_articles_per_active_researcher]
  )
)
write_table(stats_cuanto, "00_indicadores_cuanto.csv")
