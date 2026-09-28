d <- read_analysis_data()
articles <- copy(d$articles)

coauthorship_articles <- articles[article_year >= 2001 & article_year <= 2025]

# Evolución anual del tamaño de los equipos de autoría.
coauthorship_annual <- data.table(article_year = 2001:2025)[
  coauthorship_articles[, .(
    articles = uniqueN(article_id),
    mean_authors_per_article = mean(n_authors_total)
  ), by = article_year],
  on = "article_year"
]
coauthorship_annual[, moving_average_3y := frollmean(
  mean_authors_per_article, n = 3, align = "center"
)]
write_table(coauthorship_annual, "15_autores_por_articulo_anio.csv")

# Categorías exhaustivas y mutuamente excluyentes solicitadas.
coauthorship_articles[, collaboration_type := fcase(
  n_authors_total == 1, "Autor único",
  n_authors_total > 1 & n_dcp_authors == 1, "Coautoría externa",
  n_dcp_authors > 1, "Coautoría con otros DCP"
)]
collaboration_levels <- c(
  "Autor único", "Coautoría externa", "Coautoría con otros DCP"
)
coauthorship_articles[, `:=`(
  period = period_5(article_year),
  collaboration_type = factor(collaboration_type, levels = collaboration_levels)
)]

coauthorship_period <- coauthorship_articles[, .N, by = .(period, collaboration_type)]
coauthorship_period[, `:=`(
  share = N / sum(N),
  articles_period = sum(N)
), by = period]
write_table(coauthorship_period, "16_tipos_coautoria_por_quinquenio.csv")

p_team_size <- ggplot(
  coauthorship_annual,
  aes(article_year, mean_authors_per_article)
) +
  geom_line(color = "grey72", linewidth = .55) +
  geom_point(color = "grey58", size = 1.5) +
  geom_line(
    aes(y = moving_average_3y),
    color = main_color, linewidth = 1.15, na.rm = TRUE
  ) +
  scale_x_continuous(breaks = seq(2001, 2025, 4)) +
  scale_y_continuous(
    breaks = pretty_breaks(5),
    expand = expansion(mult = c(0, .08))
  ) +
  labs(
    title = "Número promedio de autores por artículo",
    subtitle = "La línea destacada es la media móvil de tres años",
    x = NULL, y = "Autores por artículo"
  ) +
  theme_blog

period_totals <- unique(coauthorship_period[, .(period, articles_period)])
p_collaboration <- ggplot(
  coauthorship_period,
  aes(period, share, fill = collaboration_type)
) +
  geom_col(width = .66) +
  geom_text(
    aes(label = percent(share, accuracy = 1)),
    position = position_stack(vjust = .5),
    color = "white", size = 3.2, fontface = "bold"
  ) +
  geom_text(
    data = period_totals,
    aes(period, -.055, label = paste0("n=", articles_period)),
    inherit.aes = FALSE, color = "grey45", size = 3
  ) +
  scale_fill_manual(values = c(
    "Autor único" = main_color,
    "Coautoría externa" = second_color,
    "Coautoría con otros DCP" = third_color
  )) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, .25),
    expand = expansion(mult = c(0, .02))
  ) +
  coord_cartesian(ylim = c(-.1, 1), clip = "off") +
  labs(
    title = "Modalidades de autoría por quinquenio",
    subtitle = "Porcentaje de artículos; n indica el total de artículos del período",
    x = NULL, y = "Porcentaje de artículos", fill = NULL
  ) +
  theme_blog +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_line(color = "grey90", linewidth = .35),
    plot.margin = margin(8, 10, 16, 8)
  )

coauthorship_figure <- p_team_size / p_collaboration +
  plot_layout(heights = c(1, 1.05)) +
  plot_annotation(
    title = "Evolución de la coautoría en los artículos del DCP",
    theme = theme(
      plot.title = element_text(
        size = 15, face = "bold", color = "grey20", margin = margin(b = 7)
      )
    )
  )

save_figure(
  coauthorship_figure,
  "05_evolucion_coautoria.png",
  width = 10, height = 9
)

stats_coauthorship <- data.table(
  indicador = c(
    "promedio_autores_por_articulo_2001",
    "promedio_autores_por_articulo_2025",
    paste0(
      "porcentaje_2021_2025_",
      c("autor_unico", "coautoria_externa", "coautoria_con_otros_dcp")
    )
  ),
  valor = c(
    coauthorship_annual[article_year == 2001, mean_authors_per_article],
    coauthorship_annual[article_year == 2025, mean_authors_per_article],
    coauthorship_period[period == "2021–25", share] * 100
  )
)
write_table(stats_coauthorship, "00_indicadores_coautoria.csv")
