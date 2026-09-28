d <- read_analysis_data()
articles <- copy(d$articles)
journals <- copy(d$journals)

scope_fields <- journals[, .(journal_id, journal_scope_category, journal_field,
                             journal_scope_confidence, journal_scope_source)]
articles <- scope_fields[articles, on = "journal_id"]
recent <- articles[article_year >= 2021 & article_year <= 2025]

journal_recent <- recent[!is.na(journal_id), .(
  articles = uniqueN(article_id),
  journal_name = first(journal_name)
), by = journal_id]
setorder(journal_recent, -articles, journal_name)
journal_recent[, rank := .I]
write_table(journal_recent, "08_revistas_2021_2025.csv")

articles_time <- articles[article_year >= 2001 & article_year <= 2025]
articles_time[, period := period_5(article_year)]
detected <- cld3::detect_language(articles_time$article_title)
articles_time[, language := fcase(
  detected == "es", "Español",
  detected == "en", "Inglés",
  detected == "pt", "Portugués",
  default = "Otros"
)]
language_levels <- c("Español", "Inglés", "Portugués", "Otros")
articles_time[, language := factor(language, levels = language_levels)]
language_period <- articles_time[!is.na(period), .N, by = .(period, language)]
language_period[, share := N / sum(N), by = period]
write_table(language_period, "09_idioma_por_quinquenio.csv")

quality_time <- rbind(
  articles_time[, .(
    covered = sum(ranked_scimago == TRUE, na.rm = TRUE),
    high_tier = sum(q1_scimago == TRUE, na.rm = TRUE)
  ), by = period][, sistema := "Scimago Q1"],
  articles_time[, .(
    covered = sum(!is.na(scopus_highest_percentile)),
    high_tier = sum(scopus_highest_percentile >= 90, na.rm = TRUE)
  ), by = period][, sistema := "Scopus top 10%"]
)
period_totals <- articles_time[, .(total_articles = .N), by = period]
quality_time <- period_totals[quality_time, on = "period"]
quality_time[, share_all_articles := high_tier / total_articles]
quality_time[, sistema := factor(sistema, levels = c("Scimago Q1", "Scopus top 10%"))]
setorder(quality_time, sistema, period)
write_table(quality_time, "10_calidad_por_quinquenio.csv")

indexing <- data.table(
  sistema = c("Algún índice", "Scimago", "Scopus", "Qualis", "Latindex"),
  covered = c(
    sum(fcoalesce(recent$ranked_scimago, FALSE) |
          fcoalesce(recent$scopus_match, FALSE) |
          fcoalesce(recent$ranked_qualis, FALSE) |
          fcoalesce(recent$indexed_latindex, FALSE)),
    sum(recent$ranked_scimago == TRUE, na.rm = TRUE),
    sum(recent$scopus_match == TRUE, na.rm = TRUE),
    sum(recent$ranked_qualis == TRUE, na.rm = TRUE),
    sum(recent$indexed_latindex == TRUE, na.rm = TRUE)
  )
)
indexing[, `:=`(total_articles = nrow(recent), coverage_share = covered / nrow(recent))]

quality <- data.table(
  sistema = c("Scimago Q1", "Scopus top 10%"),
  covered = c(sum(recent$ranked_scimago == TRUE, na.rm = TRUE),
              sum(!is.na(recent$scopus_highest_percentile))),
  high_tier = c(sum(recent$q1_scimago == TRUE, na.rm = TRUE),
                sum(recent$scopus_highest_percentile >= 90, na.rm = TRUE))
)
quality[, `:=`(total_articles = nrow(recent), high_tier_share_all_articles = high_tier / nrow(recent))]
write_table(indexing, "11_cobertura_indices_2021_2025.csv")
write_table(quality, "12_posicion_editorial_2021_2025.csv")

language_colors <- c("Español" = main_color, "Inglés" = third_color,
                     "Portugués" = "#B49A70", "Otros" = "grey82")
p_language <- ggplot(language_period, aes(period, share, fill = language)) +
  geom_col(width = .62) +
  geom_text(aes(label = ifelse(share >= .07, percent(share, accuracy = 1), "")),
            position = position_stack(vjust = .5), size = 2.9, color = "white") +
  scale_fill_manual(values = language_colors) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "(A) Idioma estimado a partir del título", x = NULL, y = "% de artículos") + theme_blog

quality_colors <- c("Scimago Q1" = main_color, "Scopus top 10%" = third_color)
p_quality_share <- ggplot(quality_time, aes(period, share_all_articles, color = sistema, group = sistema)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = percent(share_all_articles, accuracy = .1)),
            vjust = -1, size = 3, show.legend = FALSE) +
  scale_color_manual(values = quality_colors) +
  scale_x_discrete(labels = setNames(paste0(period_totals$period, "\n(n=", period_totals$total_articles, ")"),
                                     period_totals$period)) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(.02, .15))) +
  expand_limits(y = 0) +
  labs(title = "Proporción sobre todos los artículos",
       subtitle = "Rankings disponibles en 2025–2026",
       x = NULL, y = "% del total del quinquenio") + theme_blog

p_quality_count <- ggplot(quality_time, aes(period, high_tier, fill = sistema)) +
  geom_col(position = position_dodge(width = .72), width = .62) +
  geom_text(aes(label = high_tier), position = position_dodge(width = .72),
            vjust = -.55, size = 3, color = dark_gray) +
  scale_fill_manual(values = quality_colors, guide = "none") +
  scale_x_discrete(labels = setNames(paste0(period_totals$period, "\n(n=", period_totals$total_articles, ")"),
                                     period_totals$period)) +
  scale_y_continuous(breaks = pretty_breaks(5), expand = expansion(mult = c(.02, .18))) +
  expand_limits(y = 0) +
  labs(title = "Número absoluto de artículos", x = NULL, y = "Artículos") + theme_blog

p_index <- ggplot(indexing, aes(coverage_share, reorder(sistema, coverage_share),
                                fill = sistema == "Algún índice")) +
  geom_col(width = .58, show.legend = FALSE) +
  geom_text(aes(label = paste0(covered, " (", percent(coverage_share, accuracy = 1), ")")),
            hjust = -.18, size = 3.2, color = dark_gray) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, .25))) +
  scale_fill_manual(values = c(`TRUE` = second_color, `FALSE` = main_color)) +
  labs(title = "(B) Cobertura en índices", subtitle = "Artículos de 2021–2025",
       x = "% de artículos", y = NULL) + theme_blog

figure_international <- (p_language | p_index) +
  plot_annotation(title = "Internacionalización y circulación",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", color = "grey20")))
save_figure(figure_international, "03a_internacionalizacion_circulacion.png", width = 11.5, height = 5.5)

figure_quality <- (p_quality_share / p_quality_count) +
  plot_layout(heights = c(3, 2), guides = "collect") +
  plot_annotation(title = "Publicaciones en journals de mayor posición",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", color = "grey20"))) &
  theme(legend.position = "bottom")
save_figure(figure_quality, "03b_journals_mayor_posicion.png", width = 10.5, height = 8)

stats_donde <- data.table(
  indicador = c("articulos_2021_2025", "revistas_2021_2025", "revistas_con_un_articulo",
                "porcentaje_en_top_10_revistas", "articulos_en_algun_indice",
                "articulos_scimago", "articulos_scimago_q1",
                "articulos_scopus", "articulos_scopus_top10", "articulos_qualis", "articulos_latindex"),
  valor = c(
    nrow(recent), nrow(journal_recent), sum(journal_recent$articles == 1),
    100 * sum(head(journal_recent$articles, 10)) / nrow(recent),
    indexing[sistema == "Algún índice", covered],
    indexing[sistema == "Scimago", covered], quality[sistema == "Scimago Q1", high_tier],
    indexing[sistema == "Scopus", covered], quality[sistema == "Scopus top 10%", high_tier],
    indexing[sistema == "Qualis", covered],
    indexing[sistema == "Latindex", covered]
  )
)
write_table(stats_donde, "00_indicadores_donde.csv")
