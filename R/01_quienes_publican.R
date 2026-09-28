d <- read_analysis_data()
authors <- copy(d$authors)
publication_authors <- copy(d$publication_authors)
articles <- copy(d$articles)
links <- copy(d$author_article)

grade_levels <- paste0("Grado ", 2:5)
authors[, grado := factor(paste0("Grado ", grado_2026), levels = grade_levels)]
publication_authors[, grado := factor(paste0("Grado ", grado_2026), levels = grade_levels)]

recent_ids <- articles[recent_2021_2025 == TRUE, article_id]
recent_counts <- links[article_id %in% recent_ids, .(recent_articles = uniqueN(article_id)), by = author_id]
author_profile <- merge(authors, recent_counts, by = "author_id", all.x = TRUE)
author_profile[cv_available == TRUE & is.na(recent_articles), recent_articles := 0L]

phd <- authors[doctorado == 1 & !is.na(pais_doctorado) & !is.na(anio_doctorado),
               .(pais_doctorado, anio_doctorado)]
phd[, periodo_doctorado := fcase(
  anio_doctorado >= 1995 & anio_doctorado <= 1999, "1995–99",
  anio_doctorado >= 2000 & anio_doctorado <= 2004, "2000–04",
  anio_doctorado >= 2005 & anio_doctorado <= 2009, "2005–09",
  anio_doctorado >= 2010 & anio_doctorado <= 2014, "2010–14",
  anio_doctorado >= 2015 & anio_doctorado <= 2019, "2015–19",
  anio_doctorado >= 2020 & anio_doctorado <= 2024, "2020–24",
  anio_doctorado >= 2025 & anio_doctorado <= 2026, "2025–26",
  default = NA_character_
)]
phd <- phd[!is.na(periodo_doctorado)]
phd_periods <- c("1995–99", "2000–04", "2005–09", "2010–14", "2015–19", "2020–24", "2025–26")
phd_heat <- phd[, .N, by = .(pais_doctorado, periodo_doctorado)]
phd_heat <- CJ(pais_doctorado = sort(unique(phd$pais_doctorado)), periodo_doctorado = phd_periods)[
  phd_heat, on = .(pais_doctorado, periodo_doctorado)]
phd_heat[is.na(N), N := 0L]
country_order <- phd[, .N, by = pais_doctorado][order(N)]$pais_doctorado
phd_heat[, `:=`(
  pais_doctorado = factor(pais_doctorado, levels = country_order),
  periodo_doctorado = factor(periodo_doctorado, levels = phd_periods)
)]
write_table(phd_heat, "01_formacion_doctoral.csv")

author_participation <- author_profile[, .(
  author_id, author_name, grado, dt_2026, doctorado, sni_2026, cv_available,
  scholar_available, scholar_h_index, scholar_citations_total, recent_articles
)]
setorder(author_participation, -recent_articles, author_name, na.last = TRUE)
write_table(author_participation, "02_participacion_reciente_autores.csv")

audit <- build_authorship_audit(articles, d$author_article_all, d$authors_all,
                                publication_authors$author_id)
write_table(audit, "03_auditoria_coautoria_articulos.csv")

audit[, period := period_5(article_year)]
collaboration_levels <- c("Autoría individual", "Coautoría externa al DCP",
                          "Coautoría interna al DCP", "Coautoría mixta")
audit[, collaboration_verified := factor(collaboration_verified, levels = collaboration_levels)]
collaboration_period <- audit[!is.na(period), .N, by = .(period, collaboration_verified)]
collaboration_period[, share := N / sum(N), by = period]
write_table(collaboration_period, "04_coautoria_verificada_por_quinquenio.csv")

audit[, collaboration_original := fcase(
  solo_author_original == TRUE, "Autoría individual",
  n_dcp_authors_original > 1, "Coautoría DCP",
  default = "Coautoría externa (regla anterior)"
)]
audit_comparison <- audit[article_year >= 2021 & article_year <= 2025,
                          .N, by = .(collaboration_original, collaboration_verified)]
write_table(audit_comparison, "05_auditoria_coautoria_comparacion.csv")

# Figura 01: formación doctoral, publicaciones e impacto.
x_tot <- phd_heat[, .(total = sum(N)), by = periodo_doctorado]
y_tot <- phd_heat[, .(total = sum(N)), by = pais_doctorado]
x_labs <- setNames(paste0(x_tot$periodo_doctorado, "\n(", x_tot$total, ")"), x_tot$periodo_doctorado)
y_labs <- setNames(paste0(y_tot$pais_doctorado, " (", y_tot$total, ")"), y_tot$pais_doctorado)
p_phd <- ggplot(phd_heat, aes(periodo_doctorado, pais_doctorado, fill = N)) +
  geom_tile(color = "white", linewidth = .55) +
  geom_text(aes(label = ifelse(N == 0, "", N)), color = dark_gray, size = 2.9) +
  scale_fill_gradient(low = "grey96", high = second_color) +
  scale_x_discrete(labels = x_labs) +
  scale_y_discrete(labels = y_labs) +
  labs(title = "Formación doctoral", subtitle = "País y período de obtención",
       x = NULL, y = NULL) + theme_blog +
  guides(fill = "none") +
  theme(panel.grid = element_blank(), legend.position = "none",
        axis.text.x = element_text(size = 7.5), axis.text.y = element_text(size = 8))

publication_plot <- author_profile[cv_available == TRUE]
recent_color <- main_color
p_publications <- ggplot(publication_plot, aes(grado, recent_articles)) +
  geom_boxplot(width = .5, outlier.shape = NA, color = "grey70", fill = NA) +
  geom_point(
    position = position_jitter(width = .13, height = 0, seed = 2026),
    size = 2.2, alpha = .8, color = recent_color
  ) +
  scale_y_continuous(breaks = pretty_breaks(5), expand = expansion(mult = c(.02, .08))) +
  labs(title = "Por grado", subtitle = "Artículos por investigador con CVuy, 2021–2025",
       x = NULL, y = "Artículos") + theme_blog

area_recent <- publication_plot[!is.na(area_1)]
area_summary <- area_recent[, .(mediana = median(recent_articles), n = .N), by = area_1][order(mediana, area_1)]
area_summary[, area_label := paste0(area_1, " (n=", n, ")")]
area_recent <- area_summary[, .(area_1, area_label)][area_recent, on = "area_1"]
area_recent[, area_label := factor(area_label, levels = area_summary$area_label)]
p_area_recent <- ggplot(area_recent, aes(recent_articles, area_label)) +
  geom_boxplot(width = .5, outlier.shape = NA, color = "grey70", fill = NA) +
  geom_point(
    position = position_jitter(width = 0, height = .13, seed = 2026),
    size = 2.2, alpha = .8, color = recent_color
  ) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  labs(title = "Por área temática", subtitle = "Áreas ordenadas por la mediana",
       x = "Artículos por investigador, 2021–2025", y = NULL) + theme_blog +
  theme(axis.text.y = element_text(size = 8.5))

grade_composition <- authors[, .N, by = grado]
area_composition <- authors[, .N, by = area_1][order(N)]
p_grade_composition <- ggplot(grade_composition, aes(grado, N)) +
  geom_col(fill = main_color, width = .58) +
  geom_text(aes(label = N), vjust = -.45, size = 3.1, color = dark_gray) +
  scale_y_continuous(breaks = pretty_breaks(4), expand = expansion(mult = c(0, .15))) +
  labs(title = "Composición por grado", x = NULL, y = "Investigadores") + theme_blog
p_area_composition <- ggplot(area_composition, aes(N, reorder(area_1, N))) +
  geom_col(fill = second_color, width = .58) +
  geom_text(aes(label = N), hjust = -.45, size = 3, color = dark_gray) +
  scale_x_continuous(breaks = pretty_breaks(4), expand = expansion(mult = c(0, .18))) +
  labs(title = "Composición por área", x = "Investigadores", y = NULL) + theme_blog +
  theme(axis.text.y = element_text(size = 7.5))

save_figure((p_phd | p_grade_composition | p_area_composition) +
              plot_layout(widths = c(1.45, .65, 1)),
            "01a_perfil_investigadores.png", width = 15, height = 5.6)

figure_recent <- (p_publications / p_area_recent) +
  plot_layout(heights = c(1, 1.15)) +
  plot_annotation(title = "Producción reciente por grado y área",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", color = "grey20")))
save_figure(figure_recent, "01b_publicaciones_impacto.png", width = 11.5, height = 9)

# Red de coautoría entre docentes del universo con CVuy accesible.
network_articles <- audit[article_year >= 2001 & article_year <= 2025 & n_analysis_authors_verified > 1]
edge_list <- rbindlist(lapply(seq_len(nrow(network_articles)), function(i) {
  ids <- strsplit(network_articles$analysis_author_ids_verified[i], " \\| ")[[1]]
  if (length(ids) < 2) return(NULL)
  pairs <- t(combn(sort(ids), 2))
  data.table(from = pairs[, 1], to = pairs[, 2], article_id = network_articles$article_id[i])
}))
if (!nrow(edge_list)) edge_list <- data.table(from = character(), to = character(), article_id = character())
edges <- edge_list[, .(shared_articles = uniqueN(article_id)), by = .(from, to)]

publication_counts <- links[articles[article_year >= 2001 & article_year <= 2025, .(article_id)],
                            on = "article_id", nomatch = 0,
                            .(publications_2001_2025 = uniqueN(article_id)), by = author_id]
nodes <- publication_authors[, .(author_id, author_name, area_1)]
nodes[is.na(area_1), area_1 := "Sin área informada"]
nodes <- publication_counts[nodes, on = "author_id"]
nodes[is.na(publications_2001_2025), publications_2001_2025 := 0L]
degree_from <- edges[, .(collaborators = uniqueN(to)), by = .(author_id = from)]
degree_to <- edges[, .(collaborators = uniqueN(from)), by = .(author_id = to)]
degrees <- rbind(degree_from, degree_to)[, .(collaborators = sum(collaborators)), by = author_id]
nodes <- degrees[nodes, on = "author_id"]
nodes[is.na(collaborators), collaborators := 0L]

areas <- sort(unique(nodes$area_1))
area_angles <- setNames(seq(0, 2*pi, length.out = length(areas) + 1)[-(length(areas) + 1)], areas)
layout_rows <- lapply(areas, function(area) {
  sub <- nodes[area_1 == area][order(-publications_2001_2025, author_name)]
  center_angle <- area_angles[[area]]
  cx <- 3.4 * cos(center_angle); cy <- 3.4 * sin(center_angle)
  local_angles <- seq(0, 2*pi, length.out = nrow(sub) + 1)[-(nrow(sub) + 1)] + center_angle
  radius <- ifelse(nrow(sub) >= 10, 1.08, .88)
  sub[, `:=`(x = cx + radius*cos(local_angles), y = cy + radius*sin(local_angles),
             x_label = cx + (radius + .34)*cos(local_angles),
             y_label = cy + (radius + .34)*sin(local_angles))]
  sub
})
nodes <- rbindlist(layout_rows)

surname <- vapply(strsplit(nodes$author_name, " "), tail, character(1), 1)
duplicate_surname <- duplicated(surname) | duplicated(surname, fromLast = TRUE)
nodes[, label := surname]
nodes[duplicate_surname, label := paste0(substr(author_name, 1, 1), ". ", surname[duplicate_surname])]
nodes[author_id == "AUTH014", label := "González Scandizzi"]
nodes[author_id == "AUTH023", label := "Pérez Bentancur"]
nodes[author_id == "AUTH026", label := "Rocha-Carpiuc"]

edges_plot <- nodes[, .(from = author_id, x, y)][edges, on = "from"]
setnames(edges_plot, c("x", "y"), c("x_from", "y_from"))
edges_plot <- nodes[, .(to = author_id, x, y)][edges_plot, on = "to"]
setnames(edges_plot, c("x", "y"), c("x_to", "y_to"))
area_palette <- setNames(c("#3F6F8F", "#C07A5A", "#7A9E7E", "#A78BBE", "#D5A253",
                           "#5E9FA3", "#9B7B6D", "#8F9AA6")[seq_along(areas)], areas)
p_network <- ggplot() +
  geom_segment(data = edges_plot,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = shared_articles),
               color = "grey62", alpha = .3, lineend = "round") +
  geom_point(data = nodes, aes(x, y, fill = area_1, size = publications_2001_2025),
             shape = 21, color = "white", stroke = .65, alpha = .97) +
  scale_fill_manual(values = area_palette) +
  scale_linewidth_continuous(range = c(.3, 2), guide = "none") +
  scale_size_continuous(range = c(3.5, 11), guide = "none") +
  coord_equal(clip = "off") +
  labs(title = "Red de coautoría dentro del Departamento",
       subtitle = "Artículos publicados entre 2001 y 2025; tamaño según publicaciones individuales",
       fill = "Área principal", size = "Artículos") +
  theme_void(base_size = 11) +
  guides(fill = "none") +
  theme(plot.title = element_text(size = 15, face = "bold", color = "grey20"),
        plot.subtitle = element_text(size = 10, color = "grey45"),
        plot.margin = margin(20, 35, 20, 35))

area_legend <- data.table(area_1 = areas, y = rev(seq_along(areas)))
p_area_legend <- ggplot(area_legend, aes(1, y)) +
  geom_point(aes(fill = area_1), shape = 21, size = 6.2, color = "white", stroke = .7) +
  geom_text(aes(x = 1.28, label = area_1), hjust = 0, size = 3.2, color = dark_gray) +
  scale_fill_manual(values = area_palette, guide = "none") +
  scale_x_continuous(limits = c(.75, 4.8)) +
  scale_y_continuous(limits = c(.4, length(areas) + .8)) +
  labs(title = "Área temática principal") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(size = 10.5, face = "bold", color = "grey30",
                                  margin = margin(b = 12)))

size_legend <- data.table(articles = c(20, 60, 100), x = 1:3, y = 1)
p_size_legend <- ggplot(size_legend, aes(x, y)) +
  geom_point(aes(size = articles), shape = 21, fill = "grey55", color = "white", stroke = .65) +
  geom_text(aes(y = .48, label = articles), size = 3, color = dark_gray) +
  scale_size_continuous(range = c(3.5, 11), limits = range(nodes$publications_2001_2025), guide = "none") +
  scale_x_continuous(limits = c(.45, 3.55)) +
  scale_y_continuous(limits = c(.2, 1.55)) +
  labs(title = "Artículos") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(size = 10.5, face = "bold", color = "grey30", hjust = .5))

network_top <- p_network + p_area_legend + plot_layout(widths = c(4, 1.7))
network_bottom <- p_size_legend + plot_spacer() + plot_layout(widths = c(4, 1.7))
network_final <- network_top / network_bottom + plot_layout(heights = c(8, 1.15))
save_figure(network_final, "04_red_coautoria_dcp.png", width = 13, height = 9)

p_network_names <- p_network +
  geom_text(data = nodes, aes(x_label, y_label, label = label),
            size = 2.75, color = dark_gray, lineheight = .9)
network_names_top <- p_network_names + p_area_legend + plot_layout(widths = c(4.35, 1.7))
network_names_bottom <- p_size_legend + plot_spacer() + plot_layout(widths = c(4.35, 1.7))
network_names_final <- network_names_top / network_names_bottom + plot_layout(heights = c(8.5, 1.15))
save_figure(network_names_final, "04b_red_coautoria_dcp_nombres.png", width = 14, height = 10)

write_table(nodes[, .(author_id, author_name, label, area_1, publications_2001_2025,
                      collaborators, x, y, x_label, y_label)],
            "13_red_coautoria_nodos.csv")
write_table(edges, "14_red_coautoria_aristas.csv")

recent_verified <- audit[article_year >= 2021 & article_year <= 2025, .N, by = collaboration_verified]
recent_verified[, share := N / sum(N)]
stats_quienes <- data.table(
  indicador = c("docentes_con_funciones_de_investigacion", "docentes_con_cv",
                "docentes_con_scholar", "docentes_con_articulo_2021_2025",
                "mediana_articulos_2021_2025_entre_quienes_publican",
                paste0("coautoria_verificada_", gsub(" ", "_", normalize_person(as.character(recent_verified$collaboration_verified))), "_pct")),
  valor = c(nrow(authors), nrow(publication_authors),
            sum(authors$scholar_available == TRUE, na.rm = TRUE),
            author_profile[cv_available == TRUE & recent_articles > 0, .N],
            median(author_profile[cv_available == TRUE & recent_articles > 0, recent_articles]),
            100 * recent_verified$share)
)
write_table(stats_quienes, "00_indicadores_quienes.csv")
