library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

main_color <- "#3F6F8F"
second_color <- "#7FA5B9"
third_color <- "#6F9E86"
light_color <- "#DCE8EF"
dark_gray <- "grey25"

theme_blog <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = .35),
    axis.text = element_text(color = "grey35"),
    axis.title = element_text(color = "grey35"),
    plot.title = element_text(size = 13, face = "bold", color = "grey20", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "grey45", margin = margin(b = 6)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    plot.margin = margin(8, 10, 8, 8)
  )

period_levels <- c("2001–05", "2006–10", "2011–15", "2016–20", "2021–25")

period_5 <- function(year) {
  factor(
    fcase(
      year >= 2001 & year <= 2005, "2001–05",
      year >= 2006 & year <= 2010, "2006–10",
      year >= 2011 & year <= 2015, "2011–15",
      year >= 2016 & year <= 2020, "2016–20",
      year >= 2021 & year <= 2025, "2021–25",
      default = NA_character_
    ),
    levels = period_levels
  )
}

read_analysis_data <- function() {
  authors_all <- fread("data/authors.csv", na.strings = c("", "NA"))
  articles_all <- fread("data/articles.csv", na.strings = c("", "NA"))
  author_article_all <- fread("data/author_article.csv", na.strings = c("", "NA"))
  journals_all <- fread("data/journals.csv", na.strings = c("", "NA"))
  publications_all <- fread("data/publications_academic.csv", na.strings = c("", "NA"))

  # El universo analítico se reconstruye en memoria a partir de los cinco CSV fuente.
  authors <- authors_all[dt_2026 %in% c(0, 1)]
  publication_authors <- authors[cv_available == TRUE]
  author_article <- unique(
    author_article_all[author_id %in% publication_authors$author_id],
    by = c("author_id", "article_id")
  )
  author_article[, article_weight_analysis := 1 / .N, by = article_id]
  articles <- articles_all[article_id %in% unique(author_article$article_id)]
  journals <- journals_all[journal_id %in% unique(articles$journal_id)]

  normalize_local <- function(x) {
    x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
    trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(x))))
  }
  analysis_names <- normalize_local(publication_authors$author_name)
  publications <- publications_all[vapply(strsplit(dcp_authors, " \\| "), function(parts) {
    any(normalize_local(parts) %in% analysis_names)
  }, logical(1))]

  list(authors = authors, publication_authors = publication_authors,
       authors_all = authors_all, articles = articles, articles_all = articles_all,
       author_article = author_article, author_article_all = author_article_all,
       journals = journals, journals_all = journals_all,
       publications = publications, publications_all = publications_all)
}

normalize_person <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

split_author_parts <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  parts <- trimws(strsplit(x, "\\|", perl = TRUE)[[1]])
  unique(parts[nzchar(parts)])
}

match_person_part <- function(part, people) {
  p <- normalize_person(part)
  if (!nzchar(p)) return(NA_character_)
  pt <- strsplit(p, " ", fixed = TRUE)[[1]]
  aliases <- list(
    AUTH010 = c("de giorgi", "giorgi"),
    AUTH014 = c("julian gonzalez", "gonzalez scandizzi", "scandizzi"),
    AUTH023 = c("perez bentancur"),
    AUTH026 = c("rocha carpiuc")
  )
  for (id in intersect(names(aliases), people$author_id)) {
    if (any(vapply(aliases[[id]], function(a) all(strsplit(a, " ", fixed = TRUE)[[1]] %in% pt), logical(1)))) return(id)
  }
  matches <- character()
  for (i in seq_len(nrow(people))) {
    a <- normalize_person(people$author_name[i])
    at <- strsplit(a, " ", fixed = TRUE)[[1]]
    if (identical(sort(pt), sort(at))) {
      matches <- c(matches, people$author_id[i]); next
    }
    surname <- tail(at, 1)
    if (!surname %in% pt) next
    remaining <- setdiff(pt, surname)
    given <- head(at, -1)
    if (!length(remaining)) {
      matches <- c(matches, people$author_id[i]); next
    }
    given_match <- any(remaining %in% given) || any(vapply(remaining[nchar(remaining) == 1], function(initial) {
      any(startsWith(given, initial))
    }, logical(1)))
    if (given_match) matches <- c(matches, people$author_id[i])
  }
  matches <- unique(matches)
  if (length(matches) == 1) matches else NA_character_
}

build_authorship_audit <- function(articles, links_all, authors_all, analysis_ids) {
  canonical <- links_all[article_id %in% articles$article_id, .(canonical_dcp_ids = list(unique(author_id))), by = article_id]
  out <- merge(articles[, .(article_id, article_title, article_year, authors_full,
                            n_authors_total_original = n_authors_total,
                            solo_author_original = solo_author,
                            n_dcp_authors_original = n_dcp_authors)], canonical, by = "article_id", all.x = TRUE)
  records <- lapply(seq_len(nrow(out)), function(i) {
    parts <- split_author_parts(out$authors_full[i])
    matched <- vapply(parts, match_person_part, character(1), people = authors_all)
    matched_ids <- unique(na.omit(c(unlist(out$canonical_dcp_ids[i]), matched)))
    analysis_matched <- intersect(matched_ids, analysis_ids)
    external_parts <- unique(normalize_person(parts[is.na(matched)]))
    external_parts <- setdiff(external_parts, c("autor", "autora", "coautor", "coautora", "coautores", "coautoras"))
    external_parts <- external_parts[nzchar(external_parts)]
    n_dcp_current <- length(matched_ids)
    category <- if (length(matched_ids) > 1 && length(external_parts) > 0) {
      "Coautoría mixta"
    } else if (length(matched_ids) > 1) {
      "Coautoría interna al DCP"
    } else if (length(external_parts) > 0) {
      "Coautoría externa al DCP"
    } else {
      "Autoría individual"
    }
    data.table(
      article_id = out$article_id[i],
      n_analysis_authors_verified = length(analysis_matched),
      analysis_author_ids_verified = paste(sort(analysis_matched), collapse = " | "),
      n_dcp_authors_verified = length(matched_ids),
      dcp_author_ids_verified = paste(sort(matched_ids), collapse = " | "),
      n_external_authors_verified = length(external_parts),
      external_author_names = paste(external_parts, collapse = " | "),
      collaboration_verified = category
    )
  })
  audit <- cbind(out[, setdiff(names(out), "canonical_dcp_ids"), with = FALSE],
                 rbindlist(records)[, -"article_id"])
  audit
}

write_table <- function(x, name) {
  fwrite(x, file.path("outputs/tables", name), na = "")
}

save_figure <- function(plot, name, width = 9, height = 6) {
  ggsave(file.path("outputs/figures", name), plot, width = width, height = height,
         dpi = 320, bg = "white")
}
