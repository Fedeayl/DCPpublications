args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
setwd(root)

required <- c("data.table", "ggplot2", "scales", "patchwork", "cld3", "stringi")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Faltan paquetes de R: ", paste(missing, collapse = ", "))

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
old_outputs <- c(list.files("outputs/tables", full.names = TRUE), list.files("outputs/figures", full.names = TRUE))
if (length(old_outputs)) invisible(file.remove(old_outputs))

source("R/00_helpers.R", encoding = "UTF-8")
source("R/01_quienes_publican.R", encoding = "UTF-8")
source("R/02_cuanto_publican.R", encoding = "UTF-8")
source("R/03_donde_publican.R", encoding = "UTF-8")
source("R/04_coautoria.R", encoding = "UTF-8")
source("R/99_validate.R", encoding = "UTF-8")
capture.output(sessionInfo(), file = "outputs/session_info.txt")

cat("Análisis reproducido correctamente.\n")
