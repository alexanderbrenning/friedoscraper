library("rvest")
library("stringr")
library("purrr")
library("openxlsx")
source("friedolin-functions.R", encoding = "UTF-8")

# Overwrite existing files?
OVERWRITE <- FALSE

# Save course catalogs in this folder:
out_folder <- "output"

# Get URLs of module catalogs and the corresponding file names
# from this file:
catalogs <- openxlsx::read.xlsx("Modulkataloge.xlsx")

for (i in 1:nrow(catalogs)) {
  fnm <- file.path(out_folder, paste0(catalogs$Name[i], ".xlsx"))
  if (file.exists(fnm) & !OVERWRITE) {
    cat("File", fnm, "already exists, skipping it...\n")
  } else {
    cat("Processing ", catalogs$Name[i], "...\n", sep = "")
    read_module_catalog(catalogs$URL[i], filename = fnm)
  }
}
