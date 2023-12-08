# library(here)
# library(rutils)

# Scan Quarto files for citations and add them to references.bib -----

rutils:::bbt_write_quarto_bib(
  bib_file = here::here("references.bib"),
  dir = c(""),
  pattern = c("\\.qmd$|\\.tex$"),
  wd = here::here()
)
