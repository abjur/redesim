## code to prepare `redesim_min` dataset goes here

library(magrittr)

redesim_min <- redesim %>%
  dplyr::select(
    ano_mes,
    dplyr::starts_with("qtde_"),
    municipio, uf
  ) %>%
  dplyr::mutate(
    ano_mes = lubridate::ymd(paste0(ano_mes, "-01")),
    uf = factor(uf),
    municipio = factor(municipio)
  )

usethis::use_data(redesim_min, overwrite = TRUE, compress = "xz")
