## code to prepare `redesim` dataset goes here

library(magrittr)

combinacoes <- purrr::cross_df(list(ano = 2019:2021, mes = 1:12)) %>%
  dplyr::arrange(ano) %>%
  dplyr::filter(mes <= 3 | ano != 2021) %>%
  purrr::transpose()

safe <- purrr::safely(redesim::redesim_fetch)
progressr::with_progress({
  p <- progressr::progressor(length(combinacoes))
  res <- purrr::map(combinacoes, ~{
    p()
    safe(.x$ano, .x$mes)
  })
})

redesim <- res %>%
  purrr::set_names(purrr::map_chr(combinacoes, ~sprintf("%s_%02d", .x$ano, .x$mes))) %>%
  purrr::map_dfr("result", .id = "ano_mes")

readr::write_rds(redesim, "data-raw/redesim.rds", compress = "xz")

# usethis::use_data(redesim, overwrite = TRUE)

