#' Carrega dados da redesim
#'
#' @param ano ano de consulta. Por padrão, 2021.
#' @param mes mes de consulta. Por padrão, março (mês 3).
#' @param uf UF de consulta. Por padrão, baixa de todo o Brasil.
#'
#' @export
redesim_fetch <- function(ano = 2021, mes = 3, uf = "") {
  f_xlsx <- fs::file_temp("redesim", ext = ".xlsx")
  query <- list(mes = mes, ano = ano)
  if (uf != "") uf <- paste0("/", uf)
  u <- paste0(
    "https://estatistica.redesim.gov.br/tempos-abertura-redesim",
    "/exportar/solicitacoes", uf
  )
  r <- httr::GET(
    u, query = query, httr::write_disk(f_xlsx, TRUE),
    httr::config(ssl_verifypeer = FALSE)
  )
  f_xlsx %>%
    readxl::read_excel(skip = 1, guess_max = 1e5) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("^data_|^dt_"), lubridate::dmy),
      dplyr::across(dplyr::matches("^hh_"), lubridate::hms)
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("cnae"),
      names_to = "id_cnae",
      values_to = "cnae"
    ) %>%
    dplyr::group_by(serial) %>%
    tidyr::nest(cnae = c(id_cnae, cnae)) %>%
    dplyr::ungroup()
}
