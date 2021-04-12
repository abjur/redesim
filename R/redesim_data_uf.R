#' Carrega dados da redesim
#'
#' @param ano ano(s) de consulta. Por padrão, 2021.
#' @param mes mes(es) de consulta. Por padrão, março (mês 3).
#' @param uf UF de consulta. Por padrão, baixa de todo o Brasil.
#'
#' @export
redesim_fetch <- function(ano = 2021, mes = 3, uf = "") {
  dt_atual <- lubridate::floor_date(Sys.Date(), "month")
  stopifnot(
    all(ano %in% c(2019:(lubridate::year(dt_atual)))),
    mes %in% c(1:12)
  )
  purrr::cross_df(list(ano = ano, mes = mes, uf = uf)) %>%
    dplyr::mutate(ano_mes = sprintf("%s-%02d", ano, mes)) %>%
    dplyr::filter(as.Date(paste0(ano_mes, "-01")) < dt_atual) %>%
    dplyr::mutate(ano_mes_uf = paste0(ano_mes, "_", uf)) %>%
    dplyr::arrange(ano, mes, uf) %>%
    purrr::transpose(.names = .[["ano_mes_uf"]]) %>%
    purrr::map_dfr(
      ~redesim_fetch_one(.x$ano, .x$mes, .x$uf),
      .id = "ano_mes_uf"
    )
}

redesim_fetch_one <- function(ano, mes, uf) {
  uf_lab <- ifelse(uf == "", "Brasil", uf)
  message(sprintf("Ano: %s, mes: %s, uf: %s", ano, mes, uf_lab))
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
