# -----------------------------------------------------------------------------
# Comdinheiro FundScreener001: fund-level equity holdings.
#
# See COMDINHEIRO_NOTES.md for the full write-up. Summary: FundScreener001
# hard-caps every response at 1,000 rows (linppag/truncar/pageviews/limit_01
# are all inert), and the cut is not a random sample -- it silently drops the
# largest funds unless a `patrimonio` band is supplied. `filtro` does accept
# a chained `patrimonio>=lo AND patrimonio<hi` condition, so
# comd_fundscreener_bisect() recovers a complete, self-verifying class by
# adaptively splitting on AUM: any response under 1,000 rows is provably
# complete for its band, since the bands are disjoint and half-open.
#
# SHELVED: a full pull across all ANBIMA equity classes and the ~66 months in
# ml_index is on the order of 3,000-4,600 requests; the account has ~1,000/
# month. Nothing in this file executes on source() -- see the examples in
# `if (FALSE)` at the bottom.
# -----------------------------------------------------------------------------

COMD_ENDPOINT <- "https://api.comdinheiro.com.br/v1/ep1/import-data"
COMD_ROW_CAP  <- 1000L

# Credentials live in the pipeline project's .Renviron, not this project's.
comd_load_credentials <- function() {
  if (nzchar(Sys.getenv("COMD_USER")) && nzchar(Sys.getenv("COMD_PASS"))) {
    return(invisible(TRUE))
  }
  hit <- Sys.glob("C:/Users/paulo.fonteles/OneDrive - Sicoob/Renda*/Multifatorial*/pipeline/.Renviron")
  if (!length(hit)) {
    rlang::abort("COMD_USER/COMD_PASS are not set and the pipeline .Renviron was not found.")
  }
  readRenviron(hit[1L])
  if (!nzchar(Sys.getenv("COMD_USER")) || !nzchar(Sys.getenv("COMD_PASS"))) {
    rlang::abort("Loaded a .Renviron but COMD_USER/COMD_PASS are still unset.")
  }
  invisible(TRUE)
}

# JSON3 table parser, lifted from
# Multifatorial 3.0/pipeline/R/load/download_comdinheiro.R (parse_json3_table).
comd_parse_json3_table <- function(tab) {
  stopifnot(is.list(tab), !is.null(tab$lin0))
  header    <- make.unique(unlist(tab$lin0, use.names = FALSE), sep = "_dup")
  lin_names <- grep("^lin\\d+$", names(tab), value = TRUE)
  lin_names <- lin_names[order(as.integer(sub("^lin", "", lin_names)))]
  row_names <- setdiff(lin_names, "lin0")
  if (!length(row_names)) return(data.frame())
  rows_list <- lapply(row_names, function(nm) unlist(tab[[nm]], use.names = FALSE))
  max_len   <- length(header)
  rows_list <- lapply(rows_list, function(v) {
    if (length(v) < max_len) v <- c(v, rep(NA_character_, max_len - length(v)))
    v[seq_len(max_len)]
  })
  mat <- do.call(rbind, rows_list)
  colnames(mat) <- header
  df <- as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
  df[df == ""] <- NA_character_
  df
}

# One POST. Returns a data.frame, or NULL if the API returned no table.
comd_request <- function(rel_url) {
  comd_load_credentials()
  req <- httr2::request(COMD_ENDPOINT) |>
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded",
                       "User-Agent"   = "comdinheiro-r/0.1") |>
    httr2::req_retry(max_tries = 1) |>
    httr2::req_timeout(3600) |>
    httr2::req_body_form(
      username = Sys.getenv("COMD_USER"),
      password = Sys.getenv("COMD_PASS"),
      URL      = rel_url,
      format   = "json3"
    )
  res <- httr2::req_perform(req)
  txt <- httr2::resp_body_string(res, encoding = "UTF-8")
  x   <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (is.null(x$tables$tab0)) return(NULL)
  comd_parse_json3_table(x$tables$tab0)
}

# One FundScreener001 request for a single ANBIMA equity class, optionally
# bounded to a half-open PL band [lo, hi). data_cart/data_rr are dd/mm/yyyy.
comd_fundscreener_band <- function(data_cart, data_rr, cl_anb,
                                   lo = NULL, hi = NULL,
                                   cl_cvm = "FI_CVM FIA") {
  filtro <- "desc_ativo=acoes"
  if (!is.null(lo) && lo > 0)
    filtro <- paste0(filtro, "+AND+patrimonio%3E%3D", format(lo, scientific = FALSE))
  if (!is.null(hi) && is.finite(hi))
    filtro <- paste0(filtro, "+AND+patrimonio%3C",   format(hi, scientific = FALSE))

  rel_url <- paste0(
    "FundScreener001.php?&relat=&data_rr=", data_rr, "&data_cart=", data_cart,
    "&variaveis=data_carteira+cnpj_fundo+nome_fundo+codigo_anbima+ativo+patrimonio",
    "+VOLUME_PAPEL(mes_atual,data_analise,ativo)+compras_valor+vendas_valor+final_valor",
    "&agrupar=&gr_classe=FI&cl_cvm=", gsub(" ", "+", cl_cvm),
    "&cl_anb=", cl_anb,
    "&admin=&gestor=&situacao=f&cotas=7&quali=7&exclu=7&forma=3&bolsa=3",
    "&largura=960&truncar=200&casas=2&salve=&filtro=", filtro, "&join=inner",
    "&overwrite=0&minha_variavel=&transpor=0&salve_obs=",
    "&relat_alias_automatico=cmd_alias_01&flag_tab_carteira=0&periodos=0",
    "&periodicidade=mensal&formato_data=1&linppag=100&ordenar=&limit_01=",
    "&reduza_nome_fundo=1&habilitar_fundos_multiclasse=0&pageviews=1"
  )
  comd_request(rel_url)
}

# Adaptive PL bisection: recovers a complete class by recursing on any band
# that returns exactly COMD_ROW_CAP rows. Disjoint half-open bands guarantee
# no double-counting; a response under the cap is provably complete for its
# band. `on_request` is called before each live request (for logging/quota
# tracking) -- wire it to stop() if you want a hard request budget.
comd_fundscreener_bisect <- function(data_cart, data_rr, cl_anb,
                                     on_request = function(lo, hi) invisible(NULL),
                                     max_depth = 15L) {
  out <- list()
  walk <- function(lo, hi, depth) {
    if (depth > max_depth) {
      rlang::abort(sprintf("Bisection depth limit reached at [%g, %g) -- band will not resolve.", lo, hi))
    }
    on_request(lo, hi)
    df <- comd_fundscreener_band(data_cart, data_rr, cl_anb, lo, hi)
    n  <- if (is.null(df)) 0L else nrow(df)
    if (n == 0L) return(invisible())
    if (n < COMD_ROW_CAP) {
      out[[length(out) + 1L]] <<- df
      return(invisible())
    }
    mid <- if (is.finite(hi)) exp((log(max(lo, 1e5)) + log(hi)) / 2) else max(lo * 10, 1e9)
    if (!is.finite(mid) || mid <= lo || (is.finite(hi) && mid >= hi)) {
      rlang::warn(sprintf("Cannot split [%g, %g) further; keeping truncated response.", lo, hi))
      out[[length(out) + 1L]] <<- df
      return(invisible())
    }
    walk(lo, mid, depth + 1L)
    walk(mid, hi, depth + 1L)
  }
  walk(0, Inf, 0L)
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

# ------------------------------- examples ------------------------------- #
# Nothing above this line fires a request. These are the calls used to
# validate the approach in COMDINHEIRO_NOTES.md; wrapped in `if (FALSE)` so
# sourcing this file never touches the network or the request budget.
if (FALSE) {

  # Single band, no bisection -- capped at 1,000 rows for a large class.
  df <- comd_fundscreener_band(data_cart = "01/01/2021", data_rr = "26/05/2021",
                               cl_anb = "ACIE")

  # Full class via bisection -- 9 requests, 3,139 rows, 234 funds for
  # cl_anb=ACIE at 2021-01 (vs. 1,000 rows / 54 funds from the call above).
  full <- comd_fundscreener_bisect(
    data_cart = "01/01/2021", data_rr = "26/05/2021", cl_anb = "ACIE",
    on_request = function(lo, hi) cat(sprintf("requesting [%g, %g)\n", lo, hi))
  )
}
