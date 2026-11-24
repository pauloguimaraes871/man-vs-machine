variable_definition_dictionary <- function() {
  data.frame(
    signal = c(
      "sharpe_3m", "prc_ret_3m_1m", "alpha_ibov_6m",
      "alpha_tstat_ibov_6m", "acceleration_6m", "res_mom_6m",
      "prc_ret_6m_1m", "sharpe_6m", "sharpe_6m_inv1m",
      "sharpe_6m_c1_median", "sharpe_12m", "prc_ret_12m_1m",
      "prc_highprice_12m",
      "ocf_me", "fcf_me", "assets_me", "ocf_me_c1_adj",
      "fcf_me_c1_adj", "be_me_c1_adj", "be_me", "sales_me", "ni_me",
      "fcf_ev", "ebitda_ev", "bev_ev", "div_me", "div_me_med_36m",
      "ni_me_sur_5y", "ocf_me_sur_5y", "div_me_sur_5y",
      "gross_mrg_12m", "ebit_mrg_12m", "net_mrg_12m", "ocf_sales_3m",
      "roe_3m", "roe_12m", "roa_3m", "roa_12m", "roic_12m",
      "asset_turnover_12m", "gp_at_12m", "ocf_at_12m", "fcf_be_12m",
      "asset_turnover_12m_c1_adj", "roe_12m_c1_adj",
      "roe_3m_gr1y", "roa_3m_gr1y", "roe_12m_gr3y", "roa_12m_gr3y",
      "gp_at_12m_gr3y", "ocf_at_12m_gr3y", "gross_mrg_12m_gr3y",
      "roe_12m_gr3y_c1_adj", "ni_incr_36m", "sales_3m_gr1y_sur3y",
      "net_inc_3m_gr1y_sur3y", "roe_3m_gr1y_sur3y",
      "roe_12m_gr3y_sur3y",
      "qmj", "f_score",
      "assets_gr1y", "assets_gr3y", "be_gr1y", "be_gr3y", "noa_gr1y",
      "noa_gr3y", "capex_12m_gr1y", "nncoa_gr1y", "cowc_gr1y", "eqis",
      "total_debt_gr1y", "total_debt_gr3y",
      "oaccruals_at", "oaccruals_ni", "cf_accruals_ratio", "noa_at",
      "cash_at", "tangibility", "z_score", "net_debt_be",
      "net_debt_ebitda", "roe_vol_36m", "cf_vol_36m",
      "beta_ibov_60m", "corr_ibov_60m", "ret_vol_36m", "ret_vol_12m",
      "idio_vol_12m",
      "ret_60m_12m", "ret_36m_12m",
      "ret_21d", "max_ret_21d", "max_5ret_21d", "idio_vol_21d",
      "skewness_21d",
      "log_mkt_cap", "log_mkt_cap_c1_adj", "log_mean_volfin_6m",
      "turnover_6m",
      "short_int_ratio", "short_int_ratio_sur3y", "days_to_cover",
      "seas_ret_1m",
      "pro_cyclicals", "bond_proxies", "industrials", "defensives",
      "commodities"
    ),
    definition = c(
      "Mean daily return divided by daily return volatility over the trailing three months.",
      "Compounded return over months t-3 to t-1, excluding the most recent month.",
      "Intercept from a daily market-model regression on IBOV returns over the trailing six months.",
      "t-statistic of the trailing six-month IBOV market-model intercept.",
      "Current six-month return Sharpe measure minus its value six months earlier.",
      "Standardized sum of residuals from a six-month daily-return regression on IBOV.",
      "Compounded return over months t-6 to t-1, excluding the most recent month.",
      "Mean daily return divided by daily return volatility over the trailing six months.",
      "Six-month mean-to-volatility ratio after reversing the sign of returns in the most recent month.",
      "Cross-sectional median of the six-month Sharpe measure within the level-1 sector.",
      "Mean daily return divided by daily return volatility over the trailing 12 months.",
      "Compounded return over months t-12 to t-1, excluding the most recent month.",
      "Current adjusted price divided by the maximum adjusted price over the trailing 12 months.",
      "Trailing 12-month operating cash flow divided by market capitalization.",
      "Trailing 12-month free cash flow divided by market capitalization.",
      "Total assets divided by market capitalization.",
      "Operating-cash-flow yield standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Free-cash-flow yield standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Book-to-market standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Book equity divided by market capitalization.",
      "Trailing 12-month sales divided by market capitalization.",
      "Trailing 12-month net income divided by market capitalization.",
      "Trailing 12-month free cash flow divided by enterprise value.",
      "Trailing 12-month EBITDA divided by enterprise value.",
      "Book enterprise value divided by market enterprise value.",
      "Trailing 12-month dividends per adjusted share divided by adjusted price.",
      "Median dividend yield over the trailing 36 months.",
      "Current earnings yield minus its trailing five-year mean, scaled by its trailing five-year standard deviation.",
      "Current operating-cash-flow yield minus its trailing five-year mean, scaled by its trailing five-year standard deviation.",
      "Current dividend yield minus its trailing five-year mean, scaled by its trailing five-year standard deviation.",
      "Trailing 12-month gross profit divided by trailing 12-month sales.",
      "Trailing 12-month EBIT divided by trailing 12-month sales.",
      "Trailing 12-month net income divided by trailing 12-month sales.",
      "Annualized quarterly operating cash flow divided by quarterly sales.",
      "Annualized quarterly net income divided by current book equity.",
      "Trailing 12-month net income divided by average book equity over the trailing year.",
      "Annualized quarterly net income divided by total assets.",
      "Trailing 12-month net income divided by average total assets over the trailing year.",
      "After-tax trailing 12-month EBIT divided by average invested capital over the trailing year.",
      "Trailing 12-month sales divided by average total assets over the trailing year.",
      "Trailing 12-month gross profit divided by average total assets over the trailing year.",
      "Trailing 12-month operating cash flow divided by average total assets over the trailing year.",
      "Trailing 12-month free cash flow divided by average book equity over the trailing year.",
      "Asset turnover standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Trailing return on equity standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Compound monthly growth rate in quarterly return on equity between the current value and its 12-month lag.",
      "Compound monthly growth rate in quarterly return on assets between the current value and its 12-month lag.",
      "Compound monthly growth rate in trailing return on equity between the current value and its 36-month lag.",
      "Compound monthly growth rate in trailing return on assets between the current value and its 36-month lag.",
      "Compound monthly growth rate in gross profitability to assets between the current value and its 36-month lag.",
      "Compound monthly growth rate in operating cash flow to assets between the current value and its 36-month lag.",
      "Compound monthly growth rate in gross margin between the current value and its 36-month lag.",
      "The 36-month compound monthly ROE growth rate standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Number of positive quarterly net-income growth observations over the trailing 36 months.",
      "Current one-year quarterly-sales growth minus its trailing three-year mean, scaled by its trailing standard deviation.",
      "Current one-year quarterly-net-income growth minus its trailing three-year mean, scaled by its trailing standard deviation.",
      "Current one-year quarterly-ROE growth minus its trailing three-year mean, scaled by its trailing standard deviation.",
      "Current three-year trailing-ROE growth minus its trailing three-year mean, scaled by its trailing standard deviation.",
      "Composite quality-minus-junk score combining profitability, growth, safety, and payout information.",
      "Piotroski score: count of nine profitability, financing, and operating-efficiency conditions.",
      "Compound monthly growth rate in total assets between the current value and its 12-month lag.",
      "Compound monthly growth rate in total assets between the current value and its 36-month lag.",
      "Compound monthly growth rate in book equity between the current value and its 12-month lag.",
      "Compound monthly growth rate in book equity between the current value and its 36-month lag.",
      "Compound monthly growth rate in net operating assets between the current value and its 12-month lag.",
      "Compound monthly growth rate in net operating assets between the current value and its 36-month lag.",
      "Compound monthly growth rate in trailing capital expenditures between the current value and its 12-month lag.",
      "Compound monthly growth rate in net non-current operating assets between the current value and its 12-month lag.",
      "Compound monthly growth rate in current operating working capital between the current value and its 12-month lag.",
      "Log market-cap growth net of the trailing 12-month stock return, a composite equity-issuance measure.",
      "Compound monthly growth rate in total debt between the current value and its 12-month lag.",
      "Compound monthly growth rate in total debt between the current value and its 36-month lag.",
      "Change in current and non-current operating working capital, scaled by total assets.",
      "Operating accruals divided by positive trailing 12-month net income.",
      "Trailing net income minus free cash flow, scaled by average net operating assets.",
      "Net operating assets divided by total assets.",
      "Cash and short-term investments divided by total assets.",
      "Current assets plus property, plant, and equipment, divided by total assets.",
      "Modified Altman Z-score based on working capital, profitability, EBIT, and book leverage.",
      "Net debt divided by book equity.",
      "Net debt divided by trailing 12-month EBITDA.",
      "Standard deviation of quarterly return on equity over the trailing 36 months.",
      "Standard deviation of quarterly operating-cash-flow margin over the trailing 36 months.",
      "Market beta from daily stock and IBOV returns over the trailing 60 months.",
      "Pearson correlation between daily stock and IBOV returns over the trailing 60 months.",
      "Standard deviation of daily returns over the trailing 36 months.",
      "Standard deviation of daily returns over the trailing 12 months.",
      "Residual return volatility from a daily market-model regression on IBOV over the trailing 12 months.",
      "Compounded return from month t-60 through t-12, excluding the most recent year.",
      "Compounded return from month t-36 through t-12, excluding the most recent year.",
      "Compounded return over the most recent 21 trading days.",
      "Maximum daily return over the most recent 21 trading days.",
      "Mean of the five highest daily returns over the most recent 21 trading days.",
      "Residual return volatility from a daily market-model regression on IBOV over the most recent 21 trading days.",
      "Skewness of daily returns over the most recent 21 trading days.",
      "Natural logarithm of market capitalization.",
      "Log market capitalization standardized relative to the contemporaneous level-1 sector mean and standard deviation.",
      "Natural logarithm of average inflation-adjusted daily financial volume over the trailing six months.",
      "Average inflation-adjusted daily financial volume divided by average market capitalization over the trailing six months.",
      "Shares on loan divided by shares outstanding.",
      "Current short-interest ratio minus its trailing three-year mean, scaled by its trailing standard deviation.",
      "Shares on loan divided by average daily shares traded over the trailing three months.",
      "Seasonal mean-to-volatility measure of same-calendar-window one-month returns over the trailing five years.",
      "Indicator for membership in the pro-cyclical sector group.",
      "Indicator for membership in the bond-proxy sector group.",
      "Indicator for membership in the industrial sector group.",
      "Indicator for membership in the defensive sector group.",
      "Indicator for membership in the commodity-related sector group."
    ),
    source = c(
      "This study; Jegadeesh and Titman (1993)", "Jegadeesh and Titman (1993)",
      "This study; Fama and MacBeth (1973)", "This study; Fama and MacBeth (1973)",
      "Gettleman and Marks (2006)", "Blitz, Huij, and Martens (2011)",
      "Jegadeesh and Titman (1993)", "This study; Jegadeesh and Titman (1993)",
      "This study; Jegadeesh and Titman (1993)", "This study; Moskowitz and Grinblatt (1999)",
      "This study; Jegadeesh and Titman (1993)", "Jegadeesh and Titman (1993)",
      "George and Hwang (2004)",
      "Desai, Rajgopal, and Venkatachalam (2004)", "Lakonishok, Shleifer, and Vishny (1994)",
      "Fama and French (1992)", "Asness, Porter, and Stevens (2000)",
      "Asness, Porter, and Stevens (2000)", "Asness, Porter, and Stevens (2000)",
      "Rosenberg, Reid, and Lanstein (1985)", "Barbee, Mukherji, and Raines (1996)",
      "Basu (1983)", "Lakonishok, Shleifer, and Vishny (1994)",
      "Loughran and Wellman (2011)", "Loughran and Wellman (2011)",
      "Litzenberger and Ramaswamy (1979)", "This study; Litzenberger and Ramaswamy (1979)",
      "This study; Basu (1983)", "This study; Desai et al. (2004)",
      "This study; Litzenberger and Ramaswamy (1979)",
      "Soliman (2008)", "Soliman (2008)", "Soliman (2008)",
      "Bouchard et al. (2019)", "Hou, Xue, and Zhang (2015)",
      "Haugen and Baker (1996)", "Balakrishnan, Bartov, and Faurel (2010)",
      "Balakrishnan, Bartov, and Faurel (2010)", "Brown and Rowe (2007)",
      "Haugen and Baker (1996)", "Novy-Marx (2013)", "Bouchard et al. (2019)",
      "Bouchard et al. (2019)", "Soliman (2008)", "Asness, Porter, and Stevens (2000)",
      "Asness, Frazzini, and Pedersen (2018)", "Asness, Frazzini, and Pedersen (2018)",
      "Asness, Frazzini, and Pedersen (2018)", "Asness, Frazzini, and Pedersen (2018)",
      "Asness, Frazzini, and Pedersen (2018)", "Asness, Frazzini, and Pedersen (2018)",
      "Asness, Frazzini, and Pedersen (2018)", "Asness, Porter, and Stevens (2000)",
      "Barth, Elliott, and Finn (1999)", "Jegadeesh and Livnat (2006)",
      "Foster, Olsen, and Shevlin (1984)", "Foster, Olsen, and Shevlin (1984)",
      "Asness, Frazzini, and Pedersen (2018)",
      "Asness, Frazzini, and Pedersen (2018)", "Piotroski (2000)",
      "Cooper, Gulen, and Schill (2008)", "Cooper, Gulen, and Schill (2008)",
      "Richardson et al. (2005)", "Richardson et al. (2005)",
      "Hirshleifer et al. (2004)", "Hirshleifer et al. (2004)",
      "Anderson and Garcia-Feijoo (2006)", "Richardson et al. (2005)",
      "Richardson et al. (2005)", "Daniel and Titman (2006)",
      "Lyandres, Sun, and Zhang (2008)", "Lyandres, Sun, and Zhang (2008)",
      "Sloan (1996)", "Hafzalla, Lundholm, and Van Winkle (2011)",
      "Richardson et al. (2005)", "Hirshleifer et al. (2004)",
      "Palazzo (2012)", "Hahn and Lee (2009)", "Dichev (1998)",
      "Penman, Richardson, and Tuna (2007)", "Penman, Richardson, and Tuna (2007)",
      "Francis et al. (2004)", "Huang (2009)",
      "Fama and MacBeth (1973)", "Asness et al. (2020)",
      "Ang et al. (2006)", "Ang et al. (2006)", "Ali, Hwang, and Trombley (2003)",
      "De Bondt and Thaler (1985)", "De Bondt and Thaler (1985)",
      "Jegadeesh (1990)", "Bali, Cakici, and Whitelaw (2011)",
      "Bali et al. (2017)", "Ang et al. (2006)", "Bali, Engle, and Murray (2016)",
      "Banz (1981)", "Asness, Porter, and Stevens (2000)",
      "Brennan, Chordia, and Subrahmanyam (1998)", "Datar, Naik, and Radcliffe (1998)",
      "Hanauer, Lesnevski, and Smajlbegovic (2022)",
      "Hanauer, Lesnevski, and Smajlbegovic (2022)",
      "Hanauer, Lesnevski, and Smajlbegovic (2022)",
      "Heston and Sadka (2008)",
      "Gu, Kelly, and Xiu (2020); Hanauer and Kalsbach (2023)",
      "Gu, Kelly, and Xiu (2020); Hanauer and Kalsbach (2023)",
      "Gu, Kelly, and Xiu (2020); Hanauer and Kalsbach (2023)",
      "Gu, Kelly, and Xiu (2020); Hanauer and Kalsbach (2023)",
      "Gu, Kelly, and Xiu (2020); Hanauer and Kalsbach (2023)"
    ),
    stringsAsFactors = FALSE
  )
}

build_variable_definition_data <- function(chosen_signals_and_positions,
                                           signal_themes_data) {
  dictionary <- variable_definition_dictionary()

  if (!is.character(chosen_signals_and_positions) ||
      is.null(names(chosen_signals_and_positions)) ||
      anyNA(names(chosen_signals_and_positions)) ||
      anyDuplicated(names(chosen_signals_and_positions))) {
    rlang::abort("chosen_signals_and_positions must be a uniquely named character vector.")
  }
  if (!all(chosen_signals_and_positions %in% c("long", "short", "force"))) {
    rlang::abort("Signal positions must be long, short, or force.")
  }
  required_theme_columns <- c("tickers", "theme")
  if (!is.data.frame(signal_themes_data) ||
      !all(required_theme_columns %in% names(signal_themes_data))) {
    rlang::abort("signal_themes_data must contain tickers and theme columns.")
  }

  selected_signals <- names(chosen_signals_and_positions)
  if (!setequal(dictionary$signal, selected_signals)) {
    missing_dictionary <- setdiff(selected_signals, dictionary$signal)
    stale_dictionary <- setdiff(dictionary$signal, selected_signals)
    rlang::abort(paste0(
      "Variable dictionary and selected feature universe differ. Missing: ",
      paste(missing_dictionary, collapse = ", "), "; stale: ",
      paste(stale_dictionary, collapse = ", "), "."
    ))
  }

  positions <- data.frame(
    signal = selected_signals,
    position = unname(chosen_signals_and_positions),
    stringsAsFactors = FALSE
  )
  positions$corrected_signal <- ifelse(
    positions$position == "short",
    paste0("low_", positions$signal),
    positions$signal
  )

  themes <- unique(signal_themes_data[required_theme_columns])
  if (anyDuplicated(themes$tickers)) {
    rlang::abort("Each corrected signal must map to exactly one feature-importance theme.")
  }

  output <- dictionary |>
    dplyr::left_join(positions, by = "signal", relationship = "one-to-one") |>
    dplyr::left_join(
      themes,
      by = c("corrected_signal" = "tickers"),
      relationship = "many-to-one"
    )

  if (nrow(output) != 108L || anyNA(output$theme) || anyDuplicated(output$signal)) {
    rlang::abort("The current feature universe did not produce 108 unique, themed definitions.")
  }
  output
}

escape_variable_table_tex <- function(x) {
  slash <- intToUtf8(92)
  x <- gsub(slash, paste0(slash, "textbackslash{}"), x, fixed = TRUE)
  x <- gsub("&", paste0(slash, "&"), x, fixed = TRUE)
  x <- gsub("%", paste0(slash, "%"), x, fixed = TRUE)
  x <- gsub("#", paste0(slash, "#"), x, fixed = TRUE)
  x <- gsub("_", paste0(slash, "_"), x, fixed = TRUE)
  x
}

render_variable_definitions_table <- function(variable_data) {
  required_columns <- c("signal", "definition", "source", "position", "theme")
  if (!is.data.frame(variable_data) ||
      !all(required_columns %in% names(variable_data))) {
    rlang::abort(paste0(
      "variable_data must contain: ", paste(required_columns, collapse = ", "), "."
    ))
  }

  theme_order <- c(
    "momentum", "value", "profitability", "growth", "quality", "investment",
    "accruals", "low_fin_risk", "low_mkt_risk", "trading_frictions",
    "lottery_demand", "lt_reversal", "seasonality", "sentiment", "others"
  )
  theme_labels <- c(
    momentum = "Momentum", value = "Value", profitability = "Profitability",
    growth = "Growth", quality = "Quality", investment = "Investment",
    accruals = "Accruals", low_fin_risk = "Low financial risk",
    low_mkt_risk = "Low market risk", trading_frictions = "Trading frictions",
    lottery_demand = "Lottery demand", lt_reversal = "Long-term reversal",
    seasonality = "Seasonality", sentiment = "Short-sale sentiment",
    others = "Sector controls"
  )
  if (!setequal(unique(variable_data$theme), theme_order)) {
    rlang::abort("The variable-definition themes do not match the current manuscript taxonomy.")
  }

  direction_labels <- c(long = "High", short = "Low", force = "Control")
  slash <- intToUtf8(92)
  row_end <- paste0(slash, slash)
  panel_rows <- unlist(lapply(seq_along(theme_order), function(i) {
    theme <- theme_order[[i]]
    rows <- variable_data[variable_data$theme == theme, , drop = FALSE]
    rows <- rows[order(rows$signal), , drop = FALSE]
    heading <- paste0(
      slash, "multicolumn{4}{@{}l}{", slash, "textit{Panel ",
      LETTERS[[i]], ". ", theme_labels[[theme]], "}} ", row_end
    )
    body <- vapply(seq_len(nrow(rows)), function(j) {
      paste0(
        slash, "texttt{", escape_variable_table_tex(rows$signal[[j]]), "} & ",
        escape_variable_table_tex(rows$definition[[j]]), " & ",
        escape_variable_table_tex(rows$source[[j]]), " & ",
        direction_labels[[rows$position[[j]]]], " ", row_end
      )
    }, character(1))
    c(heading, body, paste0(slash, "hline"))
  }), use.names = FALSE)

  c(
    "\\begin{landscape}",
    "\\begingroup",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{1.08}",
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{3.5pt}",
    "% The checked landscape layout deliberately uses the physical margins for legibility.",
    "\\hfuzz=100pt",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{4.1cm}>{\\raggedright\\arraybackslash}p{10.4cm}>{\\raggedright\\arraybackslash}p{6.1cm}>{\\centering\\arraybackslash}p{1.2cm}}",
    "\\caption[Variable definitions]{\\footnotesize \\textbf{: Variable definitions.} Characteristics are grouped by the themes used in the feature-importance analysis. The table gives the implemented definition, closest conceptual source, and signal orientation after screening.} \\label{vardefinitions} \\\\",
    "\\hline",
    "Signal & Definition & Closest source & Direction \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\multicolumn{4}{l}{\\footnotesize \\textit{Variable definitions (continued)}} \\\\",
    "\\hline",
    "Signal & Definition & Closest source & Direction \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\multicolumn{4}{r}{\\footnotesize Continued on next page} \\\\",
    "\\endfoot",
    "\\hline",
    "\\endlastfoot",
    panel_rows,
    paste0(
      "\\multicolumn{4}{@{}p{0.98\\linewidth}@{}}{\\textit{Notes:} Definitions are reconstructed from the point-in-time feature pipeline and matched to the nearest published characteristic in \\citet{jensenkellypedersen2023}, \\citet{gukellyxiu2020}, and \\citet{hanauerkalsbach2023}. The cited study is therefore a conceptual source when the Brazilian implementation changes the horizon, benchmark, or scaling. Suffix c1\\_adj denotes contemporaneous level-1-sector standardization; gr1y and gr3y denote compound monthly growth rates over 12 and 36 months; and sur3y or sur\\_5y denotes a current value minus its trailing mean, divided by its trailing standard deviation. Direction reports the orientation used after signal screening: High retains the raw characteristic, Low multiplies it by -1, and Control denotes an un-oriented sector indicator. Accounting quantities use the most recently available point-in-time observation.} ",
      row_end
    ),
    "\\end{longtable}",
    "\\endgroup",
    "\\end{landscape}"
  )
}

write_variable_definitions_table <- function(variable_data, path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("path must be one non-empty character value.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(render_variable_definitions_table(variable_data), path, useBytes = TRUE)
  invisible(path)
}
