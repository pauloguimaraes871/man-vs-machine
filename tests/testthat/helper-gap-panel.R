helper_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
                        error = function(e) NA_character_)
root_candidates <- unique(c(
  Sys.getenv("GAP_PANEL_PROJECT_ROOT", unset = ""),
  dirname(dirname(dirname(helper_file))),
  getwd(),
  file.path(getwd(), ".."),
  file.path(getwd(), "..", "..")
))
valid_roots <- root_candidates[
  !is.na(root_candidates) & nzchar(root_candidates) &
    file.exists(file.path(root_candidates, "build_gap_panel.R"))
]
project_root <- valid_roots[1L]

if (is.na(project_root) || !nzchar(project_root)) {
  stop("Could not locate build_gap_panel.R for tests.", call. = FALSE)
}

old_options <- options(gap_panel.run = FALSE)
tryCatch(
  source(file.path(project_root, "build_gap_panel.R"), local = .GlobalEnv),
  finally = options(old_options)
)

daily_panel <- function(dates, values, code = "A") {
  list(
    dates = as.Date(dates),
    values = matrix(as.numeric(values), ncol = 1L,
                    dimnames = list(NULL, code)),
    codes = code
  )
}
