args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  sub("^--file=", "", file_arg)
} else {
  file.path(getwd(), "tests", "testthat.R")
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(project_root, "renv", "activate.R"))
Sys.setenv(GAP_PANEL_PROJECT_ROOT = project_root)
testthat::test_dir(file.path(project_root, "tests", "testthat"), reporter = "summary")
