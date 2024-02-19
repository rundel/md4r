if (Sys.getenv("CI") == "") {
  text <- gfm_tests_to_html()
  flat <- purrr::reduce(text, ~ c(.x, "", .y))
  writeLines(flat, "test-to_html-gfm.R")
}
