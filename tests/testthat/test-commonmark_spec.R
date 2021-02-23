read_spec = function(version) {
  tests = jsonlite::read_json(
    glue::glue("commonmark/spec_{version}.json")
  )

  purrr::map(
    tests,
    function(test) {
      test$label = glue::glue_data("Ex {example} - {section} (L{start_line}-{end_line})", .x = test)
      test[c("label", "markdown", "html", "example")]
    }
  )
}

clean_html = function(x) {
  x = paste(x, collapse="")
  x = gsub("\n", "", x)
}

expect_identical_html = function(object, expected, info = NULL, label = NULL, expected.label = NULL, ...) {
  md = parse_md(object, flags = "MD_DIALECT_COMMONMARK")
  md_text = trimws(paste(object, collapse="\n"))
  md_text = cli_glue("{.val {md_text}}")
  md_html = to_html(md)
  md_html = clean_html(md_html)

  spec_html = clean_html(expected)

  # Based on testthat:::expect_waldo_equal
  comp = testthat:::waldo_compare(md_html, spec_html, ..., x_arg = "actual", y_arg = "expected")
  comp_txt = paste(comp, collapse = '\n\n', sep = "\n")

  msg = paste(
    paste0(info, ": generated html does not match expected html."),
    "",
    paste0("`markdown`: ", md_text),
    comp_txt,
    sep="\n"
  )

  expect(
    length(comp) == 0,
    msg,
    trace_env = rlang::caller_env()
  )

  invisible()
}



test_that("0.29 Spec - to_html()", {

  skip_examples = c(
    497, # Minor disagreement about encoding # within href
    591  # Minor disagreement about encoding & between params
  )

  tests = read_spec("0.29")

  purrr::walk(
    tests,
    function(test) {
      if (test$example %in% as.character(skip_examples))
        return()

      expect_identical_html(
        test$markdown,
        test$html,
        info = test$label,
      )
    }
  )

})
