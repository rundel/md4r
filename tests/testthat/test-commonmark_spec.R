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
        test$markdown, "MD_DIALECT_COMMONMARK",
        test$html,
        info = test$label
      )
    }
  )

})
