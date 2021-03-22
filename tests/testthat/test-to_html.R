##################
###            ###
### md4c tests ###
###            ###
##################

md4c_tests_to_html = function(file, name, flags, examples) {

  skip_tests = list(
    "coverage" = tibble::tribble(
      ~ex, ~msg,
      29, "Known bug, will be fixed in the next version of md4c",
      33, "Weird utf-8 issue with En Quad U+2000 spaces"
    ),
    "permissive-www-autolinks" = tibble::tribble(
      ~ex, ~msg,
      6, "& vs &amp; in href params"
    )
  )

  purrr::iwalk(
    examples,
    function(test, i) {
      label = glue::glue("{name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

      test_that(label, {

        sub = (i == skip_tests[[name]][["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[[name]][["msg"]][sub] )


        if (length(test$md) == 1)
          test$md = paste0(test$md, "\n")

        expect_identical_html(
          test$md, flags,
          test$html,
          info = label
        )
      })
    }
  )
}

m4dc_tests = purrr::map(
  list.files("md4c/", ".txt", full.names = TRUE),
  read_md4c_tests
)

purrr::walk(
  m4dc_tests,
  do.call, what = md4c_tests_to_html
)



########################
###                  ###
### CommonMark tests ###
###                  ###
########################

skip_tests = tibble::tribble(
  ~ex, ~msg,
  497, "Minor disagreement about encoding # within href",
  591, "Minor disagreement about encoding & between params in url"
)

purrr::walk(
  read_commonmark_spec(version="0.29"),
  function(test) {
    label = test$label
    ex = test$example

    test_that(label, {
      sub = (ex == skip_tests[["ex"]])
      if (any(sub))
        testthat::skip( skip_tests[["msg"]][sub] )

      expect_identical_html(
        test$markdown, "MD_DIALECT_COMMONMARK",
        test$html,
        info = label
      )
    })
  }
)
