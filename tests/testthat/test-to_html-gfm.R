#################
###           ###
### gfm tests ###
###           ###
#################

gfm_tests_to_html = function() {
  file = system.file("specs/gfm/spec.txt", package="md4r")
  tests = read_gfm_tests(file)

  skip_tests = tibble::tribble(
    ~ex, ~msg,
    203, "md4c bug - should not be a table",

    345, "Minor spacing difference with code span",
    347, "Minor spacing difference with code span",

    398, "Nested strong tags not being simplified",
    426, "Preference given to md4c test",
    434, "Nested strong tags not being simplified",
    435, "Nested strong tags not being simplified",
    436, "Nested strong tags not being simplified",

    473, "Preference given to md4c test",
    474, "Preference given to md4c test",
    475, "Preference given to md4c test",
    477, "Preference given to md4c test",

    616, "Seems like a md4c bug - should not be an autolink",
    619, "Seems like a md4c bug - should not be an autolink",
    620, "Seems like a md4c bug - should not be an autolink",

    625, "Seems like a md4c bug - doesn't treat the trailing '+ok' as part of the url",
    626, "Seems like a md4c bug - doesn't find the 2nd url",
    627, "Seems like a md4c bug - doesn't find the url",

    652, "Not sure what to do about tag filtering atm - FIXME",

    660, "Minor spacing difference with code span"
  )

  purrr::iwalk(
    tests,
    function(test, i) {
      section = paste(test$sec, collapse = " > ")
      label = glue::glue("gfm - Ex {i} (L{test$line_start}-{test$line_end}) - {section}")
      url = glue::glue("https://github.github.com/gfm/#example-{i}")

      test_that(label, {

        if (test$disabled) {
          testthat::skip("Disabled test(s)")
        }

        sub = (i == skip_tests[["ex"]])
        if (any(sub)) {
          testthat::skip( paste0(
            "gfm #", skip_tests[["ex"]][sub],
            " - ", skip_tests[["msg"]][sub]
          ) )
        }

        expect_identical_html(
          test$md, "MD_DIALECT_GITHUB",
          test$html,
          info = label,
          url = url
        )
      })
    }
  )
}

gfm_tests_to_html()
