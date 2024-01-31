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
    619, "Seems like an md4c bug - should not be an autolink #619",
    620, "Seems like an md4c bug - doesn't parse url arg #620",
    626, "Seems like an md4c bug - doesn't find the 2nd url #626",
    627, "Seems like an md4c bug - doesn't find the url #627",
    645, "Escaping issues with fake html #645",
    646, "Escaping issues with fake html #646",
    661, "Minor spacing difference with code span #661"

    #145, "GFM spec issue - tag filtering should be applied",
    #147, "GFM spec issue - tag filtering should be applied",
    #279, "Minor difference in task list attributes",
    #280, "Minor difference in task list attributes",
    #616, "GFM spec issue - these are autolinks",
    #619, "GFM spec issue - these are autolinks",
    #620, "GFM spec issue - these are autolinks",
    #603, "Minor disgreement - prefer & in url",
    #626, "Minor disgreement - prefer & in url",
    #203, "Known discrepancy - See https://github.com/mity/md4c/issues/157"
  )

  purrr::iwalk(
    tests,
    function(test, i) {
      section = paste(test$sec, collapse = " > ")
      label = glue::glue("gfm - Ex {i} (L{test$line_start}-{test$line_end}) - {section}")
      url = glue::glue("https://github.github.com/gfm/#example-{i}")

      test_that(label, {

        sub = (i == skip_tests[["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[["msg"]][sub] )


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
