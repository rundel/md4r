##################
###            ###
### md4c tests ###
###            ###
##################

md4c_tests_to_md = function() {

  tests = purrr::map(
    list.files(
      system.file("specs/md4c/", package="md4r"),
      ".txt", full.names = TRUE
    ),
    read_md4c_tests
  )


  skip_tests = list(
    "coverage" = tibble::tribble(
      ~ex, ~msg,
       29, "Current bug with md4c"
    ),
    "permissive-www-autolinks" = tibble::tribble(
      ~ex, ~msg,
        1, "TODO - autolink issues",
        2, "TODO - autolink issues",
        3, "TODO - autolink issues",
        4, "TODO - autolink issues",
        5, "TODO - autolink issues",
        6, "TODO - autolink issues",
        7, "TODO - autolink issues"
    )
  )

  run_tests = function(file, name, flags, examples) {
    purrr::iwalk(
      examples,
      function(test, i) {
        label = glue::glue("md4c tests - {name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

        test_that(label, {

          # Check skips
          sub = (i == skip_tests[[name]][["ex"]])
          if (any(sub))
            testthat::skip( skip_tests[[name]][["msg"]][sub] )

          expect_identical_md(
            md = test$md, flags = flags, info = NULL
          )
        })
      }
    )
  }

  purrr::walk(
    tests,
    do.call, what = run_tests
  )
}

md4c_tests_to_md()


########################
###                  ###
### CommonMark tests ###
###                  ###
########################


commonmark_tests_to_md = function(version = "0.29") {
  file = paste0("specs/commonmark/spec_", version, ".json")
  tests = read_commonmark_spec(file = system.file(file, package="md4r"))

  skip_tests = tibble::tribble(
    ~ex, ~msg,
    227, "TODO - not sure how to keep code block and list separate",
    269, "Bug with md4c - See #153",
    283, "Related to 227",
    516, "TODO - Seems like an md4c bug - follow up"
  )

  purrr::walk(
    tests,
    function(test) {
      label = paste0("CommonMark Spec (", version, ") - ", test$label)
      ex = test$example
      url = paste0("https://spec.commonmark.org/", version, "/#example-", ex)

      test_that(label, {
        sub = (ex == skip_tests[["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[["msg"]][sub] )

        flags = "MD_DIALECT_COMMONMARK"

        expect_identical_md(
          md = test$markdown, flags = flags, info = url
        )
      })
    }
  )
}

commonmark_tests_to_md("0.29")



#################
###           ###
### gfm tests ###
###           ###
#################

gfm_tests_to_html = function() {
  file = system.file("specs/gfm/spec.txt", package="md4r")
  tests = read_gfm_tests(file)

  # Seem to be essentially the same issues as for CommonMark
  skip_tests = tibble::tribble(
    ~ex, ~msg,
    235, "list vs code issue, see above",
    277, "Bug with md4c",
    293, "list vs code issue, see above",
    528, "TODO - incorrect escape in link"
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

        flags = "MD_DIALECT_GITHUB"

        expect_identical_md(
          md = test$md, flags = flags, info = url
        )
      })
    }
  )
}

gfm_tests_to_html()
