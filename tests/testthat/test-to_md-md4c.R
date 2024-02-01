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
    "regressions" = tibble::tribble(
      ~ex, ~msg,
       51, "Ambiguous punct escaping #51"
    ),
    "spec-permissive-autolinks" = tibble::tribble(
      ~ex, ~msg,
        5, "Ambiguous punct escaping #5"
    ),
    spec = tibble::tribble(
      ~ex, ~msg,
      257, "Ambiguous list to code block case #257",
      313, "Ambiguous list to code block case #313",
      520, "Ambiguous punct escaping #520"
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
          if (any(sub)) {
            testthat::skip( paste0(
              name, " #",
              skip_tests[[name]][["ex"]][sub], " - ",
              skip_tests[[name]][["msg"]][sub]
            ) )
          }

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

