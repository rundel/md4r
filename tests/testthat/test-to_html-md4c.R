##################
###            ###
### md4c tests ###
###            ###
##################

md4c_tests_to_html = function() {

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
      #29, "Known bug, will be fixed in the next version of md4c",
      #33, "Weird utf-8 issue with En Quad U+2000 spaces"
    ),
    "permissive-www-autolinks" = tibble::tribble(
      ~ex, ~msg,
      #6, "& vs &amp; in href params"
    )
  )

  run_tests = function(file, name, flags, examples) {
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

  purrr::walk(
    tests,
    do.call, what = run_tests
  )
}

md4c_tests_to_html()


