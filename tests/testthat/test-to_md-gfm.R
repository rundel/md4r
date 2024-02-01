
#################
###           ###
### gfm tests ###
###           ###
#################

gfm_tests_to_md = function() {
  file = system.file("specs/gfm/spec.txt", package="md4r")
  tests = read_gfm_tests(file)

  # Seem to be essentially the same issues as for CommonMark
  skip_tests = tibble::tribble(
    ~ex, ~msg,
    235, "Ambiguous list to code block case #235",
    293, "Ambiguous list to code block case #293",
    528, "Ambiguous normal text escaping #528",
    623, "Ambiguous normal text escaping #623",
    624, "Ambiguous normal text escaping #624",
    631, "Ambiguous normal text escaping #631"
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
            "gfm #",
            skip_tests[["ex"]][sub], " - ",
            skip_tests[["msg"]][sub]
          ) )
        }
        flags = "MD_DIALECT_GITHUB"

        expect_identical_md(
          md = test$md, flags = flags, info = url
        )
      })
    }
  )
}

gfm_tests_to_md()
