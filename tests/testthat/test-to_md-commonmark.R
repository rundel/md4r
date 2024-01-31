
########################
###                  ###
### CommonMark tests ###
###                  ###
########################


commonmark_tests_to_md = function(version) {
  file = paste0("specs/commonmark/spec_", version, ".json")
  tests = read_commonmark_spec(file = system.file(file, package="md4r"))

  skip_tests = tibble::tribble(
    ~ex, ~msg,
    257, "Ambiguous list to code block case #257",
    313, "Ambiguous list to code block case #313",
    520, "Unclear when punct needs to be escaped"
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

commonmark_tests_to_md("0.31.2")

