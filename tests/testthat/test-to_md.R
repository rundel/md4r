

md4c_tests_to_md = function(file, name, flags, examples) {

  if (name %in% c("tables", "tasklists"))
    return()

  skip_tests = list(
    "coverage" = tibble::tribble(
      ~ex, ~msg,
       11, "Example is missing closing ``` for some reason",
       12, "Without MD_FLAG_UNDERLINE results are identical",
       14, "Equivalent up to newline between link & title",
    ),
    "wiki-links" = tibble::tribble(
      ~ex, ~msg,
       12, "Empty explicit is just converted to implicit",
       23, "Collapsing the multiline url seems ok here"
    )
  )

  use_commonmark_flag = list(
    `permissive-email-autolinks` = c(1L),
    `permissive-url-autolinks` = c(1L)
  )

  purrr::iwalk(
    examples,
    function(test, i) {
      label = glue::glue("{name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

      test_that(label, {

        # Check for link references, if present skip
        if (any(grepl("\\[.*?\\]:", paste(test$md, collapse="\n"))))
          testthat::skip( "Link references do not return original md" )

        # Check skips
        sub = (i == skip_tests[[name]][["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[[name]][["msg"]][sub] )

        # Check for commonmark flag
        if (i %in% use_commonmark_flag[[name]])
          flags = "MD_DIALECT_COMMONMARK"

        if (length(test$md) == 1)
          test$md = paste0(test$md, "\n")

        to_md = to_md( parse_md(test$md, flags) )
        orig_md = trimws( paste(test$md, collapse="\n") )

        expect_identical(
          to_md,
          orig_md,
          info = label
        )
      })
    }
  )
}


md4c_tests_to_md = function(file, name, flags, examples) {

  if (name %in% c("tables", "tasklists"))
    return()

  skip_tests = list(
    "coverage" = tibble::tribble(
      ~ex, ~msg,
    ),
    "wiki-links" = tibble::tribble(
      ~ex, ~msg,
    )
  )

  purrr::iwalk(
    examples,
    function(test, i) {
      label = glue::glue("{name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

      test_that(label, {

        # Check skips
        sub = (i == skip_tests[[name]][["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[[name]][["msg"]][sub] )

        orig_md = parse_md(test$md, flags)
        to_md = orig_md %>% to_md() %>% parse_md(flags)

        expect_identical_md(test$md, to_md, orig_md, flags)

        #expect_identical(
        #  to_md,
        #  orig_md,
        #  info = label
        #)
      })
    }
  )
}




list.files("md4c/", ".txt", full.names = TRUE) %>%
  purrr::map(read_md4c_tests) %>%
  purrr::walk(
    do.call, what = md4c_tests_to_md
  )
