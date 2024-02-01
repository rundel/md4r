##################
###            ###
### md4c tests ###
###            ###
##################


flag_lookup = c(
  "--fcollapse-whitespace"         = "MD_FLAG_COLLAPSEWHITESPACE",
  "--ftables"                      = "MD_FLAG_TABLES",
  "--fpermissive-url-autolinks"    = "MD_FLAG_PERMISSIVEURLAUTOLINKS",
  "--fstrikethrough"               = "MD_FLAG_STRIKETHROUGH",
  "--ftables"                      = "MD_FLAG_TABLES",
  "--fwiki-links"                  = "MD_FLAG_WIKILINKS",
  "--fhard-soft-breaks"            = "MD_FLAG_HARD_SOFT_BREAKS",
  "--flatex-math"                  = "MD_FLAG_LATEXMATHSPANS",
  "--fpermissive-email-autolinks"  = "MD_FLAG_PERMISSIVEEMAILAUTOLINKS",
  "--fpermissive-www-autolinks"    = "MD_FLAG_PERMISSIVEWWWAUTOLINKS",
  "--ftasklists"                   = "MD_FLAG_TASKLISTS",
  "--funderline"                   = "MD_FLAG_UNDERLINE",
  "--fwiki-links"                  = "MD_FLAG_WIKILINKS"
)

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
      8, "Weird utf issue with spaces",
      9, "Weird utf issue with spaces",
      10, "Weird utf issue with spaces"
    ),
    "regressions" = tibble::tribble(
      ~ex, ~msg,
      5, "New line vs space issue"
    ),
    "spec" = tibble::tribble(
      ~ex, ~msg,
      335, "Slight spacing discrepancy in the span output",
      337, "Slight spacing discrepancy in the span output",
      640, "Slight spacing discrepancy in the span output",
    )
  )

  run_tests = function(file, name, flags, examples) {
    purrr::iwalk(
      examples,
      function(test, i) {
        label = glue::glue("{name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

        test_that(label, {

          sub = (i == skip_tests[[name]][["ex"]])
          if (any(sub)) {
            testthat::skip( paste0(
              name, " #",
              skip_tests[[name]][["ex"]][sub], " - ",
              skip_tests[[name]][["msg"]][sub]
            ) )
          }


          if (length(test$md) == 1)
            test$md = paste0(test$md, "\n")

          other_flags = flag_lookup[test$other]
          stopifnot(all(!is.na(other_flags)))

          expect_identical_html(
            test$md, c(other_flags, flags),
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


