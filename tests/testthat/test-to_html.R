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
  list.files(
    system.file("specs/md4c/", package="md4r"),
    ".txt", full.names = TRUE
  ),
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
  591, "Minor disagreement about encoding & between params in url"
)
version = "0.29"
file = paste0("specs/commonmark/spec_", version, ".json")

purrr::walk(
  read_commonmark_spec(file = system.file("specs/commonmark/spec_0.29.json", package="md4r")),
  function(test) {
    label = test$label
    ex = test$example
    url = paste0("https://spec.commonmark.org/", version, "/#example-", ex)

    test_that(label, {
      sub = (ex == skip_tests[["ex"]])
      if (any(sub))
        testthat::skip( skip_tests[["msg"]][sub] )

      expect_identical_html(
        test$markdown, "MD_DIALECT_COMMONMARK",
        test$html,
        info = label,
        url = url
      )
    })
  }
)


#################
###           ###
### gfm tests ###
###           ###
#################

gfm_tests_to_html = function(examples) {

  skip_tests = tibble::tribble(
    ~ex, ~msg,
    145, "GFM spec issue - tag filtering should be applied",
    147, "GFM spec issue - tag filtering should be applied",
    279, "Minor difference in task list attributes",
    280, "Minor difference in task list attributes",
    616, "GFM spec issue - these are autolinks",
    619, "GFM spec issue - these are autolinks",
    620, "GFM spec issue - these are autolinks",
    603, "Minor disgreement - prefer & in url",
    626, "Minor disgreement - prefer & in url",
    203, "Known discrepancy - See https://github.com/mity/md4c/issues/157"
  )

  purrr::iwalk(
    examples,
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

tests = read_gfm_tests(system.file("specs/gfm/spec.txt", package="md4r"))

gfm_tests_to_html(tests)
