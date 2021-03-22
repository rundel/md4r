##################
###            ###
### md4c tests ###
###            ###
##################

md4c_tests_to_md = function(file, name, flags, examples) {

  if (name %in% c("permissive-www-autolinks"))
    return()

  skip_tests = list(
    "coverage" = tibble::tribble(
      ~ex, ~msg,
       29, "Current bug with md4c"
    )
  )

  purrr::iwalk(
    examples,
    function(test, i) {
      label = glue::glue("md4c tests - {name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

      test_that(label, {

        # Check skips
        sub = (i == skip_tests[[name]][["ex"]])
        if (any(sub))
          testthat::skip( skip_tests[[name]][["msg"]][sub] )

        orig_md = test$md
        orig_md_ast = parse_md(orig_md, flags)
        to_md = to_md(orig_md_ast)
        to_md_ast = parse_md(to_md, flags)

        expect_identical_md(
          orig_md, to_md,
          to_md_ast, orig_md_ast,
          flags
        )
      })
    }
  )
}


m4dc_tests = purrr::map(
  list.files("md4c/", ".txt", full.names = TRUE),
  read_md4c_tests
)

purrr::walk(
  m4dc_tests,
  do.call, what = md4c_tests_to_md
)


########################
###                  ###
### CommonMark tests ###
###                  ###
########################

skip_tests = tibble::tribble(
  ~ex, ~msg,
   63, "Not sure how to handle currently",
  208, "Related to 63",
  227, "Indenting madness",
  262, "More block quote wrapping fun",
  263, "More block quote wrapping fun",
  269, "Bug with md4c - See #153",
  282, "Subtle list indenting handling",
  283, "Subtle list indenting handling",
  285, "missing block p in lists",
  289, "missing block p in lists",
  349, "code span needs additional context awareness",
  469, "emph / strong nesting rule weirdness",
  516, "escaping edge case"
)


version = "0.29"
purrr::walk(
  read_commonmark_spec(version=version),
  function(test) {
    label = paste0("CommonMark Spec (", version, ") - ", test$label)
    ex = test$example

    test_that(label, {
      sub = (ex == skip_tests[["ex"]])
      if (any(sub))
        testthat::skip( skip_tests[["msg"]][sub] )

      flags = "MD_DIALECT_COMMONMARK"

      orig_md = test$markdown
      orig_md_ast = parse_md(orig_md, flags)
      to_md = to_md(orig_md_ast)
      to_md_ast = parse_md(to_md, flags)

      expect_identical_md(
        orig_md, to_md,
        to_md_ast, orig_md_ast,
        flags
      )
    })
  }
)
