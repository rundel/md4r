indent <- function(x) {
  if (length(x) == 0) return(character())
  x <- gsub("^( *)(  )([^ ].*[^ ])([)])$", "\\1\\2\\3\n\\1\\4", x)
  x <- unlist(strsplit(x, "\n"))
  paste0("  ", x)
}


##################
###            ###
### md4c tests ###
###            ###
##################

read_md4c_tests = function(file) {
  section = character()
  flags = "MD_DIALECT_COMMONMARK"
  examples = list()

  in_example = FALSE
  ex_sec = 0

  md = character()
  html = character()
  other = character()

  flag_pat = "(MD_FLAG_|MD_DIALECT_)[A-Z_]+"
  chunk_start_pat = "^```````````````````````````````` example"
  chunk_end_pat   = "^````````````````````````````````$"

  l = readLines(file)
  for(i in seq_along(l)) {
    line = l[i]

    if (!in_example && grepl("^#{1,6}", line)) {
      section = sub("^#{1,6}\\s+","", line)
    }
    else if (grepl(flag_pat, line)) {
      m = regexpr(flag_pat, line)
      flags = c(flags, regmatches(line,m))
    }
    else if (grepl(chunk_start_pat, line)) {
      in_example = TRUE
      line_start = i+1
    }
    else if (grepl(chunk_end_pat, line)) {
      in_example = FALSE
      ex_sec = 0

      examples[[length(examples)+1]] = list(
        md = md,
        html = html,
        other = unique(unlist(strsplit(other, " "))),
        sec = section,
        line_start = line_start,
        line_end = i-1
      )

      md = character()
      html = character()
      other = character()
    }
    else if (grepl("^\\.$", line)) {
      ex_sec = ex_sec + 1
    } else {
      if (in_example & ex_sec == 0) {
        md = c(md, line)
      } else if (in_example & ex_sec == 1) {
        html = c(html, line)
      } else if (in_example & ex_sec == 2) {
        other = c(other, line)
      }
    }
  }

  if (grepl("wiki-links", file)) {
    # Required flag not listed in the file for some reason
    flags = c(flags, "MD_FLAG_TABLES")
  }

  # Replace → with \t for inputs and outputs
  examples = lapply(
    examples,
    function(x) {
      x$md = gsub("→", "\t", x$md)
      x$html = gsub("→", "\t", x$html)
      x
    }
  )

  list(
    file = file,
    name = gsub("^.*/|\\.txt$","", file),
    flags = flags,
    examples = examples
  )
}


########################
###                  ###
### CommonMark Tests ###
###                  ###
########################

#read_commonmark_spec = function(file) {
#  tests = jsonlite::read_json(file)
#
#  purrr::map(
#    tests,
#    function(test) {
#      test$label = glue::glue_data("Ex {example} - {section} (L{start_line}-{end_line})", .x = test)
#      test[c("label", "markdown", "html", "example")]
#    }
#  )
#}

#################
###           ###
### gfm Tests ###
###           ###
#################

read_gfm_tests = function(file) {
  section = c("", "")
  examples = list()

  line_start = NA
  in_example = FALSE
  end_md = FALSE
  disabled = FALSE
  md = character()
  html = character()

  chunk_start_pat = "^```````````````````````````````` example"
  chunk_end_pat   = "^````````````````````````````````$"

  l = readLines(file)
  for(i in seq_along(l)) {
    line = l[i]

    if (!in_example & grepl("^#\\s+", line)) {
      section[1] = sub("^#\\s+","", line)
    }
    else if (!in_example & grepl("^##\\s+", line)) {
      section[2] = sub("^##\\s+","", line)
    }
    else if (grepl(chunk_start_pat, line)) {

      disabled = grepl("disabled", line) # track disabled tests
      in_example = TRUE
      line_start = i+1
    }
    else if (grepl(chunk_end_pat, line)) {
      examples[[length(examples)+1]] = list(
        md = md,
        html = html,
        sec = section,
        line_start = line_start,
        line_end = i-1,
        disabled = disabled
      )

      disabled = FALSE
      in_example = FALSE
      end_md = FALSE
      line_start = NA

      md = character()
      html = character()
    }
    else if (in_example & grepl("^\\.$", line)) {
      end_md = TRUE
    }
    else if (in_example) {
      if (!end_md)
        md = c(md, line)
      else if (end_md)
        html = c(html, line)
    }
  }

  # Replace → with \t for inputs and outputs
  lapply(
    examples,
    function(x) {
      x$md = gsub("→", "\t", x$md)
      x$html = gsub("→", "\t", x$html)
      x
    }
  )
}

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
    203, "md4c bug - should not be a table",

    345, "Minor spacing difference with code span",
    347, "Minor spacing difference with code span",

    398, "Nested strong tags not being simplified",
    426, "Preference given to md4c test",
    434, "Nested strong tags not being simplified",
    435, "Nested strong tags not being simplified",
    436, "Nested strong tags not being simplified",

    473, "Preference given to md4c test",
    474, "Preference given to md4c test",
    475, "Preference given to md4c test",
    477, "Preference given to md4c test",

    616, "Seems like a md4c bug - should not be an autolink",
    619, "Seems like a md4c bug - should not be an autolink",
    620, "Seems like a md4c bug - should not be an autolink",

    625, "Seems like a md4c bug - doesn't treat the trailing '+ok' as part of the url",
    626, "Seems like a md4c bug - doesn't find the 2nd url",
    627, "Seems like a md4c bug - doesn't find the url",

    652, "Not sure what to do about tag filtering atm - FIXME",

    660, "Minor spacing difference with code span"
  )

  purrr::imap(
    tests,
    function(test, i) {
      section = paste(test$sec, collapse = " > ")
      label = glue::glue("gfm - Ex {i} (L{test$line_start}-{test$line_end}) - {section}")
      url = glue::glue("https://github.github.com/gfm/#example-{i}")
      sub = (i == skip_tests[["ex"]])

      c(
        paste0("test_that(", rlang::expr_deparse(unclass(label)), ", {"),
        indent(c(
          if (test$disabled) rlang::expr_deparse(rlang::expr(testthat::skip("Disabled test(s)"))),
          if (!!any(sub)) rlang::expr_deparse(rlang::expr(
            testthat::skip( !!paste0(
              "gfm #", skip_tests[["ex"]][sub],
              " - ", skip_tests[["msg"]][sub]
            ))
          )),
          rlang::expr_deparse(width = 20, rlang::expr(
            expect_identical_html(
              c(!!!test$md),
              "MD_DIALECT_GITHUB",
              c(!!!test$html),
              info = !!unclass(label),
              url = !!unclass(url)
          )))
        )),
        "})"
      )
    }
  )
}

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
