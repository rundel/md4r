read_md4c_tests = function(file) {
  section = character()
  flags = character()
  examples = list()

  in_example = FALSE
  end_md = FALSE
  md = character()
  html = character()

  flag_pat = "(MD_FLAG_|MD_DIALECT_)[A-Z]+"
  chunk_start_pat = "^```````````````````````````````` example$"
  chunk_end_pat   = "^````````````````````````````````$"

  l = readLines(file)
  for(i in seq_along(l)) {
    line = l[i]

    if (grepl("^#{1,6}", line)) {
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
      end_md = FALSE

      examples[[length(examples)+1]] = list(
        md = md,
        html = html,
        sec = section,
        line_start = line_start,
        line_end = i-1
      )

      md = character()
      html = character()
    }
    else if (grepl("^\\.$", line)) {
      end_md = TRUE
    } else {
      if (in_example & !end_md)
        md = c(md, line)
      if (in_example & end_md)
        html = c(html, line)
    }
  }

  list(
    file = file,
    flags = flags,
    examples = examples
  )
}

test_files = list.files("md4c/", ".txt", full.names = TRUE)

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

check_skip = function(name, i) {
  x = i == skip_tests[[name]][["ex"]]
  if (sum(x) > 0) {
    testthat::skip(
      skip_tests[[name]][["msg"]][x]
    )
  }
}


for(file in test_files) {
  name = gsub("^.*/|\\.txt$","", file)

  tests = read_md4c_tests(file)
  flags = tests$flags

  if (name == "wiki-links")
    flags = c(flags, "MD_FLAG_TABLES") # Flag not listed in the file for some reason

  if (length(flags) == 0)
    flags = character()

  purrr::iwalk(
    tests$examples,
    function(test, i) {
      label = glue::glue("{name} - Ex {i} (L{test$line_start}-{test$line_end}) - {test$sec}")

      test_that(label, {

        check_skip(name, i)

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
