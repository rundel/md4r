
# From https://github.com/r-lib/cli/issues/226

local_cli_config = function(
  unicode = FALSE, dynamic = FALSE, ansi = FALSE,
  num_colors = 1, .local_envir = parent.frame()
) {
  withr::local_options(
    cli.dynamic = dynamic,
    cli.ansi = ansi,
    cli.unicode = unicode,
    crayon.enabled = num_colors > 1,
    crayon.colors = num_colors,
    .local_envir = .local_envir
  )
  withr::local_envvar(
    PKG_OMIT_TIMES = "true",
    PKG_OMIT_SIZES = "true",
    .local_envir = .local_envir
  )
}



######

block_tags = c(
  'article', 'header', 'aside', 'hgroup', 'blockquote',
  'hr', 'iframe', 'body', 'li', 'map', 'button', 'object', 'canvas',
  'ol', 'caption', 'output', 'col', 'p', 'colgroup', 'pre', 'dd',
  'progress', 'div', 'section', 'dl', 'table', 'td', 'dt',
  'tbody', 'embed', 'textarea', 'fieldset', 'tfoot', 'figcaption',
  'th', 'figure', 'thead', 'footer', 'tr', 'form', 'ul',
  'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'video', 'script', 'style'
)


clean_html = function(x) {
  x = paste(x, collapse="")
  x = gsub("\n", "", x)
  x = gsub(" />", ">", x) # Some img and a tags differ with usage of this across test cases
}

expect_identical_html = function(md, flags, expected, info = NULL, url = NULL, ...) {
  local_cli_config(unicode = TRUE, num_colors = 256)

  ast = parse_md(md, flags = flags)

  md_html = clean_html( to_html(ast) )
  ex_html = clean_html( expected )

  # Based on testthat:::expect_waldo_equal

  if (identical(md_html, ex_html)) {
    type = "success"
    msg = ""
  } else {
    type = "failure"

    md_text = paste(md, collapse="\n")
    #md_text = cli::format_inline("{.val {md_text}}")
    #md_text = paste(md_text, collapse=" ")

    diff = diffmatchpatch::diff_make(
      paste(ex_html, collapse="\n"),
      paste(md_html, collapse="\n")
    )

    error = paste0(info, ": generated html does not match expected html.")
    if (!is.null(url))
      error = paste(error, url, sep="\n")

    fix_esc_chars = function(x) {
      x = gsub("\n", "\\\\n", x)
      x = gsub("\t", "\\\\t", x)
    }

    msg = paste(
      error,
      "",
      paste0("markdown : \"", fix_esc_chars(md_text), "\""),
      paste0("expected : ", fix_esc_chars(ex_html)),
      paste0("generated: ", fix_esc_chars(md_html)),
      #comp_txt,
      #"",
      paste0("diff     : ", as.character(diff)),
      sep="\n"
    )
  }

  exp_signal(
    expectation(type, msg, srcref = NULL, trace = NULL)
  )

  invisible()
}




expect_identical_md = function(md, flags, info = NULL, ...) {

  orig_md = md
  orig_md_ast = parse_md(orig_md, flags)

  to_md = to_md(orig_md_ast)
  to_md_ast = parse_md(to_md, flags)

  comp = identical(orig_md_ast, to_md_ast)

  local_cli_config(unicode = TRUE, num_colors = 256)

  if (comp) {
    type = "success"
    msg = ""
  } else {
    type = "failure"

    msg = paste(
      info,
      "",
      "Generated markdown ast does not match expected ast.",
      "",
      cli::style_bold("markdown:"),
      cli::col_grey( paste(
        md, collapse="\n"
      ) ),
      "",
      cli::style_bold("expected ast:"),
      paste(
        capture.output(print(orig_md_ast)),
        collapse="\n"
      ),
      "",
      cli::style_bold("to_md() markdown:"),
      cli::col_grey( paste(
        to_md, collapse="\n"
      ) ),
      "",
      cli::style_bold("observed ast:"),
      paste(
        capture.output(print(to_md_ast)),
        collapse="\n"
      ),
      #"",
      #paste(
      #  capture.output(print(comp)),
      #  collapse = "\n"
      #),
      sep="\n"
    )
  }

  exp_signal(
    expectation(type, msg, srcref = NULL, trace = NULL)
  )

  invisible()
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
