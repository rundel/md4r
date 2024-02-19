
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
