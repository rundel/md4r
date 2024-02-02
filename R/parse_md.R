#' Parse markdown
#'
#' @description Parse either a literal markdown string or a markdown file
#' given a path. Different dialects and features are supported via the `flags`
#' argument. See [flags_describe()] for possible flags and their usage. `parse_md()`
#' defaults parsing using the commonmark spec while `parse_gfm()` uses the GitHub
#' flavored markdown spec.
#'
#' @param md Character. Either literal string of markdown or a path to a markdown file.
#' @param flags Character vector. Dialect flags used by the parser.
#'
#' @returns Both functions return a markdown ast, a list with the `md_block_doc` class.
#'
#' @examples
#' parse_md(system.file("examples/commonmark.md", package = "md4r"))
#'
#' parse_gfm(system.file("examples/github.md", package = "md4r"))
#'
#' @export
#'
parse_md = function(md, flags = "MD_DIALECT_COMMONMARK") {
  checkmate::assert_character(md, any.missing = FALSE)
  if (length(md) == 0)
    md = ""

  flags_check(flags)

  if (length(md) > 1) {              # If multiple lines in a char vec assume
    md = paste(md, collapse = "\n")  # it has been read in already
  } else if (!grepl("\n", md)) {     # If no newlines check if it is a path or url
    if (file.exists(md))
      md = read_file(md)
  }

  # Make sure file ends with trailing newline
  if (!grepl("\n$", md))
    md = paste0(md, "\n")

  md_block_doc = parse_md_cpp(md, flag_mask(flags))[[1]]
  attr(md_block_doc, "flags") = flags

  md_block_doc
}

#' @rdname parse_md
#'
#' @export
parse_gfm = function(md, flags = "MD_DIALECT_GITHUB") {
  parse_md(md, flags)
}


read_file = function(file) {
  txt = readLines(file)
  txt = paste(txt, collapse = "\n")
  paste0(txt, "\n")
}
