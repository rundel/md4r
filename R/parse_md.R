#' @export
parse_md = function(md, flags = "MD_DIALECT_GITHUB") {
  checkmate::assert_character(md, min.len = 1, any.missing = FALSE)
  flags_check(flags)

  if (length(md) > 1) {              # If multiple lines in a char vec assume
    md = paste(md, collapse = "\n")  # it has been read in already
  } else if (!grepl("\n", md)) {     # If no newlines then assume it is a path or url
    md = readr::read_file(md)
  }

  # Make sure file ends with trailing newline
  if (!grepl("\n$", md))
    md = paste0(md, "\n")

  md_block_doc = parse_md_cpp(md, flag_mask(flags))[[1]]
  attr(md_block_doc, "flags") = flags

  md_block_doc
}
