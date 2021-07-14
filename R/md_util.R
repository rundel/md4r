md_n_nodes = function(x) {
  checkmate::assert_class(x, "md_node")

  length(unlist(x))
}

md_text = function(x, type = "md_text") {
  checkmate::assert_class(x, "md_node")

  # This is an ugly hack
  map_ast(x, classes = type)
}

md_text_summary = function(x, max_width = 60, type = "md_text_normal", replace_newline = "\\\\n") {
  txt = md_text(x, type)
  txt = paste(txt, collapse="\n")
  txt = gsub("\\n", replace_newline, txt)
  txt = substr(txt, 0, max_width)

  if (nchar(txt) == max_width)
    txt = paste0(txt, ellipsis_chars())

  txt
}

ellipsis_chars = function() {
  if (is_utf8_output()) {
    "\u2026"
  } else {
    "..."
  }
}



