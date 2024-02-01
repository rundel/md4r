## Based in part on https://github.com/r-lib/fs/tree/master/R

pc = function(...) {
  paste(..., collapse="", sep="")
}

print_node = function(x, indent = "", prefix = "") {
  UseMethod("print_node")
}

#' @exportS3Method
print_node.default = function(x, indent = "", prefix = "") {
  cli::cli_abort("Unsupported class: {class(x)}")
}

attr_text = function(attr) {
  attr[["class"]] = NULL  # Hide class

  if (length(attr) == 0)
    return("")

  txt = purrr::map2_chr(
    names(attr), attr,
    function(name, val) {
      cli::format_inline("{.field {name}}: {.val {val}}")
    }
  )

  paste0(" [", paste(txt, collapse = ", "), "]")
}

#' @exportS3Method
print_node.md_block_li = function(x, indent = "", prefix = "") {
  if (attr(x, "is_task") == 0) {
    attr(x, "is_task") = NULL
    attr(x, "task_mark") = NULL
  }
  print_node.md_node(x, indent, prefix)
}

#' @exportS3Method
print_node.md_node = function(x, indent = "", prefix = "") {
  a = attributes(x)
  attr = ""
  if (length(a) != 0)
    attr = attr_text(a)

  node_format(
    type = class(x)[1],
    attr = attr,
    text = "",
    indent, prefix
  )
}

#' @exportS3Method
print_node.md_text_softbreak = function(x, indent = "", prefix = "") {
  print_node.md_node(x, indent, prefix)
}

#' @exportS3Method
print_node.md_text = function(x, indent = "", prefix = "") {
  type = class(x)[1]
  x = sub("\n$", "\\\\n", x) # escape trailing newlines
  node_format(
    type = type,
    attr = "",
    text = paste('"', x, '"', sep="", collapse=", "),
    indent, prefix
  )
}


node_format = function(type, attr, text, indent = "", prefix = "") {
  emph = cli::combine_ansi_styles(
    cli::style_bold,
    #cli::style_italic,
    cli::make_ansi_style("tomato")
  )


  if (text != "")
    text = paste0(" - ", cli::col_grey(text))
  cli::cat_line(
    indent,
    prefix,
    emph(type),
    attr,
    text
  )
}

code_format = function(code, indent, prefix) {
  line = paste0(indent, prefix)
  empty_prefix = paste0(
    indent,
    paste(rep(" ", nchar(prefix)), collapse="")
  )

  for(txt in code) {
    if (txt == "\n") {
      cli::cat_line(line)
      line = empty_prefix
    } else {
      line = paste0(line, cli::style_italic(txt))
    }
  }

}


print_tree = function(x, ...) {
  ch = box_chars()

  indent_width = 4

  empty_space = "    "
  child_space = pc(ch$v, "   ")
  mid_leaf = pc(ch$j, ch$h, ch$h, " ")
  end_leaf = pc(ch$l, ch$h, ch$h, " ")

  print_leaf = function(x, indent) {


    for(i in seq_along(x)) {
      if (i==length(x)) {
        print_node(x[[i]], indent, end_leaf)
        child_indent = paste0(indent, empty_space)
      } else {
        print_node(x[[i]], indent, mid_leaf)
        child_indent = paste0(indent, child_space)
      }

      if (inherits(x[[i]], "md_text"))
        next

      #if (inherits(x[[i]], "md_block_code")) {
      #  code_format(x[[i]], child_indent, end_leaf)
      #  next
      #}

      print_leaf(x[[i]], child_indent)
    }
  }

  print_node(x, "", "")

  if (!inherits(x, "md_text"))
    print_leaf(x, "")

  invisible(x)
}

print_brief = function(x, ...) {
  n = length(unlist(x))

  node_format(
    glue::glue("md ast [{n} nodes]"),
    paste0('"', md_text_summary(x, 60), '"')
  )
}


#' @exportS3Method
print.md_node = function(x, ..., brief = FALSE) {
  if (brief) print_brief(x, ...)
  else       print_tree(x, ...)
}


# These are derived from https://github.com/r-lib/cli/blob/e9acc82b0d20fa5c64dd529400b622c0338374ed/R/tree.R#L111
box_chars = function() {
  if (is_utf8_output()) {
    list(
      "h" = "\u2500",  # ─   horizontal
      "v" = "\u2502",  # │   vertical
      "l" = "\u2514",  # └
      "j" = "\u251C"   # ├
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",
      "j" = "+"
    )
  }
}



is_latex_output = function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}

is_utf8_output = function() {
  opt = getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}
