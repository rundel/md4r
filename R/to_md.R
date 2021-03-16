process_child_nodes = function(md, ..., collapse = NULL) {
  content = unlist(lapply(md, to_md, ...))

  if (!is.null(collapse))
    content = paste(content, collapse = collapse)

  content
}

#' @title Convert a markdown object back to markdown text
#' @description
#'
#' @param md Markdown object
#' @param ... Unused, for extensibility.
#'
#' @export
to_md = function(md, ...) {
 UseMethod("to_md")
}

#' @exportS3Method
to_md.default = function(md, ...) {
  stop("Unsupported S3 class", call. = FALSE)
}


#' @exportS3Method
to_md.md_node = function(md, ...) {
  process_child_nodes(md, ...)
}

#############
#
# md_block_*
#
#############

#' @exportS3Method
to_md.md_block = function(md, ...) {
  stop("Unknown block class: ", class(md)[1], call. = FALSE)
}

#' @exportS3Method
to_md.md_block_doc = function(md, ...) {
  process_child_nodes(md, ..., collapse="")
}


#' @exportS3Method
to_md.md_block_quote = function(md, ...) {
  children = process_child_nodes(md, ...)
  paste(">", children)
}

#' @exportS3Method
to_md.md_block_ul = function(md, ...) {
  mark = attr(md, "mark")
  tight = attr(md, "tight")

  indent = list(...)$indent %||% " "
  new_indent = "  " # mark is always 1 char long

  children = process_child_nodes(md, indent = paste0(indent, new_indent), ...)
  if (!tight)
    children = paste0(children, "\n")

  paste(indent, mark, " ", children, sep="", collapse="\n")
}

#' @exportS3Method
to_md.md_block_ol = function(md, ...) {
  start = attr(md, "start")
  delim = attr(md, "mark_delimiter")
  tight = attr(md, "tight")

  # Assumes all children are <li>
  n = start+length(md)-1

  indent = list(...)$indent %||% " "
  new_indent = paste(rep(" ", nchar(n) + nchar(delim) + 1), collapse="")

  children = process_child_nodes(md, indent = paste0(indent, new_indent), ...)
  if (!tight)
    children = paste0(children, "\n")

  paste(indent, start:n, delim, " ", children, sep="", collapse="\n")
}

#' @exportS3Method
to_md.md_block_li = function(md, ...) {
  is_task   = attr(md, "is_task")
  task_mark = attr(md, "task_mark")

  content = process_child_nodes(md, ..., collapse="\n")

  if (is_task) {
    glue::glue("[{task_mark}] {content}")
  } else {
    glue::glue("{content}")
  }
}

#' @exportS3Method
to_md.md_block_h = function(md, ...) {
  tag = paste0("h", attr(md, "level"))
  tag_block(tag, md, ...)
}

#' @exportS3Method
to_md.md_block_code = function(md, ...) {
  lang   = attr(md, "lang")
  fence_char = attr(md, "fence_char")

  content = process_child_nodes(md, ..., collapse = "")

  if (fence_char == "") { # Indented code blocks
    # md_text_code is wonky, this seems easiest
    content = paste("    ", strsplit(content,"\n")[[1]], sep="", collapse="\n")
    glue::glue("{content}\n", .trim = FALSE)
  } else { # Fenced code block
    fence = paste(rep(fence_char, 3), collapse="")
    glue::glue("{fence}{lang}\n{content}{fence}\n", .trim = FALSE)
  }
}

#' @exportS3Method
to_md.md_block_html = function(md, ...) {
  process_child_nodes(md, ..., collapse="")
}

#' @exportS3Method
to_md.md_block_p = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = "")
  paste0(content, "\n\n" )
}

#' @exportS3Method
to_md.md_block_table = function(md, ...) {
  # TODO - Fix me
}


#############
#
# md_span_*
#
#############

#span_text = function(md, ..., collapse="\n") {
#  text = unlist(lapply(md, to_md, ...))
#  paste(text, collapse=collapse)
#}
#
#tag_span = function(tag, md, ..., collapse="\n") {
#  checkmate::assert_character(tag, len = 1, any.missing = FALSE)
#
#  tag_close = strsplit(tag, " ")[[1]][1] # strip attributes
#  paste0(
#    glue::glue("<{tag}>"),
#    span_text(md, ..., collapse),
#    glue::glue("</{tag_close}>")
#  )
#}

#' @exportS3Method
to_md.md_span_em = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  paste0("*", content, "*")
}

#' @exportS3Method
to_md.md_span_strong = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  paste0("**", content, "**")
}

#' @exportS3Method
to_md.md_span_u = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  paste0("_", content, "_")
}

#' @exportS3Method
to_md.md_span_code = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  paste0("`", content, "`")
}

#' @exportS3Method
to_md.md_span_del = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  paste0("~", content, "~")
}

#' @exportS3Method
to_md.md_span_latexmath = function(md, ...) {
  content = process_child_nodes(md, ...)
  paste0("$", content, "$")
}

#' @exportS3Method
to_md.md_span_latexmath_display = function(md, ...) {
  content = process_child_nodes(md, ...)

  sep = ""
  if (length(content) > 1) {
    content = paste(content, collapse = "")
    sep = "\n"
  }

  paste("$$", trimws(content), "$$", sep = sep)
}

#' @exportS3Method
to_md.md_span_a = function(md, ...) {
  href = attr(md, "href")
  title = attr(md, "title")

  if (title != "" & !is.null(title))
    title = glue::glue(" \"{title}\"")

  content = process_child_nodes(md, ..., collapse = "")

  if (content == href || sub("^mailto:|^http[s]?://","", href) == content)
    glue::glue("<{content}>")
  else
    glue::glue("[{content}]({href}{title})")
}

#' @exportS3Method
to_md.md_span_img = function(md, ...) {
  src = attr(md, "src")
  title = attr(md, "title")

  if (title != "" & !is.null(title))
    title = glue::glue(" '{title}'")

  content = process_child_nodes(md, ...)
  text = paste(content, collapse="")

  glue::glue("![{text}]({src}{title})")
}

#' @exportS3Method
to_md.md_span_wikilink = function(md, ...) {
  target = attr(md, "target")
  content = process_child_nodes(md, ..., collapse="")
  content

  target = gsub("\\|", "\\\\|", target)

  if (target == content) {
    link = target
  } else {
    link = paste0(target, "|", content)
  }

  paste0("[[", link, "]]")
}






#############
#
# md_text_*
#
#############

#' @exportS3Method
to_md.md_text_break = function(md, ...) {
  "\n\n"
}

#' @exportS3Method
to_md.md_text_softbreak = function(md, ...) {
  "\n"
}

#' @exportS3Method
to_md.md_text_latexmath = function(md, ...) {
  if (md == " ")
    "\n"
  else
    md
}

#' @exportS3Method
to_md.md_text = function(md, ...) {
  c(md)
}



# TODO - check breaks'
# TODO - html escaping?


