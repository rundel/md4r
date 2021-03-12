process_child_nodes = function(md, flags, ..., collapse = NULL) {
  content = unlist(lapply(md, to_md, flags = flags, ...))

  if (!is.null(collapse))
    content = paste(content, collapse = collapse)

  content
}

#' @title Convert a markdown object back to markdown text
#' @description
#'
#' @param md Markdown object
#' @param flags Parser flags
#' @param ... Unused, for extensibility.
#'
#' @export
to_md = function(md, flags = flags_used(md), ...) {
 UseMethod("to_md")
}

#' @exportS3Method
to_md.default = function(md, flags, ...) {
  stop("Unsupported S3 class", call. = FALSE)
}


#' @exportS3Method
to_md.md_node = function(md, flags, ...) {
  process_child_nodes(md, flags, ...)
}

#############
#
# md_block_*
#
#############

#' @exportS3Method
to_md.md_block = function(md, flags, ...) {
  stop("Unknown block class: ", class(md)[1], call. = FALSE)
}

#' @exportS3Method
to_md.md_block_doc = function(md, flags, ...) {
  if (missing(flags))
    flags = flags_used(md)

  content = process_child_nodes(md, flags, ..., collapse="")
  trimws(content)
}


#' @exportS3Method
to_md.md_block_quote = function(md, flags, ...) {
  children = process_child_nodes(md, flags, ...)
  paste(">", children)
}

#' @exportS3Method
to_md.md_block_ul = function(md, flags, ...) {
  mark = attr(md, "mark")
  tight = attr(md, "tight")

  children = process_child_nodes(md, flags, ...)
  if (!tight)
    children = paste0(children, "\n")

  paste(mark, children, collapse="\n")
}

#' @exportS3Method
to_md.md_block_ol = function(md, flags, ...) {
  start = attr(md, "start")
  delim = attr(md, "mark_delimiter")
  tight = attr(md, "tight")

  n = start+length(md)-1

  children = process_child_nodes(md, flags, ...)
  if (!tight)
    children = paste0(children, "\n")

  paste(start:n, delim, " ", children, sep="", collapse="\n")
}

#' @exportS3Method
to_md.md_block_li = function(md, flags, ...) {
  is_task   = attr(md, "is_task")
  task_mark = attr(md, "task_mark")

  content = process_child_nodes(md, flags, ..., collapse="")

  if (is_task) {
    glue::glue("[{task_mark}] {content}")
  } else {
    content
  }
}

#' @exportS3Method
to_md.md_block_h = function(md, flags, ...) {
  tag = paste0("h", attr(md, "level"))
  tag_block(tag, md, ...)
}

#' @exportS3Method
to_md.md_block_code = function(md, flags, ...) {
  lang   = attr(md, "lang")
  paste( c(
    paste0("```", lang),
    process_child_nodes(md, flags, ...),
    "```"
  ), collapse="\n")
}

#' @exportS3Method
to_md.md_block_html = function(md, flags, ...) {
  process_child_nodes(md, flags, ...)
}

#' @exportS3Method
to_md.md_block_p = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse = "")
  paste0(content, "\n\n" )
}

#' @exportS3Method
to_md.md_block_table = function(md, flags, ...) {
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
to_md.md_span_em = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse="")
  paste0("*", content, "*")
}

#' @exportS3Method
to_md.md_span_strong = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse="")
  paste0("**", content, "**")
}

#' @exportS3Method
to_md.md_span_u = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse="")
  paste0("_", content, "_")
}

#' @exportS3Method
to_md.md_span_code = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse="")
  paste0("`", content, "`")
}

#' @exportS3Method
to_md.md_span_del = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ..., collapse="")
  paste0("~", content, "~")
}

#' @exportS3Method
to_md.md_span_latexmath = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ...)
  paste0("$", content, "$")
}

#' @exportS3Method
to_md.md_span_latexmath_display = function(md, flags, ...) {
  content = process_child_nodes(md, flags, ...)

  sep = ""
  if (length(content) > 1) {
    content = paste(content, collapse = "")
    sep = "\n"
  }

  paste("$$", trimws(content), "$$", sep = sep)
}

#' @exportS3Method
to_md.md_span_a = function(md, flags, ...) {
  href = attr(md, "href")
  title = attr(md, "title")

  if (title != "" & !is.null(title))
    title = glue::glue(" \"{title}\"")

  content = process_child_nodes(md, flags, ..., collapse = "")

  if (href == content) {
    link = href
  } else {
    link = glue::glue("[{content}]({href}{title})")
  }

  if (grepl("^mailto:", href)) {
    if (!"MD_FLAG_PERMISSIVEEMAILAUTOLINKS" %in% flags & !grepl("^\\[", link)) {
      link = paste0("<", link, ">")
    } else if (sub("^mailto:", "", href) == content) {
      link = content
    }
  } else {
    check_flags = c(
      "MD_FLAG_PERMISSIVEURLAUTOLINKS",
      "MD_FLAG_PERMISSIVEWWWAUTOLINKS"
    )
    if (!any(check_flags %in% flags) & !grepl("^\\[", link)) {
      link = paste0("<", link, ">")
    } else if (sub("^http[s]?://", "", href) == content) {
      link = content
    }
  }

  link
}

#' @exportS3Method
to_md.md_span_img = function(md, flags, ...) {
  src = attr(md, "src")
  title = attr(md, "title")

  if (title != "" & !is.null(title))
    title = glue::glue(" '{title}'")

  content = process_child_nodes(md, flags, ...)
  text = paste(content, collapse="")

  glue::glue("![{text}]({src}{title})")
}

#' @exportS3Method
to_md.md_span_wikilink = function(md, flags, ...) {
  target = attr(md, "target")
  content = process_child_nodes(md, flags, ..., collapse="")
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
to_md.md_text_break = function(md, flags, ...) {
  "\n\n"
}

#' @exportS3Method
to_md.md_text_softbreak = function(md, flags, ...) {
  "\n"
}

#' @exportS3Method
to_md.md_text_latexmath = function(md, flags, ...) {
  if (md == " ")
    "\n"
  else
    md
}

#' @exportS3Method
to_md.md_text = function(md, flags, ...) {
  c(md)
}



# TODO - check breaks'
# TODO - html escaping?


