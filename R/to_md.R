process_child_nodes = function(md, ...) {
  unlist(lapply(md, to_md, ...))
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
  process_child_nodes(md, ...)
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

  children = process_child_nodes(md, ...)
  if (!tight)
    children = paste0(children, "\n")

  paste(mark, children)
}

#' @exportS3Method
to_md.md_block_ol = function(md, ...) {
  start = attr(md, "start")
  delim = attr(md, "mark_delimiter")
  tight = attr(md, "tight")

  n = start+length(md)-1

  children = process_child_nodes(md, ...)
  if (!tight)
    children = paste0(children, "\n")

  paste0(start:n, delim, " ", children)
}

#' @exportS3Method
to_md.md_block_li = function(md, ...) {
  # TODO - handle task stuff
  children = process_child_nodes(md, ...)
}

#' @exportS3Method
to_md.md_block_h = function(md, ...) {
  tag = paste0("h", attr(md, "level"))
  tag_block(tag, md, ...)
}

#' @exportS3Method
to_md.md_block_code = function(md, ...) {
  c(
    paste0("```", lang),
    process_child_nodes(md, ...),
    "```"
  )
}

#' @exportS3Method
to_md.md_block_html = function(md, ...) {
 to_md.md_block(md, ...)
}

#' @exportS3Method
to_md.md_block_p = function(md, ...) {
  process_child_nodes(md, ...)
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

span_text = function(md, ..., collapse="\n") {
  text = unlist(lapply(md, to_md, ...))
  paste(text, collapse=collapse)
}

tag_span = function(tag, md, ..., collapse="\n") {
  checkmate::assert_character(tag, len = 1, any.missing = FALSE)

  tag_close = strsplit(tag, " ")[[1]][1] # strip attributes
  paste0(
    glue::glue("<{tag}>"),
    span_text(md, ..., collapse),
    glue::glue("</{tag_close}>")
  )
}

#' @exportS3Method
to_md.md_span_em = function(md, ...) {
  tag_span("em", md, ...)
}

#' @exportS3Method
to_md.md_span_strong = function(md, ...) {
  tag_span("strong", md, ...)
}

#' @exportS3Method
to_md.md_span_u = function(md, ...) {
  tag_span("u", md, ...)
}

#' @exportS3Method
to_md.md_span_code = function(md, ...) {
  tag_span("code", md, ...)
}

#' @exportS3Method
to_md.md_span_del = function(md, ...) {
  tag_span("del", md, ...)
}

#' @exportS3Method
to_md.md_span_latexmath = function(md, ...) {
  tag_span("x-equation", md, ...)
}

#' @exportS3Method
to_md.md_span_latexmath_display = function(md, ...) {
  tag_span("x-equation type=\"display\"", md, ...)
}

#' @exportS3Method
to_md.md_span_a = function(md, ...) {
  href = attr(md, "href")
  title = attr(md, "title")

  tag = glue::glue("a href=\"{href}\"")
  if (title != "")
    tag = glue::glue("{tag} title=\"{title}\"")

  tag_span(tag, md, ...)
}

#' @exportS3Method
to_md.md_span_img = function(md, ...) {
  src = attr(md, "src")
  title = attr(md, "title")

  # Based on md4c-html.c's approach
  # see https://github.com/mity/md4c/blob/269bbdb31be2225562c802690152f0e08af26181/src/md4c-html.c#L439
  # for details.

  text = paste(unlist(md), collapse="\n")

  glue::glue(
    "<img src=\"{src}\" alt=\"{text}\" title=\"{title}\" />"
  )
}

#' @exportS3Method
to_md.md_span_wikilink = function(md, ...) {
  target = attr(md, "target")
  tag = glue::glue("x-wikilink data-target=\"{target}\"")

  tag_span(tag, md, ...)
}






#############
#
# md_text_*
#
#############

#' @exportS3Method
to_md.md_text_br = function(md, ...) {
  "<br />"
}

#' @exportS3Method
to_md.md_text_softbr = function(md, ...) {
  ""
}

#' @exportS3Method
to_md.md_text = function(md, ...) {
  c(md)
}

# TODO - check breaks'
# TODO - html escaping?


