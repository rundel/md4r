#' @title Convert a markdown object to html
#' @description
#'
#' @param md Markdown object
#' @param ... Unused, for extensibility.
#'
#' @export
to_html = function(md, ...) {
 UseMethod("to_html")
}

#' @exportS3Method
to_html.default = function(md, ...) {
  stop("Unsupported S3 class", call. = FALSE)
}


#' @exportS3Method
to_html.md_node = function(md, ...) {
  unlist(lapply(md, to_html, ...))
}


#############
#
# md_block_*
#
#############

tag_block = function(tag, md, ...) {
  checkmate::assert_character(tag, len = 1, any.missing = FALSE)

  tag_close = strsplit(tag, " ")[[1]][1]
  c(
    glue::glue("<{tag}>"),
    unlist(lapply(md, to_html, ...)),
    glue::glue("</{tag_close}>")
  )
}

#' @exportS3Method
to_html.md_block = function(md, ...) {
  stop("Unknown block class: ", class(md)[1], call. = FALSE)
}

#' @exportS3Method
to_html.md_block_doc = function(md, ...) {
  tag_block("html", md, ...)
}


#' @exportS3Method
to_html.md_block_quote = function(md, ...) {
  tag_block("blockquote", md, ...)
}

#' @exportS3Method
to_html.md_block_ul = function(md, ...) {
  tag_block("ul", md, ...)
}

#' @exportS3Method
to_html.md_block_ol = function(md, ...) {
  start = attr(md, "start")
  c(
    glue::glue("<ol start=\"{start}\">"),
    unlist(lapply(md, to_html, ...)),
    glue::glue("</ol>")
  )
}

#' @exportS3Method
to_html.md_block_li = function(md, ...) {
  if (attr(md, "is_task")) {
    checked = ifelse(tolower(attr(md, "task_mark")) == "x", " checked", "")
    tag = c(
      "<li class=\"task-list-item\">",
      glue::glue("<input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled{checked}>")
    )
  } else {
    tag = "<li>"
  }

  c(
    tag,
    unlist(lapply(md, to_html, ...)),
    "</li>"
  )
}

#' @exportS3Method
to_html.md_block_h = function(md, ...) {
  tag = paste0("h", attr(md, "level"))
  tag_block(tag, md, ...)
}

#' @exportS3Method
to_html.md_block_code = function(md, ...) {
  lang = attr(md, "lang")
  if (lang != "") {
    tag = glue::glue("<pre><code class=\"language-{lang}\">")
  } else {
    tag = "<pre><code>"
  }

  c(
    tag,
    unlist(lapply(md, to_html, ...)),
    "</code></pre>"
  )
}

#' @exportS3Method
to_html.md_block_html = function(md, ...) {
 to_html.md_block(md, ...)
}

#' @exportS3Method
to_html.md_block_p = function(md, ...) {
  tag_block("p", md, ...)
}

#' @exportS3Method
to_html.md_block_table = function(md, ...) {
  tag_block("table", md, ...)
}

#' @exportS3Method
to_html.md_block_thead = function(md, ...) {
  tag_block("thead", md, ...)
}

#' @exportS3Method
to_html.md_block_tbody = function(md, ...) {
  tag_block("tbody", md, ...)
}

#' @exportS3Method
to_html.md_block_tr = function(md, ...) {
  tag_block("tr", md, ...)
}

td_block = function(type, md, ...) {
  align = attr(md, "align")
  if (is.null(align) || align == "default") {
    tag = glue::glue("<{type}>")
  } else {
    tag = glue::glue("<{type} align=\"{align}\">")
  }

  c(
    tag,
    unlist(lapply(md, to_html, ...)),
    glue::glue("</{type}>")
  )
}

#' @exportS3Method
to_html.md_block_th = function(md, ...) {
  td_block("th", md, ...)
}

#' @exportS3Method
to_html.md_block_td = function(md, ...) {
  td_block("td", md, ...)
}

#############
#
# md_span_*
#
#############

span_text = function(md, ..., collapse="\n") {
  text = unlist(lapply(md, to_html, ...))
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
to_html.md_span_em = function(md, ...) {
  tag_span("em", md, ...)
}

#' @exportS3Method
to_html.md_span_strong = function(md, ...) {
  tag_span("strong", md, ...)
}

#' @exportS3Method
to_html.md_span_u = function(md, ...) {
  tag_span("u", md, ...)
}

#' @exportS3Method
to_html.md_span_code = function(md, ...) {
  tag_span("code", md, ...)
}

#' @exportS3Method
to_html.md_span_del = function(md, ...) {
  tag_span("del", md, ...)
}

#' @exportS3Method
to_html.md_span_latexmath = function(md, ...) {
  tag_span("x-equation", md, ...)
}

#' @exportS3Method
to_html.md_span_latexmath_display = function(md, ...) {
  tag_span("x-equation type=\"display\"", md, ...)
}

#' @exportS3Method
to_html.md_span_a = function(md, ...) {
  href = attr(md, "href")
  title = attr(md, "title")

  tag = glue::glue("a href=\"{href}\"")
  if (title != "")
    tag = glue::glue("{tag} title=\"{title}\"")

  tag_span(tag, md, ...)
}

#' @exportS3Method
to_html.md_span_img = function(md, ...) {
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
to_html.md_span_wikilink = function(md, ...) {
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
to_html.md_text = function(md, ...) {
  c(md)
}


