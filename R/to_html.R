html_escape = function(x) {
  x = gsub('&', "&amp;", x)
  x = gsub('<', "&lt;", x)
  x = gsub('>', "&gt;", x)
  x = gsub('([^\\])?"', "\\1&quot;", x)

  x
}

tag_filter = function(x) {
  gsub(
    "<(title>|textarea>|style>|xmp>|iframe>|noembed>|noframes>|script>|plaintext>)",
    "&lt;\\1",
    x,
    ignore.case = TRUE
  )
}


#' @title Convert to html
#'
#' @description Coverts a markdown object (full ast or node) to HTML.
#'
#' @param md Markdown object
#' @param ... Unused, for extensibility.
#'
#' @returns Returns a character vector of HTML lines representing the markdown object.
#'
#' @examples
#' md_file = system.file("examples/commonmark.md", package = "md4r")
#' md = parse_md(md_file)
#' cat(to_html(md), sep="\n")
#'
#' @export
#'
to_html = function(md, ...) {
 UseMethod("to_html")
}

#' @exportS3Method
to_html.default = function(md, ...) {
  cli::cli_abort("Unsupported S3 class: {class(md)}")
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

tag_block = function(tag, md, ..., trim = FALSE, collapse = NULL, escape = FALSE) {
  checkmate::assert_character(tag, len = 1, any.missing = FALSE)

  content = unlist(lapply(md, to_html, ...))

  if (!is.null(collapse))
    content = paste(content, collapse = collapse)

  if (trim)
    content = trimws(content)

  tag_close = strsplit(tag, " ")[[1]][1]
  c(
    glue::glue("<{tag}>"),
    content,
    glue::glue("</{tag_close}>")
  )
}

#' @exportS3Method
to_html.md_block_doc = function(md, ...) {
  content = unlist(lapply(md, to_html, ...))

  if (is.null(content)) {
    ""
  } else {
    content
  }

  # Doesnt seem to be needed any more
  #else if ("MD_DIALECT_GITHUB" %in% attr(md, "flags")) {
  #  content = tag_filter(content)
  #}
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

  tag = "ol"
  if (start != 1) {
    tag = glue::glue("ol start=\"{start}\"")
  }
  tag_block(tag, md, ...)
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
  tag_block(tag, md, ..., trim = TRUE, collapse = "")
}

#' @exportS3Method
to_html.md_block_code = function(md, ...) {
  lang = textutils::HTMLdecode(attr(md, "lang"))
  lang = html_escape(lang)

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
  unlist(lapply(md, to_html, ...))
}

#' @exportS3Method
to_html.md_block_p = function(md, ...) {
  tag_block("p", md, ...)
}

#' @exportS3Method
to_html.md_block_hr = function(md, ...) {
  c("<hr />")
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
  text = paste(text, collapse=collapse)
}

tag_span = function(tag, md, ..., collapse="\n") {
  checkmate::assert_character(tag, len = 1, any.missing = FALSE)

  tag_close = strsplit(tag, " ")[[1]][1] # strip attributes
  paste0(
    glue::glue("<{tag}>"),
    span_text(md, ..., collapse=collapse),
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
  tag_span("code", md, ..., collapse="") # Fix CM Ex #335
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
  href = textutils::HTMLdecode(attr(md, "href"))
  href = decodeURI(href)
  href = encodeURI(href)
  href = html_escape(href)

  title = attr(md, "title")
  title = textutils::HTMLdecode(title)
  title = html_escape(title)

  tag = glue::glue("a href=\"{href}\"")
  if (title != "")
    tag = glue::glue("{tag} title=\"{title}\"")

  tag_span(tag, md, ...)
}

#' @exportS3Method
to_html.md_span_img = function(md, ...) {
  src = encodeURI(attr(md, "src"))
  title = html_escape(attr(md, "title"))

  # Based on md4c-html.c's approach
  # see https://github.com/mity/md4c/blob/269bbdb31be2225562c802690152f0e08af26181/src/md4c-html.c#L439
  # for details.

  text = paste(unlist(md), collapse="\n")
  text = textutils::HTMLdecode(text)
  text = html_escape(text)

  tag = glue::glue('<img src="{src}" alt="{text}"')

  if (title != "")
    tag = glue::glue('{tag} title="{title}"')

  glue::glue('{tag} />')
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
to_html.md_text_break = function(md, ...) {
  "<br />"
}

#' @exportS3Method
to_html.md_text_softbr = function(md, ...) {
  ""
}

#' @exportS3Method
to_html.md_text_code = function(md, ...) {
  text = html_escape(md)
  gsub("\\\\\\|", "|", text) # escaped | don't need to be escaped in a code chunk.
}

#' @exportS3Method
to_html.md_text_entity = function(md, ...) {
  char = textutils::HTMLdecode(md)
  if (char == "")
    char = "\uFFFD"

  html_escape(char)
}

#' @exportS3Method
to_html.md_text_normal = function(md, ...) {
  html_escape(md)
}

#' @exportS3Method
to_html.md_text = function(md, ...) {
  c(md)
}

# TODO - check breaks'
# TODO - html escaping?


