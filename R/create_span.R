#' @rdname md_span
#' @name md_span
#'
#' @title Tools for creating span nodes
#'
#' @description These functions are used to create span nodes. Span nodes are
#' used to represent inline elements in markdown and include things like links, images, code,
#' emphasized text, strong text, and more.
#'
#' @param ... Child nodes that will be contained within the span - all must inherit from `md_node`.
#' @param title Used by `md_span_a` or `md_span_img` nodes to provide a `title` attribute.
#' @param href Used by `md_span_a` nodes to provide the `href` attribute.
#' @param src Used by `md_span_img` nodes to provide the `src` attribute.
#' @param target Used by `md_span_wikilink` nodes to provide the `target` attribute.
#'
#' @returns Returns a list with a class of specified type along with `md_span` and `md_node`.
#'
#' @seealso [md_block], [md_text]
#'
NULL

#' @rdname md_span
#' @export
md_span_em = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_em", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_strong = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_strong", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_a = function(..., href, title) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  checkmate::assert_character(title, len=1, any.missing = FALSE)
  checkmate::assert_character(href, len=1, any.missing = FALSE)

  structure(
    nodes,
    title = title,
    href = href,
    class = c("md_span_a", "md_span", "md_node")
  )
}

#' @rdname md_span
#' @export
md_span_img = function(..., src, title) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  checkmate::assert_character(title, len=1, any.missing = FALSE)
  checkmate::assert_character(src, len=1, any.missing = FALSE)

  structure(
    nodes,
    title = title,
    src = src,
    class = c("md_span_img", "md_span", "md_node")
  )
}

#' @rdname md_span
#' @export
md_span_code = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_code", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_del = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_del", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_latexmath = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_latexmath", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_latexmath_display = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_latexmath_display", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_wikilink = function(..., target) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  checkmate::assert_character(target, len=1, any.missing = FALSE)

  structure(
    nodes,
    target = target,
    class = c("md_span_wikilink", "md_span", "md_node")
  )
}

#' @rdname md_span
#' @export
md_span_u = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_u", "md_span", "md_node"))
}
