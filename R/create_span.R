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
#' @param label Used by `md_span_footnote_ref` nodes to provide the footnote label (e.g. `"1"` for `[^1]`).
#' @param id Used by `md_span_footnote_ref` nodes to provide the 1-based number of the referenced footnote.
#' @param ref_id Used by `md_span_footnote_ref` nodes to provide the 1-based index of this reference
#' among all references to the same footnote.
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

#' @rdname md_span
#' @export
md_span_ins = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_ins", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_spoiler = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_spoiler", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_sup = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_sup", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_sub = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_sub", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_mark = function(...) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  structure(nodes, class = c("md_span_mark", "md_span", "md_node"))
}

#' @rdname md_span
#' @export
md_span_footnote_ref = function(label, id = 1, ref_id = 1) {
  checkmate::assert_character(label, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(id, len = 1, lower = 1, any.missing = FALSE)
  checkmate::assert_integerish(ref_id, len = 1, lower = 1, any.missing = FALSE)

  structure(
    list(),
    id = id,
    ref_id = ref_id,
    label = label,
    class = c("md_span_footnote_ref", "md_span", "md_node")
  )
}
