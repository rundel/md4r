#' @rdname md_text
#' @name md_text
#'
#' @title Tools for creating text nodes
#'
#' @description These functions are used to create text nodes. Text nodes are
#' used to represent textual elements in markdown.
#'
#' @param x Text content of the node.
#'
#' @returns Returns a character vector with a class of specified type along with `md_text` and `md_node`.
#'
#' @seealso [md_block], [md_span]
#'
NULL

#' @rdname md_text
#' @export
md_text_normal = function(x) {
  checkmate::assert_character(x, len=1, any.missing = FALSE)
  structure(x, class=c("md_text_normal","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_nullchar = function() {
  structure(character(), class=c("md_text_nullchar","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_br = function() {
  structure(character(), class=c("md_text_br","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_softbr = function() {
  structure(character(), class=c("md_text_softbr","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_entity = function(x) {
  checkmate::assert_character(x, len=1, any.missing = FALSE)
  structure(x, class=c("md_text_entity","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_code = function(x) {
  checkmate::assert_character(x, len=1, any.missing = FALSE)
  structure(x, class=c("md_text_code","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_html = function(x) {
  checkmate::assert_character(x, len=1, any.missing = FALSE)
  structure(x, class=c("md_text_html","md_text","md_node"))
}

#' @rdname md_text
#' @export
md_text_latexmath = function(x) {
  checkmate::assert_character(x, len=1, any.missing = FALSE)
  structure(x, class=c("md_text_latexmath","md_text","md_node"))
}
