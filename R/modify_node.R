#' @rdname md_node_modify
#' @name md_node_modify
#'
#' @title Tools for modifying `md_node` objects
#'
#' @description Overrides the `$` operator so the content and attributes of
#' `md_node` objects can be read and updated by name. Use `node$content` to
#' access or replace the children of a block or span node (or the text of a
#' text node), and `node$<attr>` to read or update an existing node attribute
#' (e.g. `level`, `href`, `mark`).
#'
#' Only attributes already present on the node may be replaced, and the new
#' value is validated against the same rules used by the corresponding
#' `md_block_*` / `md_span_*` constructor. The `class` and `names` attributes
#' cannot be set this way.
#'
#' @param x An object that inherits from `md_node`.
#' @param name A character string giving the attribute to access, or
#'   `"content"` to access the node's children / text content.
#' @param value Replacement value for the content or attribute.
#'
#' @returns A modified `md_node` object.
#'
#' @examples
#' md = md_text_normal("Hello World")
#' md$content = "Nice to meet you"
#' md
#'
#' md = md_block_ul(
#'   md_block_li(md_text_normal("bullet 1")),
#'   md_block_li(md_text_normal("bullet 2"))
#' )
#' md$mark = "-"
#' md$content[[1]]$content[[1]]$content = "first"
#' to_md(md) |> cat(sep = "\n")
#'
NULL


md_text_empty_classes = c("md_text_break", "md_text_softbreak", "md_text_nullchar")

md_node_attr_validators = list(
  flags          = function(v) flags_check(v),
  info           = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  lang           = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  fence_char     = function(v) checkmate::assert_character(v, len = 1, n.chars = 1, any.missing = FALSE),
  tight          = function(v) checkmate::assert_integerish(v, len = 1, lower = 0, upper = 1),
  mark           = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  start          = function(v) checkmate::assert_integerish(v, len = 1, lower = 0),
  mark_delimiter = function(v) checkmate::assert_character(v, len = 1, n.chars = 1, pattern = "\\.|\\)", any.missing = FALSE),
  is_task        = function(v) checkmate::assert_integerish(v, len = 1, lower = 0, upper = 1),
  task_mark      = function(v) checkmate::assert_character(v, len = 1, n.chars = 1, any.missing = FALSE, pattern = " |x|X"),
  level          = function(v) checkmate::assert_integerish(v, len = 1, lower = 1, upper = 6),
  col_count      = function(v) checkmate::assert_integerish(v, len = 1, lower = 0),
  body_row_count = function(v) checkmate::assert_integerish(v, len = 1, lower = 0),
  head_row_count = function(v) checkmate::assert_integerish(v, len = 1, lower = 1, upper = 1),
  align          = function(v) checkmate::assert_choice(v, c("default", "left", "center", "right")),
  href           = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  title          = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  src            = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE),
  target         = function(v) checkmate::assert_character(v, len = 1, any.missing = FALSE)
)


#' @rdname md_node_modify
#' @export
`$.md_node` = function(x, name) {
  checkmate::assert_character(name, len = 1, any.missing = FALSE)

  if (name == "content") {
    attributes(x) = NULL
    x
  } else if (name %in% setdiff(names(attributes(x)), c("class", "names"))) {
    attr(x, name)
  } else {
    cli::cli_abort(
      "{.val {name}} is not a valid attribute of a {.cls {class(x)[1]}} node; use {.code $content} or an existing attribute name."
    )
  }
}


#' @rdname md_node_modify
#' @export
`$<-.md_node` = function(x, name, value) {
  checkmate::assert_character(name, len = 1, any.missing = FALSE)

  if (name == "content") {
    checkmate::assert(
      checkmate::check_class(value, "md_node"),
      checkmate::check_list(value, types = "md_node")
    )
    if (inherits(value, "md_node")) value = list(value)
    attributes(value) = attributes(x)
    return(value)
  }

  if (name %in% c("class", "names")) {
    cli::cli_abort("Cannot set {.val {name}} via {.code $<-}.")
  }

  if (!name %in% names(attributes(x))) {
    cli::cli_abort(
      "{.val {name}} is not an existing attribute of a {.cls {class(x)[1]}} node."
    )
  }

  validator = md_node_attr_validators[[name]]
  if (!is.null(validator)) validator(value)

  attr(x, name) = value
  x
}


#' @rdname md_node_modify
#' @export
`$<-.md_text` = function(x, name, value) {
  checkmate::assert_character(name, len = 1, any.missing = FALSE)

  if (name != "content") {
    cli::cli_abort(
      "Only {.code $content} may be set on a {.cls md_text} node; got {.val {name}}."
    )
  }

  if (inherits(x, md_text_empty_classes)) {
    cli::cli_abort(
      "{.cls {class(x)[1]}} nodes do not have content."
    )
  }

  checkmate::assert_character(value, len = 1, any.missing = FALSE)
  attributes(value) = attributes(x)
  value
}
