#' @rdname md_block
#' @name md_block
#'
#' @title Tools for creating block nodes
#'
#' @description These functions are used to create block nodes. Blocks nodes are
#' used to represent block level elements in markdown.
#'
#' @param ... Child nodes that will be contained within the block - all must inherit from `md_node`.
#' @param flags Used by `md_block_doc` to specify the document parser flag(s).
#' @param info Used by `md_block_code` nodes to specify the code block info string.
#' @param lang Used by `md_block_code` nodes to specify the code block language.
#' @param fence_char Used by `md_block_code` nodes to specify the code block fence character.
#' @param tight Used by `md_block_ul` or `md_block_ol` nodes to specify whether the list is tight or loose.
#' @param mark Used by `md_block_ul` nodes to specify the list marker.
#' @param start Used by `md_block_ol` nodes to specify the start number of the list.
#' @param mark_delimiter Used by `md_block_ol` nodes to specify the delimiter between the value and text.
#' Only `"."` and `")"` are allowed.
#' @param is_task Used by `md_block_li` nodes to specify whether the item is a task.
#' @param task_mark Used by `md_block_li` task nodes to specify the task mark character.
#' Only `" "`, `"x"`, and `"X"` are allowed.
#' @param level Used by `md_block_h` nodes to specify the heading level.
#' @param col_count Used by `md_block_table` nodes to specify the number of columns.
#' @param body_row_count Used by `md_block_table` nodes to specify the number of body rows.
#' @param head_row_count Used by `md_block_table` nodes to specify the number of header rows. Should only be `1`.
#' @param align Used by `md_block_td` or `md_block_th` nodes to specify the alignment of the table contents.
#'
#' @returns Returns a list with a class of specified type along with `md_span` and `md_node`.
#'
#' @seealso [md_span], [md_text]
#'
NULL

md_block = function(..., class) {
  nodes = list(...)
  checkmate::assert_list(nodes, types = "md_node", .var.name = "...")
  checkmate::assert_character(class, len = 1, any.missing = FALSE)
  structure(nodes, class = c(class, "md_block", "md_node"))
}

#' @rdname md_block
#' @export
md_block_doc = function(..., flags = "MD_DIALECT_COMMONMARK") {
  flags_check(flags)
  structure(
    md_block(..., class = "md_block_doc"),
    flags = flags
  )
}

#' @rdname md_block
#' @export
md_block_quote = function(...) {
  md_block(..., class = "md_block_quote")
}

#' @rdname md_block
#' @export
md_block_html = function(...) {
  md_block(..., class = "md_block_html")
}

#' @rdname md_block
#' @export
md_block_p = function(...) {
  md_block(..., class = "md_block_p")
}

#' @rdname md_block
#' @export
md_block_hr = function(...) {
  md_block(..., class = "md_block_hr")
}

#' @rdname md_block
#' @export
md_block_code = function(..., info = "", lang = "", fence_char = "`") {
  checkmate::assert_character(info, len = 1, any.missing = FALSE)
  checkmate::assert_character(lang, len = 1, any.missing = FALSE)
  checkmate::assert_character(fence_char, len = 1, n.chars = 1, any.missing = FALSE)
  structure(
    md_block(..., class = "md_block_code"),
    info = info,
    lang = lang,
    fence_char = fence_char
  )
}

#' @rdname md_block
#' @export
md_block_ul = function(..., tight = 1, mark = "*") {
  tight = as.integer(tight)
  checkmate::assert_integerish(tight, len = 1, lower = 0, upper = 1)
  checkmate::assert_character(mark, len = 1, any.missing = FALSE)
  structure(
    md_block(..., class = "md_block_ul"),
    tight = tight,
    mark = mark
  )
}

#' @rdname md_block
#' @export
md_block_ol = function(..., tight = 1L, start = 1, mark_delimiter = ".") {
  checkmate::assert_integerish(tight, len = 1, lower = 0, upper = 1)
  checkmate::assert_integerish(start, len = 1, lower = 0)
  checkmate::assert_character(mark_delimiter, len = 1, n.chars = 1,
                              pattern = "\\.|\\)", any.missing = FALSE)
  structure(
    md_block(..., class = "md_block_ol"),
    tight = tight,
    start = start,
    mark_delimiter = mark_delimiter
  )
}

#' @rdname md_block
#' @export
md_block_li = function(..., is_task = 0, task_mark = " ") {
  checkmate::assert_integerish(is_task, len = 1, lower = 0, upper = 1)
  checkmate::assert_character(task_mark, len = 1, n.chars = 1, any.missing = FALSE, pattern = " |x|X")
  structure(
    md_block(..., class = "md_block_li"),
    is_task = is_task,
    task_mark = task_mark
  )
}

#' @rdname md_block
#' @export
md_block_h = function(..., level) {
  checkmate::assert_integerish(level, len = 1, lower = 1, upper = 6)
  structure(
    md_block(..., class = "md_block_h"),
    level = level
  )
}

#' @rdname md_block
#' @export
md_block_table = function(..., col_count, body_row_count, head_row_count = 1) {
  checkmate::assert_integerish(col_count, len = 1, lower = 0)
  checkmate::assert_integerish(body_row_count, len = 1, lower = 0)
  checkmate::assert_integerish(head_row_count, len = 1, lower = 1, upper = 1)
  structure(
    md_block(..., class = "md_block_table"),
    col_count = col_count,
    body_row_count = body_row_count,
    head_row_count = head_row_count
  )
}

#' @rdname md_block
#' @export
md_block_thead = function(...) {
  md_block(..., class = "md_block_thead")
}

#' @rdname md_block
#' @export
md_block_tbody = function(...) {
  md_block(..., class = "md_block_tbody")
}

#' @rdname md_block
#' @export
md_block_tr = function(...) {
  md_block(..., class = "md_block_tr")
}

#' @rdname md_block
#' @export
md_block_th = function(..., align = c("default", "left", "center", "right")) {
  align = match.arg(align)
  structure(
    md_block(..., class = "md_block_th"),
    align = align
  )
}

#' @rdname md_block
#' @export
md_block_td = function(..., align = c("default", "left", "center", "right")) {
  align = match.arg(align)
  structure(
    md_block(..., class = "md_block_td"),
    align = align
  )
}
