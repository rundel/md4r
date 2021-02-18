map_ast = function(x, f = c, classes = "md_node", res=list()) {
  checkmate::assert_class(x, "md_node")
  checkmate::assert_class(f, "function")
  checkmate::assert_character(classes, any.missing = FALSE, min.len = 1)

  for(i in seq_along(x)) {
    if (inherits(x[[i]], classes)) {
      res = c(res, f(x[[i]]))
    } else if (inherits(x[[i]], "md_node")) {
      res = c(res, map_ast(x[[i]], f, classes))
    }
  }

  res
}
