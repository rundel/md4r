test_that("md_n_nodes counts nodes in a parsed document", {
  md = parse_md("hello\n")
  expect_gt(md4r:::md_n_nodes(md), 0)
  expect_error(md4r:::md_n_nodes("not a node"))
})

test_that("md_text extracts text content from a parsed document", {
  md = parse_md("# Title\n\nbody **text**\n")
  txt = md4r:::md_text(md)
  expect_true(any(grepl("Title", unlist(txt))))
  expect_true(any(grepl("body",  unlist(txt))))
  expect_true(any(grepl("text",  unlist(txt))))
})

test_that("md_text_summary truncates long text with an ellipsis", {
  long = strrep("x", 200)
  md = parse_md(long)
  out = md4r:::md_text_summary(md, max_width = 10)
  expect_true(nchar(out) >= 10)
  expect_true(grepl("\\.\\.\\.|…$", out))
})

test_that("md_text_summary replaces newlines per replace_newline", {
  md = parse_md(c("a", "b"))
  out = md4r:::md_text_summary(md, replace_newline = "|")
  expect_false(grepl("\n", out))
})
