test_that("map_ast with f=list returns matched nodes intact", {
  md = parse_md("# A\n\n## B\n\n### C\n")
  hs = md4r:::map_ast(md, f = list, classes = "md_block_h")
  expect_length(hs, 3)
  expect_true(all(vapply(hs, inherits, logical(1), "md_block_h")))
})

test_that("map_ast default f=c flattens matched-node contents", {
  md = parse_md("# A\n## B\n")
  hs = md4r:::map_ast(md, classes = "md_block_h")
  expect_true(all(vapply(hs, inherits, logical(1), "md_text_normal")))
})

test_that("map_ast applies the function to matched nodes", {
  md = parse_md("**a** *b* **c**\n")
  strongs = md4r:::map_ast(md, function(x) attr(x, "class")[1], classes = "md_span_strong")
  expect_true(all(unlist(strongs) == "md_span_strong"))
})

test_that("map_ast walks into nested structures", {
  md = parse_md("> outer **inner** text\n")
  strongs = md4r:::map_ast(md, classes = "md_span_strong")
  expect_length(strongs, 1)
})

test_that("map_ast returns empty list when no matches", {
  md = parse_md("plain text\n")
  expect_identical(md4r:::map_ast(md, classes = "md_block_table"), list())
})

test_that("map_ast validates inputs", {
  expect_error(md4r:::map_ast("not a node"))
  expect_error(md4r:::map_ast(parse_md("x"), f = "not a function"))
  expect_error(md4r:::map_ast(parse_md("x"), classes = NA_character_))
})
