test_that("parse_md rejects non-character input", {
  expect_error(parse_md(1))
  expect_error(parse_md(TRUE))
  expect_error(parse_md(list("x")))
  expect_error(parse_md(NA_character_))
})

test_that("parse_md accepts empty character vector", {
  expect_silent(parse_md(character()))
})

test_that("parse_md rejects unknown flags", {
  expect_error(parse_md("x", flags = "MD_NOT_A_FLAG"))
  expect_error(parse_md("x", flags = c("MD_DIALECT_COMMONMARK", "MD_BOGUS")))
})

test_that("parse_md accepts known flags", {
  for (flag in flags_available()) {
    expect_no_error(parse_md("x", flags = flag))
  }
})

test_that("parse_gfm uses GFM dialect by default", {
  md = parse_gfm("~~strike~~")
  expect_identical(attr(md, "flags"), "MD_DIALECT_GITHUB")
})

test_that("to_md errors on unsupported S3 class", {
  expect_error(to_md("not a node"))
  expect_error(to_md(list()))
  expect_error(to_md(structure(list(), class = c("md_block_unknown", "md_block", "md_node"))))
})

test_that("to_html errors on unsupported S3 class", {
  expect_error(to_html("not a node"))
  expect_error(to_html(123))
})

test_that("to_md.md_block_p errors on empty children", {
  empty_p = structure(list(), class = c("md_block_p", "md_block", "md_node"))
  expect_error(to_md(empty_p), "no children")
})

test_that("to_md.md_block_li requires prefix and tight", {
  li = md_block_li(md_text_normal("x"))
  expect_error(to_md(li), "prefix")
  expect_error(to_md(li, prefix = "- "), "tight")
})

test_that("flags_used warns on missing flags attribute", {
  doc = structure(list(), class = c("md_block_doc", "md_block", "md_node"))
  expect_warning(flags_used(doc))
})

test_that("flags_used errors on non-md_block_doc", {
  expect_error(flags_used(md_text_normal("x")))
  expect_error(flags_used("not a node"))
})
