test_that("md_text_normal builds correct class and content", {
  n = md_text_normal("hello")
  expect_s3_class(n, c("md_text_normal", "md_text", "md_node"))
  expect_identical(unclass(n), "hello")
})

test_that("md_text_normal validates input", {
  expect_error(md_text_normal(123))
  expect_error(md_text_normal(NA_character_))
  expect_error(md_text_normal(c("a", "b")))
  expect_error(md_text_normal(character()))
})

test_that("md_text_code, md_text_html, md_text_entity, md_text_latexmath build correct classes", {
  for (fn in list(md_text_code, md_text_html, md_text_entity, md_text_latexmath)) {
    n = fn("x")
    expect_s3_class(n, c("md_text", "md_node"))
    expect_identical(unclass(n), "x")
    expect_error(fn(NA_character_))
    expect_error(fn(c("a", "b")))
  }
})

test_that("md_text_break, md_text_softbreak, md_text_nullchar produce empty text nodes", {
  for (fn in list(md_text_break, md_text_softbreak, md_text_nullchar)) {
    n = fn()
    expect_s3_class(n, c("md_text", "md_node"))
    expect_identical(unclass(n), character())
  }
})

test_that("constructor classes match parser output", {
  parsed = parse_md("a\nb")
  matched = md4r:::map_ast(parsed, f = list, classes = "md_text")
  classes = unique(vapply(matched, function(x) class(x)[1], character(1)))
  expect_true("md_text_normal" %in% classes)
  expect_true("md_text_softbreak" %in% classes)
})
