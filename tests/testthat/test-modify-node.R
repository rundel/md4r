test_that("$.md_node returns content with attributes stripped", {
  ul = md_block_ul(
    md_block_li(md_text_normal("a")),
    md_block_li(md_text_normal("b"))
  )

  expect_type(ul$content, "list")
  expect_null(attributes(ul$content))
  expect_length(ul$content, 2)
  expect_s3_class(ul$content[[1]], "md_block_li")
})

test_that("$.md_node returns existing attributes by name", {
  h = md_block_h(md_text_normal("title"), level = 3L)
  expect_equal(h$level, 3L)

  ol = md_block_ol(md_block_li(md_text_normal("x")), start = 5L)
  expect_equal(ol$start, 5L)
  expect_identical(ol$mark_delimiter, ".")

  a = md_span_a(md_text_normal("link"), href = "/u", title = "t")
  expect_identical(a$href, "/u")
  expect_identical(a$title, "t")
})

test_that("$.md_node errors on unknown attribute and on class/names", {
  h = md_block_h(md_text_normal("x"), level = 1)
  expect_error(h$nope, "valid attribute")
  expect_error(h$class, "valid attribute")
  expect_error(h$names, "valid attribute")
})

test_that("$.md_node on an md_text returns bare character content", {
  t = md_text_normal("hello")
  expect_identical(t$content, "hello")
  expect_null(attributes(t$content))
})

test_that("$<-.md_node replaces content while preserving class and attributes", {
  ul = md_block_ul(md_block_li(md_text_normal("old")), mark = "*")
  ul$content = list(md_block_li(md_text_normal("new")))

  expect_s3_class(ul, "md_block_ul")
  expect_identical(attr(ul, "mark"), "*")
  expect_length(ul, 1)
  expect_identical(md_text_summary(ul), "new")
})

test_that("$<-.md_node accepts a single md_node as content", {
  p = md_block_p(md_text_normal("a"))
  p$content = md_text_normal("b")
  expect_s3_class(p, "md_block_p")
  expect_identical(md_text_summary(p), "b")
})

test_that("$<-.md_node rejects non-node content", {
  p = md_block_p(md_text_normal("a"))
  expect_error(p$content <- 1L, "Must")
  expect_error(p$content <- list(1, 2), "md_node")
})

test_that("$<-.md_node updates existing attributes with validation", {
  h = md_block_h(md_text_normal("x"), level = 2L)
  h$level = 4L
  expect_identical(attr(h, "level"), 4L)

  expect_error(h$level <- 7L, "<= 6")
  expect_error(h$level <- "two", "integerish")

  ul = md_block_ul(md_block_li(md_text_normal("x")), mark = "*")
  ul$mark = "-"
  expect_identical(attr(ul, "mark"), "-")

  ol = md_block_ol(md_block_li(md_text_normal("x")), start = 1L)
  ol$mark_delimiter = ")"
  expect_identical(attr(ol, "mark_delimiter"), ")")
  expect_error(ol$mark_delimiter <- "?", "pattern")

  th = md_block_th(md_text_normal("x"), align = "left")
  th$align = "center"
  expect_identical(attr(th, "align"), "center")
  expect_error(th$align <- "middle", "Must be element of")

  doc = md_block_doc(md_block_p(md_text_normal("x")))
  doc$flags = "MD_DIALECT_GITHUB"
  expect_identical(attr(doc, "flags"), "MD_DIALECT_GITHUB")
  expect_error(doc$flags <- "BOGUS_FLAG")
})

test_that("$<-.md_node rejects unknown and protected attributes", {
  h = md_block_h(md_text_normal("x"), level = 1)
  expect_error(h$bogus <- 1, "existing attribute")
  expect_error(h$class <- "md_block_p", "Cannot set")
  expect_error(h$names <- c("a"), "Cannot set")
})

test_that("$<-.md_text replaces content for content-bearing text classes", {
  for (ctor in list(md_text_normal, md_text_code, md_text_html, md_text_entity, md_text_latexmath)) {
    t = ctor("old")
    t$content = "new"
    expect_identical(unclass(t), "new")
    expect_identical(class(t)[1], class(ctor("x"))[1])
  }
})

test_that("$<-.md_text rejects content on empty text classes", {
  br = md_text_break()
  sb = md_text_softbreak()
  nc = md_text_nullchar()
  expect_error(br$content <- "x", "do not have content")
  expect_error(sb$content <- "x", "do not have content")
  expect_error(nc$content <- "x", "do not have content")
})

test_that("$<-.md_text rejects non-content names and bad values", {
  t = md_text_normal("hi")
  expect_error(t$bogus <- "x", "Only.*content")
  expect_error(t$content <- 1, "character")
  expect_error(t$content <- c("a", "b"), "length 1")
})

test_that("chained $<- assignment propagates through nested nodes", {
  doc = md_block_ul(
    md_block_li(md_text_normal("a")),
    md_block_li(md_text_normal("b"))
  )
  doc$content[[1]]$content[[1]]$content = "first"
  expect_identical(md_text_summary(doc$content[[1]]), "first")

  doc$mark = "-"
  expect_match(paste(to_md(doc), collapse = "\n"), "^- first")
})
