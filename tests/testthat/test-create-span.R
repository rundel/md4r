test_that("simple span constructors build correct classes", {
  for (fn_name in c("md_span_em", "md_span_strong", "md_span_code", "md_span_del",
                    "md_span_u", "md_span_latexmath", "md_span_latexmath_display")) {
    fn = get(fn_name)
    n = fn(md_text_normal("x"))
    expect_s3_class(n, c(fn_name, "md_span", "md_node"))
    expect_length(n, 1)
  }
})

test_that("span constructors reject non-md_node children", {
  expect_error(md_span_em("not a node"))
  expect_error(md_span_strong(list(a = 1)))
  expect_error(md_span_code(NULL))
})

test_that("md_span_a stores href and title", {
  n = md_span_a(md_text_normal("link"), href = "https://x", title = "t")
  expect_s3_class(n, c("md_span_a", "md_span", "md_node"))
  expect_identical(attr(n, "href"), "https://x")
  expect_identical(attr(n, "title"), "t")
})

test_that("md_span_a validates href and title", {
  expect_error(md_span_a(md_text_normal("x"), title = "t"))
  expect_error(md_span_a(md_text_normal("x"), href = "y"))
  expect_error(md_span_a(md_text_normal("x"), href = NA, title = "t"))
  expect_error(md_span_a(md_text_normal("x"), href = c("a", "b"), title = "t"))
})

test_that("md_span_img stores src and title", {
  n = md_span_img(md_text_normal("alt"), src = "img.png", title = "t")
  expect_s3_class(n, c("md_span_img", "md_span", "md_node"))
  expect_identical(attr(n, "src"), "img.png")
  expect_identical(attr(n, "title"), "t")
})

test_that("md_span_wikilink stores target", {
  n = md_span_wikilink(md_text_normal("Page"), target = "Some Page")
  expect_s3_class(n, c("md_span_wikilink", "md_span", "md_node"))
  expect_identical(attr(n, "target"), "Some Page")
  expect_error(md_span_wikilink(md_text_normal("x"), target = NA))
})

test_that("md4c 0.6.0 span constructors build correct classes and markdown", {
  spans = list(
    md_span_ins     = "++x++",
    md_span_spoiler = "||x||",
    md_span_sup     = "^x^",
    md_span_sub     = "~x~",
    md_span_mark    = "==x=="
  )

  for (fn_name in names(spans)) {
    n = get(fn_name)(md_text_normal("x"))
    expect_s3_class(n, c(fn_name, "md_span", "md_node"))
    expect_identical(as.character(to_md(n)), spans[[fn_name]])
  }

  expect_identical(to_html(md_span_mark(md_text_normal("x"))), "<mark>x</mark>")
})

test_that("md_span_footnote_ref stores and validates attributes", {
  n = md_span_footnote_ref(label = "note", id = 2, ref_id = 3)
  expect_s3_class(n, c("md_span_footnote_ref", "md_span", "md_node"))
  expect_length(n, 0)
  expect_identical(as.character(to_md(n)), "[^note]")
  expect_identical(
    as.character(to_html(n)),
    "<sup><a href=\"#fn-2\" id=\"fnref-2-3\">2</a></sup>"
  )

  expect_error(md_span_footnote_ref())
  expect_error(md_span_footnote_ref(label = NA))
  expect_error(md_span_footnote_ref(label = "a", id = 0))
})
