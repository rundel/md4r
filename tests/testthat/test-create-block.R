test_that("simple block constructors build correct classes", {
  for (fn_name in c("md_block_quote", "md_block_html", "md_block_p",
                    "md_block_hr", "md_block_thead", "md_block_tbody",
                    "md_block_tr")) {
    fn = get(fn_name)
    n = fn()
    expect_s3_class(n, c(fn_name, "md_block", "md_node"))
  }
})

test_that("block constructors reject non-md_node children", {
  expect_error(md_block_p("not a node"))
  expect_error(md_block_quote(123))
})

test_that("md_block_doc stores flags and validates them", {
  n = md_block_doc(flags = "MD_DIALECT_GITHUB")
  expect_s3_class(n, c("md_block_doc", "md_block", "md_node"))
  expect_identical(attr(n, "flags"), "MD_DIALECT_GITHUB")
  expect_error(md_block_doc(flags = "MD_NOT_A_FLAG"))
})

test_that("md_block_h validates level range", {
  for (lvl in 1:6) {
    n = md_block_h(md_text_normal("h"), level = lvl)
    expect_identical(attr(n, "level"), lvl)
  }
  expect_error(md_block_h(md_text_normal("h"), level = 0))
  expect_error(md_block_h(md_text_normal("h"), level = 7))
})

test_that("md_block_code stores info, lang, fence_char and validates", {
  n = md_block_code(info = "r", lang = "r", fence_char = "`")
  expect_identical(attr(n, "info"), "r")
  expect_identical(attr(n, "lang"), "r")
  expect_identical(attr(n, "fence_char"), "`")
  expect_error(md_block_code(fence_char = "``"))
  expect_error(md_block_code(info = NA))
})

test_that("md_block_ul accepts tight 0/1 and stores mark", {
  n = md_block_ul(tight = 1, mark = "-")
  expect_identical(as.integer(attr(n, "tight")), 1L)
  expect_identical(attr(n, "mark"), "-")
  expect_error(md_block_ul(tight = 2, mark = "-"))
  expect_error(md_block_ul(tight = 1, mark = NA))
})

test_that("md_block_ol enforces mark_delimiter to '.' or ')'", {
  expect_silent(md_block_ol(start = 1, mark_delimiter = "."))
  expect_silent(md_block_ol(start = 1, mark_delimiter = ")"))
  expect_error(md_block_ol(start = 1, mark_delimiter = ":"))
  expect_error(md_block_ol(start = -1, mark_delimiter = "."))
})

test_that("md_block_li enforces task_mark to ' ', 'x', 'X'", {
  expect_silent(md_block_li(is_task = 1, task_mark = " "))
  expect_silent(md_block_li(is_task = 1, task_mark = "x"))
  expect_silent(md_block_li(is_task = 1, task_mark = "X"))
  expect_error(md_block_li(is_task = 1, task_mark = "y"))
  expect_error(md_block_li(is_task = 2))
})

test_that("md_block_table stores counts and rejects head_row_count != 1", {
  n = md_block_table(col_count = 2, body_row_count = 3)
  expect_identical(attr(n, "col_count"), 2)
  expect_identical(attr(n, "body_row_count"), 3)
  expect_identical(attr(n, "head_row_count"), 1)
  expect_error(md_block_table(col_count = 2, body_row_count = 3, head_row_count = 2))
})

test_that("md_block_th and md_block_td accept align values", {
  for (a in c("default", "left", "center", "right")) {
    expect_identical(attr(md_block_th(align = a), "align"), a)
    expect_identical(attr(md_block_td(align = a), "align"), a)
  }
  expect_error(md_block_th(align = "justify"))
})
