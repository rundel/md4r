test_that("Issue #3", {
  md = "# Header 1

## Header 2

- Bullet 1.
- Bullet 2."

  expect_equal(
    md,
    paste(to_md(parse_md(md)), collapse="\n")
  )
})

test_that("Issue #8", {
  md = "- `my_fun()` replaces
- `my_fun()` replaces"

  expect_equal(
    md,
    paste(to_md(parse_md(md)), collapse="\n")
  )
})

test_that("Issue #9", {
  md = "- Test `func()`."

  expect_equal(
    md,
    paste(to_md(parse_md(md)), collapse="\n")
  )
})
