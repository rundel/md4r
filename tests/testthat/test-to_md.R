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
