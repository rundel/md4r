test_that("header flags", {
  h = readLines(system.file("include/md4c.h", package="md4r"))
  flag_lines = h[grepl("#define MD_(FLAG|DIALECT)_[A-Z_]+", h)]
  flags = sub("^#define (MD_(FLAG|DIALECT)_[A-Z_]+).*$", "\\1", flag_lines)

  expect_identical(sort(flags),  sort(flags_available()))
})

test_that("flags_check", {
  expect_null(flags_check(flags_available()))
  expect_null(flags_check(tolower(flags_available())))

  expect_error(flags_check(tolower(flags_available()), match_case = TRUE))

  expect_error(flags_check())
  expect_error(flags_check(NA))

  expect_null(flags_check("MD_DIALECT_GITHUB"))
  expect_error(flags_check(c("MD_DIALECT_GITHUB", NA)))
})

test_that("all flag masks exist", {
  for (flag in flags_available()) {
    expect_no_error(flag_mask(flag))
  }
})

