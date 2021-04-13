test_that("table_entry_escape", {
  expect_equal( table_entry_escape("|"), "\\|")
  expect_equal( table_entry_escape(" | "), " \\| ")

  expect_equal( table_entry_escape("`|`"), "`|`")

  expect_equal( table_entry_escape("[[|"), "[[|")
  expect_equal( table_entry_escape("[[|]"), "[[|]")
  expect_equal( table_entry_escape("[[|]]"), "[[|]]")
  expect_equal( table_entry_escape("[][|]"), "[][|]")

  expect_equal( table_entry_escape("]|"), "]\\|")
  expect_equal( table_entry_escape("]]|"), "]]\\|")
  expect_equal( table_entry_escape("]]|[["), "]]\\|[[")
  expect_equal( table_entry_escape("[]|"), "[]\\|")

  expect_equal( table_entry_escape("\\|"), "\\|")
})
