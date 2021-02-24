clean_html = function(x) {
  x = paste(x, collapse="")
  x = gsub("\n", "", x)
  x = gsub(" />", ">", x) # Some img and a tags differ with usage of this across test cases
}

expect_identical_html = function(md, flags, expected, info = NULL, ...) {
  ast = parse_md(md, flags = flags)
  md_text = trimws(paste(md, collapse="\n"))
  md_text = cli_glue("{.val {md_text}}")
  md_text = paste(md_text, collapse=" ")

  md_html = clean_html( to_html(ast) )
  ex_html = clean_html( expected )

  # Based on testthat:::expect_waldo_equal
  comp = testthat:::waldo_compare(md_html, ex_html, ..., x_arg = "actual", y_arg = "expected")
  comp_txt = paste(comp, collapse = '\n\n', sep = "\n")

  msg = paste(
    paste0(info, ": generated html does not match expected html."),
    "",
    paste0("`markdown`: ", md_text),
    comp_txt,
    sep="\n"
  )

  #expect(
  #  length(comp) == 0,
  #  msg,
  #  trace = "",
  #  trace_env = rlang::caller_env()
  #)

  type = if (length(comp) == 0) "success"
  else "failure"

  exp <- expectation(type, msg, srcref = NULL, trace = NULL)
  exp_signal(exp)

  invisible()
}
