#' @name flags
#' @rdname flags
#'
#' @title Markdown parser flags
#' @description
#' The [`md4c`](https://github.com/mity/md4c) library supports a number of markdown
#' variants / options. The parsing of these is controlled by flags passed to the
#' parser. The following functions provide commonly used utilities for these flags.
#'
#' @returns `flags_available()` returns a character vector of available flags accepted by `parse_md()`.
#'
#' `flags_describe()` returns a tibble with columns `flag` and `description` describing each flag.
#'
#' `flags_used()` returns a character vector of flags used in a parsed markdown document.
#'
#' @examples
#'
#' flags_available()
#'
#' flags_describe()
#'
#' md_file = system.file("examples/commonmark.md", package = "md4r")
#' md = parse_md(md_file)
#' flags_used(md)
#'
NULL

#' @rdname flags
#' @export
flags_available = function() {
  flags_describe()[["flag"]]
}

#' @rdname flags
#' @export
flags_describe = function() {
  tibble::tribble(
    ~flag,                              ~description,
    "MD_FLAG_COLLAPSEWHITESPACE",       "In MD_TEXT_NORMAL, collapse non-trivial whitespace into single ' '",
    "MD_FLAG_PERMISSIVEATXHEADERS",     "Do not require space in ATX headers ( ###header )",
    "MD_FLAG_PERMISSIVEURLAUTOLINKS",   "Recognize URLs as autolinks even without '<', '>'",
    "MD_FLAG_PERMISSIVEEMAILAUTOLINKS", "Recognize e-mails as autolinks even without '<', '>' and 'mailto:'",
    "MD_FLAG_NOINDENTEDCODEBLOCKS",     "Disable indented code blocks. (Only fenced code works.)",
    "MD_FLAG_NOHTMLBLOCKS",             "Disable raw HTML blocks.",
    "MD_FLAG_NOHTMLSPANS",              "Disable raw HTML (inline).",
    "MD_FLAG_TABLES",                   "Enable tables extension.",
    "MD_FLAG_STRIKETHROUGH",            "Enable strikethrough extension.",
    "MD_FLAG_PERMISSIVEWWWAUTOLINKS",   "Enable WWW autolinks (even without any scheme prefix, if they begin with 'www.')",
    "MD_FLAG_TASKLISTS",                "Enable task list extension.",
    "MD_FLAG_LATEXMATHSPANS",           "Enable $ and $$ containing LaTeX equations.",
    "MD_FLAG_WIKILINKS",                "Enable wiki links extensn.",
    "MD_FLAG_UNDERLINE",                "Enable underline extension (and disables '_' for normal emphasis).",
    "MD_FLAG_HARD_SOFT_BREAKS",         "Force all soft breaks to act as hard breaks.",
    "MD_FLAG_PERMISSIVEAUTOLINKS",      "Combines flags (MD_FLAG_PERMISSIVEEMAILAUTOLINKS | MD_FLAG_PERMISSIVEURLAUTOLINKS | MD_FLAG_PERMISSIVEWWWAUTOLINKS)",
    "MD_FLAG_NOHTML",                   "Combines flags (MD_FLAG_NOHTMLBLOCKS | MD_FLAG_NOHTMLSPANS)",
    "MD_DIALECT_COMMONMARK",            "All flags off",
    "MD_DIALECT_GITHUB",                "Combines flags (MD_FLAG_PERMISSIVEAUTOLINKS | MD_FLAG_TABLES | MD_FLAG_STRIKETHROUGH | MD_FLAG_TASKLISTS)"
  )
}

#' @rdname flags
#'
#' @param md Markdown ast object
#'
#' @export
flags_used = function(md) {
  checkmate::assert_class(md, classes = "md_block_doc")
  flags = attr(md, "flags")

  if (is.null(flags)) {
    cli::cli_warn("Unable to determine flags, assuming no flags set (i.e. CommonMark spec).")
    character()
  } else {
    flags
  }
}


flags_check = function(flags, match_case = FALSE) {
  checkmate::assert_character(flags, min.len=0, any.missing = FALSE)

  f = flags
  if (!match_case)
    flags = toupper(flags)

  missing = f[ !flags %in% flags_describe()[["flag"]] ]

  if (length(missing) != 0) {
    cli::cli_abort("Invalid flag{?s} provided: {.val {missing}}")
  }

  invisible()
}

