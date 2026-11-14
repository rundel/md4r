# Changes in md4r 0.6.0.0

* Updated md4c to version 0.6.0.

* Added support for the new md4c extensions (footnotes, admonitions, preserved blank lines, highlight, insert, spoilers, superscripts, and subscripts), each with a parser flag, node class, constructor, and `to_md()` / `to_html()` methods.

* `MD_DIALECT_GITHUB` (and so `parse_gfm()`) now also enables footnotes and admonitions, following upstream.

* `to_html()` now renders surrogate and out of range numeric character references as U+FFFD instead of erroring.

* `to_md()` round trip fixes: strikethrough is written as `~~text~~`, indented code following a list stays out of the list, `[` and `]` are escaped in wiki link targets, and emphasis starting with `*` uses `_`.

# Changes in md4r 0.5.3.0

* Updated md4c to version 0.5.3.

* `to_html()` no longer doubles the `language-` prefix on a fenced code block
  whose info string already starts with `language-`, matching the upstream
  md4c-html behavior.

* Added `$` and `$<-` methods for `md_node` objects to read and modify node
  content and attributes by name.

# Changes in md4r 0.5.2.1

* Renamed `md_text_br()` and `md_text_softbr()` to `md_text_break()` and
  `md_text_softbreak()` so constructor classes match parser output.

* Added `md_text_*`, `md_span_*`, and `md_block_*` functions to create markdown elements.

* Switched spec-based tests to use code generation (thanks to @krlmlr).

* Clearer errors in `to_md()` for empty `md_block_p` and for `md_block_li`
  missing required arguments.

* C++ parser now emits an `md_span_other` fallback for unrecognized spans.

* Added `rlang` to `Suggests`; test regeneration skipped cleanly when
  `rlang`/`styler` are missing.

* Fixed wrong dependency vector in `helper-tests.R` md4c test regeneration.

# Changes in md4r 0.5.2.0

* Initial CRAN submission.

* Based on md4c version 0.5.2

* Implemented functionality for parsing markdown, displaying the AST, and generating html or md
