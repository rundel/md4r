# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

`md4r` is an R package wrapping the [MD4C](https://github.com/mity/md4c) C library to parse markdown (CommonMark + extensions like GFM, LaTeX, tables) into an R-side abstract syntax tree, with utilities to print, manipulate, and convert the AST back to markdown or HTML.

## Common commands

Build, install, test, and check are standard R package workflows. Use `Rscript -e` to run them:

```sh
Rscript -e 'devtools::load_all()'           # load package for interactive use
Rscript -e 'devtools::document()'           # regenerate man/ and NAMESPACE from roxygen
Rscript -e 'devtools::test()'               # run all testthat tests
Rscript -e 'devtools::test(filter = "to_md")'  # run a single test file (test-to_md*.R)
Rscript -e 'testthat::test_file("tests/testthat/test-flags.R")'  # run one specific file
Rscript -e 'devtools::check()'              # full R CMD check
Rscript -e 'Rcpp::compileAttributes()'      # regenerate R/RcppExports.R + src/RcppExports.cpp
```

The C++ sources (`src/markdownparser.cpp`, `src/util.cpp`, `src/httpuv_url_tools.cpp`) link against the bundled MD4C library (`src/md4c.c`, header in `inst/include/md4c.h`). `Rcpp::compileAttributes()` must be rerun after changing any `// [[Rcpp::export]]` signature.

## Architecture

### Parser pipeline

1. `parse_md()` / `parse_gfm()` (`R/parse_md.R`) accept a string, character vector, or path. They convert flag names to a bitmask via `flag_mask()` then call into C++.
2. `parse_md_cpp()` (`src/markdownparser.cpp`, generated binding in `R/RcppExports.R`) drives MD4C with callbacks (`onEnterBlock`, `onLeaveBlock`, `onEnterSpan`, `onLeaveSpan`, `onText`) that build a tree of `md_node` C++ structs. The `Rcpp::wrap` specialization for `md_node` collapses single-content `md_text` nodes into character vectors and otherwise produces nested `Rcpp::List`s carrying class + attributes.
3. The result is an R list with class `c("md_block_doc", "md_block", "md_node")` whose children are recursively classed nodes. The document carries a `flags` attribute used by `flags_used()` and downstream converters.

### AST node taxonomy

Every node inherits `md_node`. Three branches:

- `md_block_*` (`md_block`): doc, h, p, ul, ol, li, code, html, quote, hr, table, thead, tbody, tr, th, td, latexmath, latexmath_display.
- `md_span_*` (`md_span`): a, img, code, em, strong, del, u, latexmath, latexmath_display, wikilink.
- `md_text_*` (`md_text`): normal, code, html, entity, br, softbreak, latexmath, nullchar. These are typically character vectors (length 1) with class attached, not lists.

Node-specific attributes (e.g. `level` on `md_block_h`, `href`/`title` on `md_span_a`, `is_task`/`task_mark` on `md_block_li`, `align` on table cells) are R attributes set by the C++ layer; constructors in `R/create_block.R`, `R/create_span.R`, `R/create_text.R` mirror them and validate via `checkmate`.

### Converters

- `R/print.R`: tree printer dispatched by `print_node()` S3 generic, producing the `├── … └──` rendering seen at the REPL.
- `R/to_md.R`: round-trip back to markdown text. Heavy logic lives in `process_child_nodes()` which handles escaping (`needs_escape`), text-node merging (`is_mergeable`), code-span backtick balancing, and the special concatenation rules for code/html blocks and lists.
- `R/to_html.R`: HTML serialization, with `html_escape()` and `tag_filter()` helpers.
- `R/ast_util.R`: `map_ast()` walks the tree and applies a function to nodes matching a class set; `R/md_util.R`: `md_text()` / `md_text_summary()` extract text content.
- `R/flags.R`: registry of MD4C flags. `flags_describe()` is the single source of truth for valid flag names; `flags_check()` validates user input against it.

### Test generation

Tests for `to_md` and `to_html` are auto-generated from external spec files, not hand-written:

- `inst/specs/md4c/*.txt` (MD4C author's specs) and `inst/specs/gfm/spec.txt` (GFM spec).
- `R/tests.R` parses the spec format (markdown blocks delimited by ```` ```` ```` example) and emits testthat code.
- `tests/testthat/helper-tests.R` regenerates `test-to_html-gfm.R`, `test-to_md-gfm.R`, `test-to_html-md4c.R`, `test-to_md-md4c.R` whenever `R/tests.R` or the spec files are newer than the generated files. Regeneration is skipped on CI (`Sys.getenv("CI") != ""`) and runs on `devtools::test()` locally. `R/tests.R` is excluded from the build via `.Rbuildignore`.
- Hand-written tests live in `test-flags.R` and `test-table.R`.

When changing `to_md.R` or `to_html.R` semantics, expect generated test files to update on next local test run; review the diff before committing.

## Conventions

- R code in this package uses `=` for assignment (not `<-`) and references external functions as `pkg::fn()`. The packages already in `Imports` are `checkmate`, `cli`, `glue`, `purrr`, `Rcpp`, `stringr`, `textutils`, `tibble`. Do not add bare imports to `NAMESPACE` unless required.
- S3 dispatch is the primary extension mechanism. New node classes need matching `print_node`, `to_md`, and `to_html` methods (registered with `@exportS3Method`) plus a constructor in `R/create_*.R`.
- Pre-compiled artifacts (`*.o`, `*.so`) in `src/` are build outputs; do not commit changes to them.
