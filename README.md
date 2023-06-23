
# md4r

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/rundel/md4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rundel/md4r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

md4r is a wrapper around the [md4c](https://github.com/mity/md4c)
markdown parsing library. The goal is to provide a low level tool for
reading markdown documents into

## Installation

Currently this library is only available from
[GitHub](https://github.com/) with, and can be installed using
`devtools` with:

``` r
# install.packages("devtools")
devtools::install_github("rundel/md4r")
```

## Example

We will start with a simple example of parsing a markdown file using the
basic CommonMark dialect.

``` r
md_file = system.file("examples/commonmark.md", package = "md4r")
readLines(md_file) |> cat(sep='\n')
#> ## Try CommonMark
#> 
#> You can try CommonMark here.  This dingus is powered by
#> [commonmark.js](https://github.com/jgm/commonmark.js), the
#> JavaScript reference implementation.
#> 
#> 1. item one
#> 2. item two
#>    - sublist
#>    - sublist
```

this file (or markdown text) can be processed using the `parse_md`
function which creates an abstract syntax tree representation of the
document (as a list of lists of lists … with custom S3 classes)

``` r
library(md4r)
(md = parse_md(md_file))
#> md_block_doc [flags: "MD_DIALECT_GITHUB"]
#> ├── md_block_h [level: 2]
#> │   └── md_text_normal - "Try CommonMark"
#> ├── md_block_p
#> │   ├── md_text_normal - "You can try CommonMark here.  This dingus is powered by"
#> │   ├── md_text_softbreak
#> │   ├── md_span_a [title: "", href: "https://github.com/jgm/commonmark.js"]
#> │   │   └── md_text_normal - "commonmark.js"
#> │   ├── md_text_normal - ", the"
#> │   ├── md_text_softbreak
#> │   └── md_text_normal - "JavaScript reference implementation."
#> └── md_block_ol [start: 1, tight: 1, mark_delimiter: "."]
#>     ├── md_block_li
#>     │   └── md_text_normal - "item one"
#>     └── md_block_li
#>         ├── md_text_normal - "item two"
#>         └── md_block_ul [tight: 1, mark: "-"]
#>             ├── md_block_li
#>             │   └── md_text_normal - "sublist"
#>             └── md_block_li
#>                 └── md_text_normal - "sublist"
```

``` r
str(md)
#> List of 3
#>  $ :List of 1
#>   ..$ :List of 1
#>   .. ..$ : chr "Try CommonMark"
#>   .. ..- attr(*, "class")= chr [1:3] "md_text_normal" "md_text" "md_node"
#>   ..- attr(*, "level")= num 2
#>   ..- attr(*, "class")= chr [1:3] "md_block_h" "md_block" "md_node"
#>  $ :List of 6
#>   ..$ :List of 1
#>   .. ..$ : chr "You can try CommonMark here.  This dingus is powered by"
#>   .. ..- attr(*, "class")= chr [1:3] "md_text_normal" "md_text" "md_node"
#>   ..$ : list()
#>   .. ..- attr(*, "class")= chr [1:3] "md_text_softbreak" "md_text" "md_node"
#>   ..$ :List of 1
#>   .. ..$ :List of 1
#>   .. .. ..$ : chr "commonmark.js"
#>   .. .. ..- attr(*, "class")= chr [1:3] "md_text_normal" "md_text" "md_node"
#>   .. ..- attr(*, "title")= chr ""
#>   .. ..- attr(*, "href")= chr "https://github.com/jgm/commonmark.js"
#>   .. ..- attr(*, "class")= chr [1:3] "md_span_a" "md_span" "md_node"
...
```

The resulting AST can then be manipulated and transformed into HTML

``` r
to_html(md) |> cat(sep='\n')
```

<h2>
Try CommonMark
</h2>
<p>
You can try CommonMark here. This dingus is powered by
<a href="https://github.com/jgm/commonmark.js">commonmark.js</a> , the
JavaScript reference implementation.
</p>
<ol>
<li>
item one
</li>
<li>
item two
<ul>
<li>
sublist
</li>
<li>
sublist
</li>
</ul>
</li>
</ol>

<br/>

or back into markdown

``` r
to_md(md) |> cat(sep='\n')
#> ## Try CommonMark
#> You can try CommonMark here.  This dingus is powered by
#> [commonmark.js](<https://github.com/jgm/commonmark.js>), the
#> JavaScript reference implementation.
#> 
#>  1. item one
#>  2. item two
#>      - sublist
#>      - sublist
```
