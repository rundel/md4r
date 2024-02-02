

# md4r

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/rundel/md4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rundel/md4r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Provides an R wrapper for the MD4C (Markdown for C) library.Functions
exist for markdown parsing (CommonMark compliant) along with support for
other common markdown extensions (e.g. GitHub flavored markdown, LaTeX
equation support, etc.). The package also provides a number of high
level functions for exploring and manipulating markdown ASTs as well as
translating and displaying the documents.

## Installation

Install md4r from CRAN:

``` r
install.packages("md4r")
```

or install the latest development version package from GitHub:

``` r
remotes::install_github("rundel/md4r")
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
#> [commonmark.js](https://github.com/commonmark/commonmark.js), the
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
#> md_block_doc [flags: "MD_DIALECT_COMMONMARK"]
#> ├── md_block_h [level: 2]
#> │   └── md_text_normal - "Try CommonMark"
#> ├── md_block_p
#> │   ├── md_text_normal - "You can try CommonMark here.  This dingus is powered by"
#> │   ├── md_text_softbreak
#> │   ├── md_span_a [title: "", href: "https://github.com/commonmark/commonmark.js"]
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
#>   ..$ : 'md_text_normal' chr "Try CommonMark"
#>   ..- attr(*, "level")= num 2
#>   ..- attr(*, "class")= chr [1:3] "md_block_h" "md_block" "md_node"
#>  $ :List of 6
#>   ..$ : 'md_text_normal' chr "You can try CommonMark here.  This dingus is powered by"
#>   ..$ : list()
#>   .. ..- attr(*, "class")= chr [1:3] "md_text_softbreak" "md_text" "md_node"
#>   ..$ :List of 1
#>   .. ..$ : 'md_text_normal' chr "commonmark.js"
#>   .. ..- attr(*, "title")= chr ""
#>   .. ..- attr(*, "href")= chr "https://github.com/commonmark/commonmark.js"
#>   .. ..- attr(*, "class")= chr [1:3] "md_span_a" "md_span" "md_node"
#>   ..$ : 'md_text_normal' chr ", the"
#>   ..$ : list()
#>   .. ..- attr(*, "class")= chr [1:3] "md_text_softbreak" "md_text" "md_node"
#>   ..$ : 'md_text_normal' chr "JavaScript reference implementation."
#>   ..- attr(*, "class")= chr [1:3] "md_block_p" "md_block" "md_node"
#>  $ :List of 2
...
```

As the AST is just a collection of R lists - we can use subsetting to
extract specific elements of the document

``` r
parse_md(md_file)[[1]]
#> md_block_h [level: 2]
#> └── md_text_normal - "Try CommonMark"
```

``` r
parse_md(md_file)[[2]]
#> md_block_p
#> ├── md_text_normal - "You can try CommonMark here.  This dingus is powered by"
#> ├── md_text_softbreak
#> ├── md_span_a [title: "", href: "https://github.com/commonmark/commonmark.js"]
#> │   └── md_text_normal - "commonmark.js"
#> ├── md_text_normal - ", the"
#> ├── md_text_softbreak
#> └── md_text_normal - "JavaScript reference implementation."
```

``` r
parse_md(md_file)[[3]]
#> md_block_ol [start: 1, tight: 1, mark_delimiter: "."]
#> ├── md_block_li
#> │   └── md_text_normal - "item one"
#> └── md_block_li
#>     ├── md_text_normal - "item two"
#>     └── md_block_ul [tight: 1, mark: "-"]
#>         ├── md_block_li
#>         │   └── md_text_normal - "sublist"
#>         └── md_block_li
#>             └── md_text_normal - "sublist"
```

or more advanced tools like `rapply()` to extract text content

``` r
rapply(md, as.character, "md_text")
#> [1] "Try CommonMark"                                         
#> [2] "You can try CommonMark here.  This dingus is powered by"
#> [3] "commonmark.js"                                          
#> [4] ", the"                                                  
#> [5] "JavaScript reference implementation."                   
#> [6] "item one"                                               
#> [7] "item two"                                               
#> [8] "sublist"                                                
#> [9] "sublist"
```

Additionally, the AST and any component can be converted back into
markdown

``` r
to_md(md) |> cat(sep='\n')
#> ## Try CommonMark
#> You can try CommonMark here.  This dingus is powered by
#> [commonmark.js](<https://github.com/commonmark/commonmark.js>), the
#> JavaScript reference implementation.
#> 
#>  1. item one
#>  2. item two
#>      - sublist
#>      - sublist
```

or into html

``` r
to_html(md) |> cat(sep='\n')
```

<blockquote>
<h2>
Try CommonMark
</h2>
<p>
You can try CommonMark here. This dingus is powered by
<a href="https://github.com/commonmark/commonmark.js">commonmark.js</a>
, the JavaScript reference implementation.
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
</blockquote>
