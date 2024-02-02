process_child_nodes = function(md, ..., collapse = NULL) {
  if (is.null(collapse))
    return( unlist(lapply(md, to_md, ...)) )

  mergeable_text = c("md_text_normal", "md_text_html",
                     "md_text_entity", "md_span")

  if (length(md) == 0)
    return(character())

  content = to_md(md[[1]], ...)
  if (length(content) == 1 && content == "\n")
    content = ""

  for(i in seq_along(md)[-1]) {
    new_content = to_md(md[[i]], ...)

    if (inherits(md[[i]], "md_span_code") & n_match(content, "`") %% 2 == 1) {
      # Handle CommonMark Spec 0.29 - Ex 349 edge case - if there is an unmatched ` in preceding
      # content then we need to double backquote the code span
      new_content = paste0("`", new_content, "`")
    }

    if (
         inherits(md[[i]],   c("md_text_code", "md_text_html"))
      && inherits(md[[i-1]], c("md_text_code", "md_text_html"))
      && inherits(md, c("md_block_code", "md_block_html"))
    ) {
      # These occur in their respective block types
      # and contain explicit \n and we need to split on these
      prev_content = to_md(md[[i-1]], ...)
      if (new_content == "\n" && prev_content == "\n") {
        content = c(content, "")
      } else if (new_content == "\n") {
        # Do nothing
      } else if (prev_content == "\n") {
        content = c(content, new_content)
      } else {
        end = length(content)
        content[end] = paste0(content[end], new_content)
      }
    } else if (
        inherits(md[[i]], mergeable_text) &&
        inherits(md[[i-1]], mergeable_text)
    ) {
      end = length(content)
      content = c(
        content[-end],
        paste0(content[end], new_content[1]),
        new_content[-1]
      )
    } else if ( # Handles case with a md_text_break by adding trailing ws
        inherits(md[[i]], "md_text_break") &&
        inherits(md[[i-1]], mergeable_text)
    ) {
      end = length(content)
      content = c(
        content[-end],
        paste0(content[end], "    ")
      )
    } else if (
         inherits(md[[i-1]], c("md_block_p", "md_block_table", "md_block_ul", "md_block_ol",
                               "md_block_quote", "md_block_html"))
      && inherits(md[[i]],   c("md_block")) )
    {
        # Separate p blocks with an empty line (not always needed but necessary in some cases)
        content = c(content, "", new_content)
    } else {

        if (
            inherits(md[[i-1]], "md_text_softbreak")
         && (new_content %in% c("***", "---", "___")   # Needed for CommonMark Ex #19
             || grepl("^[#]", new_content) )           # Needed for CommonMark Ex #40
        ) {
          new_content = paste0("    ", new_content)
        }

        content = c(content, new_content)
    }
  }

  content
}


rep_char = function(c, n) {
  paste(rep(c, n), collapse="")
}


#' @title Convert to markdown
#'
#' @description Coverts a markdown object (full ast or node) to markdown text.
#'
#' @param md Markdown object
#' @param ... Unused, for extensibility.
#'
#' @returns Returns a character vector of markdown lines representing the markdown object.
#'
#' @examples
#' md_file = system.file("examples/commonmark.md", package = "md4r")
#' md = parse_md(md_file)
#' cat(to_md(md), sep="\n")
#'
#' @export
#'
to_md = function(md, ...) {
 UseMethod("to_md")
}

#' @exportS3Method
to_md.default = function(md, ...) {
  cli::cli_abort("Unsupported S3 class: {.val {class(md)}}.")
}


#' @exportS3Method
to_md.md_node = function(md, ...) {
  process_child_nodes(md, ...)
}

#############
#
# md_block_*
#
#############

#' @exportS3Method
to_md.md_block = function(md, ...) {
  cli::cli_abort("Unknown md block class: {.val {class(md)}}.")
}

#' @exportS3Method
to_md.md_block_doc = function(md, ...) {
  process_child_nodes(md, flags = attr(md, "flags"), ..., collapse="")
}


#' @exportS3Method
to_md.md_block_quote = function(md, ...) {
  content = process_child_nodes(md, ..., collapse=TRUE)

  # If it might look like a list or setext heading underline
  # we need to indent 4-spaces so it stays as text
  content = sub("^([*-]+ |===+|---+)", "    \\1", content)

  paste0("> ", content)
}

#' @exportS3Method
to_md.md_block_h = function(md, ...) {
  level = attr(md, "level")

  content = process_child_nodes(md, ..., collapse = TRUE)

  if (length(content) > 1) { # Setext heading
    if (level == 1) {
      content = c(content, "===")
    } else if (level == 2) {
      content = c(content, "---")
    } else {
      cli::cli_abort("Multiline headings not supported with level > 2.")
    }
  } else { # ATX heading
    paste0(rep_char("#", level), " ", content)
  }
}

calc_fence_len = function(content, fence_char, min_len = 3) {
  # If the fence char occurs, our fence needs to be longer
  len = purrr::map(
    gregexpr(paste0(fence_char, "+"), content),
    ~ attr(.x, "match.length")
  )

  max(min_len, unlist(len)+1)
}




#' @exportS3Method
to_md.md_block_code = function(md, ...) {
  info = attr(md, "info")
  lang = attr(md, "lang")
  fence_char = attr(md, "fence_char")

  content = process_child_nodes(md, ..., collapse = TRUE)


  if (fence_char == "") { # Indented code blocks
    paste0("    ", content)
  } else { # Fenced code block
    len = calc_fence_len(content, fence_char, min_len = 3)

    fence = rep_char(fence_char, len)
    c(
      paste0(fence, info),
      content,
      fence
    )
  }
}

#' @exportS3Method
to_md.md_block_html = function(md, ...) {
  process_child_nodes(md, ..., collapse = TRUE)
}

#' @exportS3Method
to_md.md_block_p = function(md, ...) {
  stopifnot(length(md) != 0)
  process_child_nodes(md, ..., collapse = TRUE)
}

#' @exportS3Method
to_md.md_block_hr = function(md, ...) {
  "***"
}

#############
#
# md list stuff
#
#############

#' @exportS3Method
to_md.md_block_ul = function(md, ...) {
  mark = attr(md, "mark")
  tight = attr(md, "tight")

  if (!mark %in% c("-","+","*")) {
    cli::cli_warn("Unexpected mark value {.val {mark}} replacing with {.val '-'}.")
    mark = "-"
  }

  prefix = paste0(" ", mark, " ")

  res = process_child_nodes(md, prefix = prefix, tight = tight, ...)
  if (res[length(res)] == "")
    res[-length(res)]
  else
    res
}


#' @exportS3Method
to_md.md_block_ol = function(md, ...) {
  start = attr(md, "start")
  delim = attr(md, "mark_delimiter")
  tight = attr(md, "tight")

  if (!delim %in% c(".",")")) {
    cli::cli_warn("Unexpected mark delimeter value {.val {delim}} replacing with {.val '.'}.")
    delim = "."
  }

  # Assumes all children are <li>
  n = start+length(md)-1

  mark_vals = start:n
  marks = paste0(" ", mark_vals, delim, " ")

  unlist( purrr::map2(
    md, marks,
    ~ to_md(.x, prefix = .y, tight = tight, ...)
  ) )
}

#' @exportS3Method
to_md.md_block_li = function(md, ...) {
  is_task   = attr(md, "is_task")
  task_mark = attr(md, "task_mark")

  args = list(...)
  prefix = args$prefix %||% cli::cli_abort("Required arg {.arg prefix} not provided.")
  tight  = args$tight  %||% cli::cli_abort("Required arg {.arg tight} not provided.")

  indent = rep_char(" ", nchar(prefix))

  if (is_task)
    prefix = glue::glue("{prefix}[{task_mark}] ")

  if (length(md) == 0) {
    if (!tight)
      return(c(prefix, ""))
    else
      return(prefix)
  }

  # TODO - bring this inline with th process_child_nodes w/ merging text etc

  content = purrr::imap(
    md,
    function(child, i, ...) {
      if (inherits(child, "md_text_softbreak"))
        return(character())

      content = to_md(child, ...)

      content = if (i == 1) {
        if (inherits(child, c("md_block_ul", "md_block_ol"))) {
          c(
            prefix,
            paste0(indent, content)
          )
        } else if (length(content) == 1) {
          paste0(prefix, content)
        } else {
          c(
            paste0(prefix, content[1]),
            paste0(indent, content[-1])
          )
        }
      } else {
        if (inherits(child, "md_block_p") | inherits(md[[i-1]], "md_block_p")) {
          # If either block_p add a spacing line
          c("", paste0(indent, content))
        } else if (inherits(child, "md_block") || inherits(md[[i-1]], "md_block")) {
          paste0(indent, content)
        } else if (grepl("^[-*] ", content)) {
          # Looks like a list but isnt, use indents to keep as text
          paste0(indent, "    ", content)
        } else {
          content
        }
      }

      content
    },
    ...
  )

  res = unlist(content)

  if (!tight)
    c(res, "")
  else
    res
}


#############
#
# md table stuff
#
#############


#' @exportS3Method
to_md.md_block_table = function(md, ...) {
  checkmate::assert_class(md[[1]], "md_block_thead")
  head = to_md(md[[1]], ...)


  if (length(md) == 1) {
    body = rep(list(character()), length(head))
    body = do.call(data.frame, body)
  } else if (length(md) == 2) {
    checkmate::assert_class(md[[2]], "md_block_tbody")
    body = to_md(md[[2]], ...)

    body = do.call(rbind, body)
    body = as.data.frame(body)
  } else {
    cli::cli_abort("Unexpected number of children nodes ({length(md)}) for a `md_block_table`.")
  }

  names(body) = head
  attr(body, "align") = attr(head, "align")

  df_md_table(body)
}

#' @exportS3Method
to_md.md_block_thead = function(md, ...) {
  checkmate::assert_list(md, len = 1)
  checkmate::assert_class(md[[1]], "md_block_tr")

  to_md(md[[1]], ...)
}

#' @exportS3Method
to_md.md_block_tbody = function(md, ...) {
  lapply(md, to_md, ...)
}

#' @exportS3Method
to_md.md_block_tr = function(md, ...) {
  content = lapply(md, to_md, ...)
  align = lapply(content, attr, which="align")
  align = unlist(align)

  content = unlist(content)
  attr(content, "align") = align

  content
}

#' @exportS3Method
to_md.md_block_th = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  if (length(content) == 0)
    content = ""

  content = table_entry_escape(content) # escape | chars
  attr(content, "align") = attr(md, "align")

  content
}

#' @exportS3Method
to_md.md_block_td = function(md, ...) {
  content = process_child_nodes(md, ..., collapse="")
  if (length(content) == 0)
    content = ""

  table_entry_escape(content) # escape | chars
}



df_md_table = function(df) {

  align = attr(df, "align")
  if (is.null(align))
    align = rep("default", ncol(df))

  checkmate::assert_character(
    align, len = ncol(df), any.missing = FALSE,
    pattern = "default|center|left|right"
  )

  col_widths = purrr::map2_int(
    df, names(df), ~ max(nchar(.x)+2L, nchar(.y)+2L, 3L)
  )

  divider = purrr::map2_chr(
    align, col_widths,
    function(align, width) {
      if (align == "center")     paste(c(":", rep("-", width-2), ":"), collapse="")
      else if (align == "left")  paste(c(":", rep("-", width-1)), collapse="")
      else if (align == "right") paste(c(rep("-", width-1), ":"), collapse="")
      else                       paste(rep("-", width), collapse="")
    }
  )

  pad_cells = function(x, width, side = "right", pad = " ") {
    stringr::str_pad(
      paste0(" ", trimws(x)),
      width,
      side = side,
      pad = pad
    )
  }

  build_line = function(x) {
    paste0("|", paste(x, collapse = "|"), "|")
  }

  c(
    build_line( pad_cells(names(df), col_widths) ),
    build_line( divider ),
    purrr::map_chr(
      seq_len(nrow(df)),
      ~ build_line( pad_cells(unlist(df[.x,]), col_widths) )
    )
  )
}




#############
#
# md_span_*
#
#############

wrap_content = function(content, begin, end = begin) {
  n = length(content)
  stopifnot(n!=0)

  content[1] = paste0(begin, content[1])
  content[n] = paste0(content[n], end)

  content
}

# Find the number of occurances of pattern within text
n_match = function(text, pattern, ..., collapse="\n") {
  text = paste(text, collapse=collapse)

  m = gregexpr(pattern, text, ...)[[1]]
  m = m[m != -1]

  length(m)
}


#' @exportS3Method
to_md.md_span_em = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = TRUE)

  fence_char = "*"

  end = length(content)
  if (grepl("^\\*[^*]", content[1]) && grepl("[^*]\\*$", content[end]))
    fence_char = "_"

  wrap_content(content, fence_char)
}

#' @exportS3Method
to_md.md_span_strong = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = TRUE)

  if (n_match(content, "\\*") %% 2 == 1) # Unbalanced *s so use __ instead
    fence_char = "__"
  else
    fence_char = "**"

  wrap_content(content, fence_char)
}

#' @exportS3Method
to_md.md_span_u = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = TRUE)
  wrap_content(content, "_")
}

#' @exportS3Method
to_md.md_span_code = function(md, ...) {
  #content = process_child_nodes(md, ..., collapse = TRUE)

  content = to_md(md[[1]], ...)
  prev_content = content

  if (length(md) != 1) {
    for(i in seq_along(md)[-1]) {
      new_content = to_md(md[[i]], ...)
      stopifnot(length(new_content) == 1)

      if (   ( !grepl("( )+", prev_content) && new_content != " ")
          || (prev_content == " " && new_content == " ") # Back to back => trailing ws
      ) {
        end = length(content)
        content = c(
          content[-end],
          paste0(content[end], new_content)
        )
      } else if (new_content != " " || i == length(md) )  {# Trailing ws should be added
        content = c(content, new_content)
      } else{
        #stop("Should not happen!")
      }

      prev_content = new_content
    }
  }


  len = calc_fence_len(content, "`", min_len = 1)
  fence = paste(rep("`", len), collapse="")

  if (len != 1)
    content = wrap_content(content, " ")

  wrap_content(content, fence)
}

#' @exportS3Method
to_md.md_span_del = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = TRUE)
  wrap_content(content, "~")
}

#' @exportS3Method
to_md.md_span_latexmath = function(md, ...) {
  content = process_child_nodes(md, ..., collapse = TRUE)
  wrap_content(content, "$")
}

#' @exportS3Method
to_md.md_span_latexmath_display = function(md, ...) {
  content = process_child_nodes(md, ...)

  sep = ""
  if (length(content) == 1) {
    wrap_content(content, "$$")
  } else {
    c("$$", content, "$$")
  }
}

#' @exportS3Method
to_md.md_span_a = function(md, ...) {
  href = attr(md, "href")
  title = attr(md, "title")

  flags = list(...)[["flags"]] %||% character()

  if (title != "" & !is.null(title)) {
    title = gsub('"', '\\\\"', title)
    title = glue::glue(" \"{title}\"")
  }

  if (length(md) != 0)
    content = process_child_nodes(md, ..., collapse = "")
  else
    content = ""

  if (content == href & href != "") {
    glue::glue("<{href}>")
  } else if (sub("^mailto:|^http[s]?://","", href) == content & href != "") {
    if (flag_is_set(flags, "MD_FLAG_PERMISSIVEWWWAUTOLINKS") & grepl("^www\\.", content)) {
      content
    } else if (flag_is_set(flags, "MD_FLAG_PERMISSIVEEMAILAUTOLINKS") & grepl("^mailto:", href)) {
      content
    } else {
      glue::glue("<{content}>")
    }
  } else {
    glue::glue("[{content}](<{href}>{title})")
  }
}

#' @exportS3Method
to_md.md_span_img = function(md, ...) {
  src = attr(md, "src")
  title = attr(md, "title")

  if (title != "" & !is.null(title))
    title = glue::glue(" '{title}'")

  content = process_child_nodes(md, ...)
  text = paste(content, collapse="")

  glue::glue("![{text}]({src}{title})")
}

#' @exportS3Method
to_md.md_span_wikilink = function(md, ...) {
  target = attr(md, "target")
  content = process_child_nodes(md, ..., collapse="")
  content

  target = gsub("\\|", "\\\\|", target)

  if (target != paste(content, collapse="")) {
    content[1] = paste0(target, "|", content[1])
  }

  wrap_content(content, "[[", "]]")
}






#############
#
# md_text_*
#
#############

#' @exportS3Method
to_md.md_text_break = function(md, ...) {
  ""
}

#' @exportS3Method
to_md.md_text_softbreak = function(md, ...) {
  character()
}

#' @exportS3Method
to_md.md_text_latexmath = function(md, ...) {
  if (md == " ")
    character()
  else
    md
}

#' @exportS3Method
to_md.md_text_normal = function(md, ...) {
  content = unlist(md)

  # Punctuation chars need to be escaped
  if (grepl("^([\\`*$%_{}<>()#+.!\"\'&/:;<>=?@^~|-]|\\[|\\])$", content))
    paste0("\\", content)
  else
    content
}


#' @exportS3Method
to_md.md_text = function(md, ...) {
  unlist(md)
}


