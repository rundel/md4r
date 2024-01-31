url = "https://raw.githubusercontent.com/github/cmark-gfm/master/test/spec.txt"

download.file(
  url,
  destfile = here::here("inst/specs/gfm/spec.txt"),
  method = "auto"
)
