#include <Rcpp.h>

// [[Rcpp::export]]
std::string table_entry_escape(std::string const& x, bool keep_double_pipe = false) {
  bool in_back_quote = false;
  int sq_bracket_level = 0;
  char prev = ' ';

  std::string out;
  for(size_t i = 0; i < x.size(); i++) {
    char c = x[i];
    if      (c == '[') sq_bracket_level++;
    else if (c == ']') sq_bracket_level = std::max(0, sq_bracket_level-1);
    else if (c == '`') in_back_quote = !in_back_quote;

    // With MD_FLAG_SPOILERS a || pair delimits a spoiler span within the cell
    bool double_pipe = keep_double_pipe && c == '|' &&
      ((i+1 < x.size() && x[i+1] == '|') || prev == '|');

    if (c == '|' && !(in_back_quote || (sq_bracket_level > 0) || (prev == '\\') || double_pipe))
      out.push_back('\\');

    out.push_back(c);

    prev = c;
  }

  return out;
}



/*** R

*/
