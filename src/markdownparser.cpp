#include <Rcpp.h>
#include <Rinternals.h>

using namespace Rcpp;

#include "md4c.h"

struct md_node;

struct md_node {
  std::vector<std::string> type;
  std::string text;
  Rcpp::List attributes;

  std::vector<md_node> children;
  md_node* parent;

  md_node(std::vector<std::string> type)
  : type(type)
  { };

  md_node(std::vector<std::string> type, Rcpp::List attr)
    : type(type), attributes(attr)
  { };
};

namespace Rcpp {
// chunk wrappers
template <> SEXP wrap(md_node const& node) {

  if (node.type[1] == "text") {
    return Rcpp::CharacterVector(node.text);
  }

  Rcpp::List res;
  for(auto const& child : node.children) {
    res.push_back(wrap(child));
  }

  res.attr("class") = node.type;

  return res;
};
}


class MarkdownParser {
public:
  MarkdownParser(std::string const& doc);
  int parse(std::string const& doc);
  md_node get_ast();

  void add_node(md_node &node);
  void end_node();

private:
  static int onEnterBlock(MD_BLOCKTYPE type, void *detail, void *context);
  static int onLeaveBlock(MD_BLOCKTYPE type, void *detail, void *context);
  static int onEnterSpan(MD_SPANTYPE type, void *detail, void *context);
  static int onLeaveSpan(MD_SPANTYPE type, void *detail, void *context);
  static int onText(MD_TEXTTYPE type, const MD_CHAR *text, MD_SIZE size, void *context);

  MD_PARSER parser;
  md_node ast;
  md_node* cur_node;
};

MarkdownParser::MarkdownParser(std::string const& doc)
: ast(md_node({"md", "root"}))
{
  parser = {
    0, // abi_version
    unsigned(MD_FLAG_NOHTML | MD_FLAG_STRIKETHROUGH),
    &MarkdownParser::onEnterBlock,
    &MarkdownParser::onLeaveBlock,
    &MarkdownParser::onEnterSpan,
    &MarkdownParser::onLeaveSpan,
    &MarkdownParser::onText,
    nullptr,
    nullptr
  };

  //stack.push_back(&ast);
  cur_node = &ast;

  parse(doc);
}

int MarkdownParser::parse(std::string const& doc) {
  return md_parse(doc.c_str(), doc.size(), &parser, this);
}

md_node MarkdownParser::get_ast() {
  return ast;
}

std::string md_attr_str(MD_ATTRIBUTE const& md_attr) {
  return std::string(md_attr.text, md_attr.size);
}



void MarkdownParser::add_node(md_node& node) {
  node.parent = cur_node;
  cur_node->children.push_back(node);
  md_node* ptr = &(*std::prev(cur_node->children.end()));
  cur_node = ptr;
}

void MarkdownParser::end_node() {
  if (cur_node->parent == nullptr)
    Rcpp::stop("Error");

  cur_node = cur_node->parent;
}




int MarkdownParser::onEnterBlock(MD_BLOCKTYPE type, void* detail, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);

  Rcpp::Rcout << "Start block " << type << "\n";

  Rcpp::List new_block;
  std::vector<std::string> block_class = {"md", "block"};

  if (type == MD_BLOCK_DOC) {
    block_class.push_back("doc");
  } else if (type == MD_BLOCK_QUOTE) {
    block_class.push_back("quote");
  } else if (type == MD_BLOCK_UL) {
    block_class.push_back("ul");
  } else if (type == MD_BLOCK_OL) {
    block_class.push_back("ol");
  } else if (type == MD_BLOCK_LI) {
    block_class.push_back("li");
  } else if (type == MD_BLOCK_HR) {
    block_class.push_back("hr");
  } else if (type == MD_BLOCK_H) {
    block_class.push_back("h");
  } else if (type == MD_BLOCK_CODE) {
    block_class.push_back("code");
  } else if (type == MD_BLOCK_HTML) {
    block_class.push_back("html");
  } else if (type == MD_BLOCK_P) {
    block_class.push_back("p");
  } else if (type == MD_BLOCK_TABLE) {
    block_class.push_back("table");
  } else if (type == MD_BLOCK_THEAD) {
    block_class.push_back("thead");
  } else if (type == MD_BLOCK_TBODY) {
    block_class.push_back("tbody");
  } else if (type == MD_BLOCK_TR) {
    block_class.push_back("tr");
  } else if (type == MD_BLOCK_TH) {
    block_class.push_back("th");
  } else if (type == MD_BLOCK_TD) {
    block_class.push_back("td");
  } else {
    block_class.push_back("other");
  }

  md_node new_node(block_class);
  parser->add_node(new_node);

  return 0;
}

int MarkdownParser::onLeaveBlock(MD_BLOCKTYPE type, void* detail, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);

  Rcpp::Rcout << "End block\n";
  parser->end_node();

  return 0;
}

int MarkdownParser::onEnterSpan(MD_SPANTYPE type, void* detail, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);
  Rcpp::Rcout << "Start span\n";

  Rcpp::List span_attr;
  std::vector<std::string> span_class = {"md", "span"};

  if (type == MD_SPAN_EM) {
    span_class.push_back("em");
  } else if (type == MD_SPAN_STRONG) {
    span_class.push_back("strong");
  } else if (type == MD_SPAN_A) {
    span_class.push_back("a");

    //Detail: Structure MD_SPAN_A_DETAIL.
    MD_SPAN_A_DETAIL* d = static_cast<MD_SPAN_A_DETAIL *>(detail);
    span_attr["title"] = md_attr_str(d->title);
    span_attr["href"] = md_attr_str(d->href);
  } else if (type == MD_SPAN_IMG) {
    span_class.push_back("img");

    //Detail: Structure MD_SPAN_IMG_DETAIL.
    MD_SPAN_IMG_DETAIL* d = static_cast<MD_SPAN_IMG_DETAIL *>(detail);
    span_attr["title"] = md_attr_str(d->title);
    span_attr["src"] = md_attr_str(d->src);
  } else if (type == MD_SPAN_CODE) {
    span_class.push_back("code");
  } else if (type == MD_SPAN_DEL) {
    span_class.push_back("del");
  } else if (type == MD_SPAN_LATEXMATH) {
    span_class.push_back("latexmath");
  } else if (type == MD_SPAN_LATEXMATH_DISPLAY) {
    span_class.push_back("latexmath_display");
  } else if (type == MD_SPAN_WIKILINK) {
    span_class.push_back("wikilink");
  } else if (type == MD_SPAN_U) {
    span_class.push_back("u");
  }

  md_node new_span(span_class, span_attr);
  parser->add_node(new_span);

  return 0;
}

int MarkdownParser::onLeaveSpan(MD_SPANTYPE type, void* detail, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);
  Rcpp::Rcout << "End span\n";

  return 0;
}

int MarkdownParser::onText(MD_TEXTTYPE type, const MD_CHAR* text, MD_SIZE size, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);

  Rcpp::Rcout << "Text \n";

  std::vector<std::string> block_class = {"md", "text"};
  std::string txt;

  if (type == MD_TEXT_NULLCHAR) {
    block_class.push_back("nullchar");
    txt = "";
  } else if (type == MD_TEXT_BR) {
    block_class.push_back("break");
    txt = "\n";
  } else if (type == MD_TEXT_SOFTBR) {
    block_class.push_back("softbreak");
    txt = " ";
  } else {
    block_class.push_back("text");
    txt = std::string(text, size);
  }

  md_node new_text(block_class);
  new_text.text = txt;

  parser->add_node(new_text);
  parser->end_node();

  return 0;
}






//' @export
// [[Rcpp::export]]
Rcpp::List mdparse(std::string x) {
  MarkdownParser p(x);
  return Rcpp::wrap(p.get_ast());
}

