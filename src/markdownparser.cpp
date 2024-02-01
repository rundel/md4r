#include <Rcpp.h>
#include <Rinternals.h>

using namespace Rcpp;

#include "md4c.h"

struct md_node;

struct md_node {
  Rcpp::List l;
  std::vector<md_node> children;
  md_node* parent;
};

namespace Rcpp {
// chunk wrappers
template <> SEXP wrap(md_node const& node) {

  // Text nodes should be Character vectors (if they have content)
  if (node.l.inherits("md_text") && node.l.size() == 1) {
    Rcpp::CharacterVector v(node.l[0]);
    v.attr("class") = node.l.attr("class");
    return v;
  }


  Rcpp::List res(node.l);
  for(auto const& child : node.children) {
    res.push_back(Rcpp::wrap(child));
  }

  for(auto name : node.l.attributeNames()) {
    res.attr(name) = node.l.attr(name);
  }

  return res;
};
}


class MarkdownParser {
public:
  MarkdownParser(std::string const& doc, int flag);
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

MarkdownParser::MarkdownParser(std::string const& doc, int flag) {
  parser = {
    0, // abi_version
    (unsigned) flag,
    &MarkdownParser::onEnterBlock,
    &MarkdownParser::onLeaveBlock,
    &MarkdownParser::onEnterSpan,
    &MarkdownParser::onLeaveSpan,
    &MarkdownParser::onText,
    nullptr,
    nullptr
  };

  md_node root;
  root.l.attr("class") = std::vector<std::string>({"md_node"});

  ast = root;

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

  md_node node;
  std::vector<std::string> block_class;

  if (type == MD_BLOCK_DOC) {
    block_class.push_back("md_block_doc");
  } else if (type == MD_BLOCK_QUOTE) {
    block_class.push_back("md_block_quote");
  } else if (type == MD_BLOCK_UL) {
    block_class.push_back("md_block_ul");

    MD_BLOCK_UL_DETAIL* d = static_cast<MD_BLOCK_UL_DETAIL *>(detail);
    node.l.attr("tight") = d->is_tight;
    node.l.attr("mark") = std::string(1, d->mark);

  } else if (type == MD_BLOCK_OL) {
    block_class.push_back("md_block_ol");

    MD_BLOCK_OL_DETAIL* d = static_cast<MD_BLOCK_OL_DETAIL *>(detail);
    node.l.attr("start") = d->start;
    node.l.attr("tight") = d->is_tight;
    node.l.attr("mark_delimiter") = std::string(1, d->mark_delimiter);

  } else if (type == MD_BLOCK_LI) {
    block_class.push_back("md_block_li");

    MD_BLOCK_LI_DETAIL* d = static_cast<MD_BLOCK_LI_DETAIL *>(detail);
    node.l.attr("is_task") = d->is_task;
    node.l.attr("task_mark") = (d->is_task) ? std::string(1, d->task_mark) : std::string("");

  } else if (type == MD_BLOCK_HR) {
    block_class.push_back("md_block_hr");
  } else if (type == MD_BLOCK_H) {
    block_class.push_back("md_block_h");

    MD_BLOCK_H_DETAIL* d = static_cast<MD_BLOCK_H_DETAIL *>(detail);
    node.l.attr("level") = d->level;

  } else if (type == MD_BLOCK_CODE) {
    block_class.push_back("md_block_code");

    MD_BLOCK_CODE_DETAIL* d = static_cast<MD_BLOCK_CODE_DETAIL *>(detail);
    node.l.attr("info") = md_attr_str(d->info);
    node.l.attr("lang") = md_attr_str(d->lang);
    node.l.attr("fence_char") = std::string(1, d->fence_char);

  } else if (type == MD_BLOCK_HTML) {
    block_class.push_back("md_block_html");
  } else if (type == MD_BLOCK_P) {
    block_class.push_back("md_block_p");
  } else if (type == MD_BLOCK_TABLE) {
    block_class.push_back("md_block_table");

    MD_BLOCK_TABLE_DETAIL* d = static_cast<MD_BLOCK_TABLE_DETAIL *>(detail);
    node.l.attr("col_count") = d->col_count;
    node.l.attr("head_row_count") = d->head_row_count;
    node.l.attr("body_row_count") = d->body_row_count;

  } else if (type == MD_BLOCK_THEAD) {
    block_class.push_back("md_block_thead");
  } else if (type == MD_BLOCK_TBODY) {
    block_class.push_back("md_block_tbody");
  } else if (type == MD_BLOCK_TR) {
    block_class.push_back("md_block_tr");
  } else if (type == MD_BLOCK_TH || type == MD_BLOCK_TD) {
    if (type == MD_BLOCK_TD) {
      block_class.push_back("md_block_td");
    } else if (type == MD_BLOCK_TH) {
      block_class.push_back("md_block_th");
    }

    MD_BLOCK_TD_DETAIL* d = static_cast<MD_BLOCK_TD_DETAIL *>(detail);
    if      (d->align == MD_ALIGN_DEFAULT) node.l.attr("align") = "default";
    else if (d->align == MD_ALIGN_LEFT)    node.l.attr("align") = "left";
    else if (d->align == MD_ALIGN_CENTER)  node.l.attr("align") = "center";
    else if (d->align == MD_ALIGN_RIGHT)   node.l.attr("align") = "right";

  } else {
    block_class.push_back("md_block_other");
  }

  block_class.push_back("md_block");
  block_class.push_back("md_node");

  node.l.attr("class") = block_class;
  parser->add_node(node);

  return 0;
}

int MarkdownParser::onLeaveBlock(MD_BLOCKTYPE type, void* detail, void* userdata) {
  static_cast<MarkdownParser *>(userdata)->end_node();

  return 0;
}

int MarkdownParser::onEnterSpan(MD_SPANTYPE type, void* detail, void* userdata) {
  MarkdownParser *parser = static_cast<MarkdownParser *>(userdata);

  md_node node;
  std::vector<std::string> span_class;

  if (type == MD_SPAN_EM) {
    span_class.push_back("md_span_em");
  } else if (type == MD_SPAN_STRONG) {
    span_class.push_back("md_span_strong");
  } else if (type == MD_SPAN_A) {
    span_class.push_back("md_span_a");

    MD_SPAN_A_DETAIL* d = static_cast<MD_SPAN_A_DETAIL *>(detail);
    node.l.attr("title") = md_attr_str(d->title);
    node.l.attr("href") = md_attr_str(d->href);

  } else if (type == MD_SPAN_IMG) {
    span_class.push_back("md_span_img");

    MD_SPAN_IMG_DETAIL* d = static_cast<MD_SPAN_IMG_DETAIL *>(detail);
    node.l.attr("title") = md_attr_str(d->title);
    node.l.attr("src") = md_attr_str(d->src);

  } else if (type == MD_SPAN_CODE) {
    span_class.push_back("md_span_code");
  } else if (type == MD_SPAN_DEL) {
    span_class.push_back("md_span_del");
  } else if (type == MD_SPAN_LATEXMATH) {
    span_class.push_back("md_span_latexmath");
  } else if (type == MD_SPAN_LATEXMATH_DISPLAY) {
    span_class.push_back("md_span_latexmath_display");
  } else if (type == MD_SPAN_WIKILINK) {
    span_class.push_back("md_span_wikilink");

    MD_SPAN_WIKILINK_DETAIL* d = static_cast<MD_SPAN_WIKILINK_DETAIL *>(detail);
    node.l.attr("target") = md_attr_str(d->target);

  } else if (type == MD_SPAN_U) {
    span_class.push_back("md_span_u");
  }

  span_class.push_back("md_span");
  span_class.push_back("md_node");

  node.l.attr("class") = span_class;
  parser->add_node(node);

  return 0;
}

int MarkdownParser::onLeaveSpan(MD_SPANTYPE type, void* detail, void* userdata) {
  static_cast<MarkdownParser *>(userdata)->end_node();
  return 0;
}

int MarkdownParser::onText(MD_TEXTTYPE type, const MD_CHAR* text, MD_SIZE size, void* userdata) {
  std::vector<std::string> text_class;
  std::string txt;

  if (type == MD_TEXT_NULLCHAR) {
    text_class.push_back("md_text_nullchar");
  } else if (type == MD_TEXT_BR) {
    text_class.push_back("md_text_break");
  } else if (type == MD_TEXT_SOFTBR) {
    text_class.push_back("md_text_softbreak");
  } else {
    if (type == MD_TEXT_NORMAL) {
      text_class.push_back("md_text_normal");
    } else if (type == MD_TEXT_ENTITY) {
      text_class.push_back("md_text_entity");
    } else if (type == MD_TEXT_CODE) {
      text_class.push_back("md_text_code");
    } else if (type == MD_TEXT_HTML) {
      text_class.push_back("md_text_html");
    } else if (type == MD_TEXT_LATEXMATH) {
      text_class.push_back("md_text_latexmath");
    } else {
      Rcpp::stop("Unknown text type");
    }

    txt = std::string(text, size);
  }
  text_class.push_back("md_text");
  text_class.push_back("md_node");

  md_node node;
  if (txt.length() != 0)
    node.l.push_back(txt);
  node.l.attr("class") = text_class;

  static_cast<MarkdownParser *>(userdata)->add_node(node);
  static_cast<MarkdownParser *>(userdata)->end_node();

  return 0;
}

// [[Rcpp::export]]
Rcpp::List parse_md_cpp(std::string x, int flag) {
  MarkdownParser p(x, flag);
  return Rcpp::wrap(p.get_ast());
}

// [[Rcpp::export]]
int flag_mask(std::vector<std::string> const& flags) {
  int flag_val = 0;
  for(auto const& flag : flags) {
    if      (flag == "MD_FLAG_COLLAPSEWHITESPACE")       flag_val |= MD_FLAG_COLLAPSEWHITESPACE;
    else if (flag == "MD_FLAG_PERMISSIVEATXHEADERS")     flag_val |= MD_FLAG_PERMISSIVEATXHEADERS;
    else if (flag == "MD_FLAG_PERMISSIVEURLAUTOLINKS")   flag_val |= MD_FLAG_PERMISSIVEURLAUTOLINKS;
    else if (flag == "MD_FLAG_PERMISSIVEEMAILAUTOLINKS") flag_val |= MD_FLAG_PERMISSIVEEMAILAUTOLINKS;
    else if (flag == "MD_FLAG_NOINDENTEDCODEBLOCKS")     flag_val |= MD_FLAG_NOINDENTEDCODEBLOCKS;
    else if (flag == "MD_FLAG_NOHTMLBLOCKS")             flag_val |= MD_FLAG_NOHTMLBLOCKS;
    else if (flag == "MD_FLAG_NOHTMLSPANS")              flag_val |= MD_FLAG_NOHTMLSPANS;
    else if (flag == "MD_FLAG_TABLES")                   flag_val |= MD_FLAG_TABLES;
    else if (flag == "MD_FLAG_STRIKETHROUGH")            flag_val |= MD_FLAG_STRIKETHROUGH;
    else if (flag == "MD_FLAG_PERMISSIVEWWWAUTOLINKS")   flag_val |= MD_FLAG_PERMISSIVEWWWAUTOLINKS;
    else if (flag == "MD_FLAG_TASKLISTS")                flag_val |= MD_FLAG_TASKLISTS;
    else if (flag == "MD_FLAG_LATEXMATHSPANS")           flag_val |= MD_FLAG_LATEXMATHSPANS;
    else if (flag == "MD_FLAG_WIKILINKS")                flag_val |= MD_FLAG_WIKILINKS;
    else if (flag == "MD_FLAG_UNDERLINE")                flag_val |= MD_FLAG_UNDERLINE;
    else if (flag == "MD_FLAG_HARD_SOFT_BREAKS")         flag_val |= MD_FLAG_HARD_SOFT_BREAKS;
    else if (flag == "MD_FLAG_PERMISSIVEAUTOLINKS")      flag_val |= MD_FLAG_PERMISSIVEAUTOLINKS;
    else if (flag == "MD_FLAG_NOHTML")                   flag_val |= MD_FLAG_NOHTML;
    else if (flag == "MD_DIALECT_COMMONMARK")            flag_val |= MD_DIALECT_COMMONMARK;
    else if (flag == "MD_DIALECT_GITHUB")                flag_val |= MD_DIALECT_GITHUB;
    else                                                 Rcpp::stop("Unknown flag name: %s", flag);
  }

  return flag_val;
}

// [[Rcpp::export]]
bool flag_is_set(std::vector<std::string> const& flags, std::vector<std::string> const& check) {
    return static_cast<bool>(flag_mask(flags) & flag_mask(check));
}
