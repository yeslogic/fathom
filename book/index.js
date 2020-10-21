import hljs from "highlight.js/lib/core";

hljs.registerLanguage("fathom", (hljs) => {
  const KEYWORDS = {
    keyword: "else enum Format if Kind match struct Type",
    built_in: [
      "Int F32 F64 Bool true false Array",
      "U8 U16Le U16Be U32Le U32Be U64Le U64Be",
      "S8 S16Le S16Be S32Le S32Be S64Le S64Be",
      "F32Le F32Be F64Le F64Be",
      "FormatArray",
    ].join(" "),
  };

  const CHARACTER = {
    className: "string",
    begin: /'([^'\\]|\\.)*'/,
  };
  const STRING = {
    className: "string",
    begin: /"([^"\\]|\\.)*"/,
  };
  const NUMBER = {
    className: "number",
    begin: /\b[-+]?[0-9][a-zA-Z0-9_\.]*\b/,
    relevance: 0,
  };

  const COMMENT = {
    variants: [hljs.COMMENT("//", "$")],
  };

  return {
    name: "Fathom",
    keywords: KEYWORDS,
    contains: [
      STRING,
      CHARACTER,
      NUMBER,

      COMMENT,

      { begin: "->|<-" }, // No markup, relevance booster
    ],
  };
});

window.addEventListener("load", (event) => {
  document
    .querySelectorAll("code.language-fathom")
    .forEach((block) => hljs.highlightBlock(block));
});
