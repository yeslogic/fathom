/// Test that one can refer to local type aliases in aliases.
extern U32Be : Format;

Foo = item U32Be;

Bar = item Foo;
