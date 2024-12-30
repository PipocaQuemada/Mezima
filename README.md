# Mezima - מזמה

A Scheme dialect, in Hebrew.

The implementation is substantially based off of the ebook Write Yourself a Scheme in 48 hours.

Probably the most interesting feature of the language is that it doesn't support regular numbers.  Instead, math must be done using hebrew numerals or the gemmatria values of hebrew words.  That is to say, `(+ ׳א ׳א)` evaluates to `׳ב`.

To edit .mzm files, you can use `:setrightleft` in vim to make sure you're editing text from right to left.  I would recommend not using a bidirectional text editor, since that can hide mistakes.  One idiosyncracy in parsing is that using the Hebrew input source on OSX in rtl vim causes parens to be flipped - what displays correctly as an open paren is a close paren.  To fix this, we try to parse twice - once as-is, and once with every paren flipped.
