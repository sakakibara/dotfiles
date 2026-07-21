---
name: code-section-markers
description: Use when adding a section separator or internal heading inside a code file, or when tempted to reach for banner comments, ASCII rules, MARK/SECTION tags, or region markers. Enforces plain named-comment headings instead.
---

# Section markers in code

Do not draw section separators. Banned: banner rules (`// --- x ---`, `// ===`), column-padding (`// x -----------`), single-glyph markers (`// # x`, `// = x`, `// * x`), keyword tags (`// MARK:`, `// SECTION:`), and `#region`/`#endregion`.

When a file genuinely needs an internal heading, use a plain named comment: a blank line, then the language's comment leader + a terse Sentence-case noun phrase, no trailing period, indented to the enclosing scope. The blank line is the separator; the words are the label.

```
// Sorted typed object emission   (Zig/C/JS/Rust)
-- Sorted typed object emission   (Lua/SQL)
#  Sorted typed object emission   (shell/Python/TOML/YAML)
```

Why a named label, not a marker glyph or tag: (1) editors already fold structure from the syntax tree, so region/marker glyphs add nothing; (2) every glyph is a comment leader or operator in some language, so no symbol is collision-proof, but an alphabetic label after the leader is pure content everywhere; (3) style authorities converge against heavy section markers (StyleCop SA1124, the Xcode-MARK critique, the kernel) - they signal a file that should be split. If a file needs many such headings, that is the signal to split it.
