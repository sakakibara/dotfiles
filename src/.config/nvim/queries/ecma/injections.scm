; extends

; Bundled ecma/injections.scm covers: styled.div`...`, styled(Component)`...`,
; styled.div.attrs(...)`...`, styled(Component).attrs(...)`...`, css`...`,
; keyframes`...`, injection.language = "styled". Only add patterns not in the
; bundled set; mirror its format (offset strips the backticks, include-children
; keeps ${interpolation} inside the injection).

; createGlobalStyle`...` / injectGlobal`...`
(call_expression
  function: (identifier) @_fn
  (#any-of? @_fn "createGlobalStyle" "injectGlobal")
  arguments: ((template_string) @injection.content
    (#offset! @injection.content 0 1 0 -1)
    (#set! injection.include-children)
    (#set! injection.language "styled")))
