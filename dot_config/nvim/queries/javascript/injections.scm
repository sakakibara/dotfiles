; extends

; styled.div`...` / styled.button`...` / etc.
((call_expression
   function: (member_expression
     object: (identifier) @_obj (#eq? @_obj "styled"))
   arguments: (template_string) @injection.content)
 (#set! injection.language "css"))

; styled(Component)`...`
((call_expression
   function: (call_expression
     function: (identifier) @_fn (#eq? @_fn "styled"))
   arguments: (template_string) @injection.content)
 (#set! injection.language "css"))

; css`...` / keyframes`...` / injectGlobal`...` / createGlobalStyle`...`
((call_expression
   function: (identifier) @_fn
   (#any-of? @_fn "css" "keyframes" "injectGlobal" "createGlobalStyle")
   arguments: (template_string) @injection.content)
 (#set! injection.language "css"))
