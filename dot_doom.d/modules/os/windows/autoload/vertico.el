;;; os/windows/autoload/vertico.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +windows--consult--regexp-compiler-fn (input type ignore-case)
  "Compile the INPUT string to a list of regular expressions.
The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
  (setq input (consult--split-escaped input))
  (cons (mapcar (lambda (x) (let ((y (consult--convert-regexp x type)))
                              (if (multibyte-string-p x)
                                  (encode-coding-string y 'cp932)
                                y))) input)
        (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
          (lambda (str)
            (let ((case-fold-search ignore-case))
              (consult--highlight-regexps regexps str))))))

