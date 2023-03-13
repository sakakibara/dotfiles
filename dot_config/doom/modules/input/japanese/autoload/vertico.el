;;; input/japanese/autoload/vertico.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +japanese--consult--migemo-regexp-compiler-fn (input type ignore-case)
  "Compile the INPUT string to a list of regular expressions.
It is an extended version of `consult--default-regexp-compiler' with migemo
to allow searching Japanese strings with romaji.
The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
  (setq input (mapcar #'migemo-get-pattern (consult--split-escaped input)))
  (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
        (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
          (apply-partially #'consult--highlight-regexps regexps ignore-case))))


;;;###autoload
(defun +japanese--consult--windows-migemo-regexp-compiler-fn (input type ignore-case)
  "Compile the INPUT string to a list of regular expressions.
It is an extended version of `consult--default-regexp-compiler' with migemo
to allow searching Japanese strings with romaji.
It also handles Japanese strings to be interpreted as cp932 on Windows machines.
The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
  (setq input (mapcar #'migemo-get-pattern (consult--split-escaped input)))
  (cons (mapcar (lambda (x) (let ((y (consult--convert-regexp x type)))
                              (if (multibyte-string-p x)
                                  (encode-coding-string y 'cp932)
                                y))) input)
        (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
          (apply-partially #'consult--highlight-regexps regexps ignore-case))))


;;;###autoload
(defun +japanese--consult--windows-regexp-compiler-fn (input type ignore-case)
  "Compile the INPUT string to a list of regular expressions.
It is an extended version of `consult--default-regexp-compiler' which handles
Japanese strings to be interpreted as cp932 on Windows machines.
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
          (apply-partially #'consult--highlight-regexps regexps ignore-case))))
