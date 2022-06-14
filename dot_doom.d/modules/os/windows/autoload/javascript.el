;;; os/windows/autoload/javascript.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +windows--remove-javascript-npm-mode-hook-h ()
  (dolist (mode '(html-mode
                  css-mode
                  web-mode
                  markdown-mode
                  js-mode
                  json-mode
                  typescript-mode
                  solidity-mode))
    (remove-hook (intern (format "%s-hook" mode))
                 (intern (format "doom--enable-%s-in-%s-h"
                                 '+javascript-npm-mode mode)))))
