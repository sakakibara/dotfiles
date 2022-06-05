;;; autoload/syntax.el -*- lexical-binding: t; -*-

;;;###autoload
(defun treat-underscore-as-word ()
  (modify-syntax-entry ?_ "w"))
