;;; input/japanese/autoload/orderless.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +japanese--orderless-migemo (component)
      (let ((pattern (migemo-get-pattern component)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
