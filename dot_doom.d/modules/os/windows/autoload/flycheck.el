;;; os/windows/autoload/flycheck.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +windows--flycheck-sanitize-output-a (fn output &rest args)
  "Sanitized output of flycheck"
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output)))
    (apply fn sanitized-output args)))
