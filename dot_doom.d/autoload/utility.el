;;; autoload/utility.el -*- lexical-binding: t; -*-

;;;###autoload
(defun replace-nested-list-element (list-var element)
  "Replace element in LIST-VAR matching `car' with ELEMENT."
  (let* ((key (car element))
         (content (cdr element))
         (target (assoc key list-var)))
    (setcdr target content)))
