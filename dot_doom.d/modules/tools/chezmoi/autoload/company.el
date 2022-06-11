;;; tools/chezmoi/autoload/company.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--company-backend-h ()
  (if chezmoi-mode
      (add-to-list 'company-backends 'chezmoi-company-backend)
    (delete 'chezmoi-company-backend 'company-backends)))
