;;; tools/chezmoi/config.el -*- lexical-binding: t; -*-

(use-package! chezmoi
  :commands (chezmoi-write
             chezmoi-magit-status
             chezmoi-diff
             chezmoi-ediff
             chezmoi-find
             chezmoi-write-files
             chezmoi-open-other
             chezmoi-template-buffer-display
             chezmoi-mode)
  :config
  ;; Company integration
  (when (featurep! :completion company)
    (defun +chezmoi--company-backend-h ()
      (require 'chezmoi-company)
      (if chezmoi-mode
          (add-to-list 'company-backends 'chezmoi-company-backend)
        (delete 'chezmoi-company-backend 'company-backends)))

    (add-hook 'chezmoi-mode-hook #'+chezmoi--company-backend-h))

  ;; Integrate with evil mode by toggling template display when entering insert mode.
  (when (featurep! :editor evil)
    (add-hook 'chezmoi-mode-hook #'+chezmoi--evil-h)))
