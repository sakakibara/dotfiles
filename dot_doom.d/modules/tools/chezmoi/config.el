;;; tools/chezmoi/config.el -*- lexical-binding: t; -*-

(defvar chezmoi-dir "~/.local/share/chezmoi/"
  "Location of the chezmoi directory")

(defvar chezmoi-doom-private-dir (concat chezmoi-dir "dot_doom.d/")
  "Location of the chezmoi managed doom private directory")

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
  (when (featurep! :editor evil)
    (add-hook 'chezmoi-mode-hook #'+chezmoi--evil-h)))

(use-package! chezmoi-company
  :when (featurep! :completion company)
  :after chezmoi
  :config
  (add-hook 'chezmoi-mode-hook #'+chezmoi--company-backend-h))

(when (featurep! :editor file-templates)
  (advice-add #'+file-templates-in-emacs-dirs-p
              :override #'+chezmoi--file-templates-in-emacs-dirs-p-a)
  (advice-add #'+file-templates-get-short-path
              :override #'+chezmoi--file-templates-get-short-path-a))
