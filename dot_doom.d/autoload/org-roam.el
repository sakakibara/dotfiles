;;; autoload/org-roam.el -*- lexical-binding: t; -*-

(defvar org-roam-old-slug nil)

;;;###autoload
(defun org-roam-update-slug-on-save-h ()
  "Set up auto-updating for the current node's filename.

Calls `org-roam-update-slug-h' on `after-save-hook'."
  (when (org-roam-buffer-p)
    (setq-local org-roam-old-slug (ignore-errors (org-roam-node-slug (org-roam-node-at-point))))
    (add-hook 'after-save-hook #'org-roam-update-slug-h
              'append 'local)))

;;;###autoload
(defun org-roam-update-slug-h ()
  "Rename the current file if #+title has changed.

Will ask for confirmation if the new filename already exists."
  (when (org-roam-buffer-p)
    (when-let* ((node (org-roam-node-at-point))
                (new-slug (org-roam-node-slug node))
                (old-slug org-roam-old-slug)
                (old-slug-re (concat "/[^/]*\\(" (regexp-quote old-slug) "\\)[^/]*\\.org$"))
                (file-name (org-roam-node-file node))
                ((not (equal old-slug new-slug)))
                ((string-match-p old-slug-re file-name)))
      (setq org-roam-old-slug new-slug)
      (condition-case _
          (let ((new-file-name
                 (replace-regexp-in-string
                  old-slug-re (regexp-quote new-slug)
                  file-name nil nil 1)))
            (message "Updating slug in filename (%S -> %S)" old-slug new-slug)
            (rename-file file-name new-file-name 1)
            (set-visited-file-name new-file-name t t)
            (org-roam-db-autosync--setup-file-h))
        (error
         (setq org-roam-old-slug old-slug))))))
