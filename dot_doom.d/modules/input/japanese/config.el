;;; input/japanese/config.el -*- lexical-binding: t; -*-

(use-package! migemo
  :after-call after-find-file pre-command-hook
  :when (featurep! +migemo)
  :init
  (setq search-default-regexp-mode nil
        ;; migemo-options '("-q" "--emacs" "-i" "\a")
        migemo-options '("--quiet" "--nonewline" "--emacs")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix
        migemo-directory (concat doom-etc-dir "migemo/"))
  (if IS-WINDOWS
      (setq migemo-dictionary (expand-file-name "~/.local/share/migemo/utf-8/migemo-dict"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  :config
  (migemo-init)
  (use-package! avy-migemo
    :after (avy)
    :config (avy-migemo-mode 1))

  (when (featurep! :completion vertico)
    ;; Uncomment following lines to enable migemo for all searches.
    ;; (when-let ((fn (cond ((and (featurep! +migemo) IS-WINDOWS)
    ;;                       #'+japanese--consult--windows-migemo-regexp-compiler-fn)
    ;;                      ((featurep! +migemo)
    ;;                       #'+japanese--consult--migemo-regexp-compiler-fn)
    ;;                      (IS-WINDOWS
    ;;                       #'+japanese--consult--windows--regexp-compiler-fn))))
    ;;   (after! consult
    ;;     (setq consult--regexp-compiler fn)))

    (after! orderless
      (orderless-define-completion-style +orderless-default
        (orderless-matching-styles '(orderless-literal
                                     orderless-regexp)))

      (orderless-define-completion-style +orderless-migemo
        (orderless-matching-styles '(orderless-literal
                                     orderless-regexp
                                     +japanese--orderless-migemo)))

      (orderless-define-completion-style +orderless-initialism
        (orderless-matching-styles '(orderless-initialism
                                     orderless-literal)))

      (add-to-list 'completion-styles '+orderless-migemo t)

      (setq completion-category-overrides
            '((command (styles +orderless-initialism))
              (file (styles +vertico-basic-remote orderless partial-completion
                            +orderless-migemo))
              (buffer (styles +orderless-migemo))
              (symbol (styles +orderless-default))
              (consult-location (styles +orderless-migemo))
              (consult-multi (styles +orderless-migemo))
              (org-roam-node (styles +orderless-migemo))
              (unicode-name (styles +orderless-migemo))
              (variable (styles +orderless-default))
              (project-file (styles +orderless-migemo))))))

  (when (featurep! :completion helm)
    (after! helm (helm-migemo-mode +1))))


(when IS-WINDOWS
  (when (featurep! :completion vertico)
    (after! consult
      (setq consult--regexp-compiler #'+japanese--consult--windows-regexp-compiler-fn)))

  (use-package! tr-ime
    :commands tr-ime-advanced-install
    :init
    ;; Setup frame if the selected frame is an ordinary frame (w32)
    ;; Add the setup function to a hook if it is console or daemon mode
    (if (eq (framep (selected-frame)) 'w32)
        (+windows--w32-frame-setup)
      (add-hook 'server-after-make-frame-hook #'+japanese--w32-frame-setup))))


(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :init
  ;; replacing `chinese-two-byte' by `japanese'
  (setq pangu-spacing-chinese-before-english-regexp
        "\\(?1:\\cj\\)\\(?2:[0-9A-Za-z]\\)"
        pangu-spacing-chinese-after-english-regexp
        "\\(?1:[0-9A-Za-z]\\)\\(?2:\\cj\\)"
        ;; Always insert `real' space in text-mode including org-mode.
        pangu-spacing-real-insert-separtor t))


;;
;;; Hacks

(defadvice! +japanese--org-html-paragraph-a (args)
  "Join consecutive Japanese lines into a single long line without unwanted space
when exporting org-mode to html."
  :filter-args #'org-html-paragraph
  (cl-destructuring-bind (paragraph contents info) args
    (let* ((fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
             "\\1\\2"
             contents)))
      (list paragraph fixed-contents info))))
