;;; os/windows/config.el -*- lexical-binding: t; -*-

;; Setup windows specific coding system as late as possible to override
;; settings in $DOOMDIR/config.el
(add-hook 'after-init-hook #'+windows--coding-system-setup)

(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; Encode subprocess parameters as cp932
(cl-loop for (func args-pos) in '((call-process        4)
                                  (call-process-region 6)
                                  (start-process       3))
         do (eval `(advice-add ',func
                               :around (lambda (orig-fun &rest args)
                                         (setf (nthcdr ,args-pos args)
                                               (mapcar (lambda (arg)
                                                         (if (multibyte-string-p arg)
                                                             (encode-coding-string arg 'cp932)
                                                           arg))
                                                       (nthcdr ,args-pos args)))
                                         (apply orig-fun args))
                               '((depth . 99)))))

(modify-coding-system-alist 'file "\\.sql\\'" 'cp932-dos)
(setq doom-leader-alt-key "C-M-SPC"
      doom-localleader-alt-key "C-M-SPC m"
      ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))

(after! comint
  (add-hook 'comint-output-filter-functions '+windows--coding-system-for-buffer-process))

;;
;;; Printing

(after! ps-print
  (setq ps-lpr-command (executable-find "gswin64c")
        ps-lpr-switches '("-q" "-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")))

(after! ps-mule
  (setq ps-multibyte-buffer 'bdf-font-except-latin
        bdf-directory-list
        (mapcar (lambda (d)
                  (concat (expand-file-name "~/.intlfonts/") d))
                '("Japanese" "Japanese.X" "Japanese.BIG"
                  "Misc" "TrueType" "Type1"))))

;;
;;; Packages

(when (modulep! :completion vertico)
  (after! consult
    (setq! consult-async-refresh-delay 0.3
           consult-async-input-throttle 0.6
           consult-async-input-debounce 0.3)))

(after! projectile
  (advice-add #'projectile-files-via-ext-command
              :around #'+windows--coding-system-for-rw-utf-8-cp932-a))

(after! flycheck
  (advice-add #'flycheck-parse-output :around #'+windows--flycheck-sanitize-output-a))

(after! undo-tree
    (setq undo-limit 5600000           ; 5600kb (default is 160kb)
          undo-strong-limit 84000000   ; 84mb  (default is 240kb)
          undo-outer-limit 896000000)) ; 896mb (default is 24mb)

(after! text-mode
    (add-hook 'text-mode-hook #'disable-require-final-newline))

(when (modulep! :lang javascript)
  (after! add-node-modules-path
    (setq add-node-modules-path-command (+windows--build-add-node-modules-path-command))))

(use-package! good-scroll
  :hook (doom-init-ui . good-scroll-mode))

(use-package! w32-browser
  :after-call dired-before-readin-hook
  :commands dired-w32-browser dired-w32explore
  :config
  (map! :map dired-mode-map
        ;; "C-RET" #'dired-w32-browser
        :gn [mouse-2] #'dired-mouse-w32-browser
        :gn [C-return] #'dired-w32-browser
        :gn [C-M-return] #'dired-w32explore))

(after! dired
  (setq dired-listing-switches "-alG"
        ls-lisp-dirs-first t
        ls-lisp-ignore-case t
        ls-lisp-UCA-like-collation nil))

(use-package! alert-toast
  :after alert
  :config
  (advice-add #'alert-toast--coding-page
              :override #'+windows--alert-toast--coding-page)
  (setq alert-default-style 'toast))

(after! ob-plantuml
  (let ((cmdline (string-join `("-charset" "utf-8" "-config") " ")))
    (setq org-babel-default-header-args:plantuml
          `((:cmdline . ,cmdline)
            (:results . "file")
            (:exports . "results")))))
