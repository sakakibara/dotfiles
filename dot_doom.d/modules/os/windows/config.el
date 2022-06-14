;;; os/windows/config.el -*- lexical-binding: t; -*-

;; Setup windows specific coding system as late as possible to override
;; settings in $DOOMDIR/config.el
(add-hook! 'doom-after-init-modules-hook
           '(+windows--coding-system-setup
             +windows--remove-javascript-npm-mode-hook-h))

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

;;
;;; Printing

(after! ps-print
  (setq ps-lpr-command (executable-find "gswin64c")
        ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")))

;;
;;; Packages

(when (featurep! :completion vertico)
  (after! consult
    (setq! consult-async-refresh-delay 0.3
           consult-async-input-throttle 0.6
           consult-async-input-debounce 0.3)))

(after! projectile
  (advice-add #'projectile-files-via-ext-command
              :around #'+windows--coding-system-for-rw-utf-8-cp932-a))

(after! flycheck
  (advice-add 'flycheck-parse-output :around #'+windows--flycheck-sanitize-output-a))

(after! undo-tree
    (setq undo-limit 5600000           ; 5600kb (default is 160kb)
          undo-strong-limit 84000000   ; 84mb  (default is 240kb)
          undo-outer-limit 896000000)) ; 896mb (default is 24mb)

(after! text-mode
    (add-hook 'text-mode-hook #'disable-require-final-newline))

(after! outline-mode
    (add-hook 'outline-mode-hook #'enable-require-final-newline))

(after! csv-mode
    (add-hook 'csv-mode-hook #'disable-require-final-newline))

(use-package! w32-browser
  :after-call dired-before-readin-hook
  :commands dired-w32-browser dired-w32explore
  :config
  (map! :map dired-mode-map
        ;; "C-RET" #'dired-w32-browser
        :gn [mouse-2] #'dired-mouse-w32-browser
        :gn [C-return] #'dired-w32-browser
        :gn [C-M-return] #'dired-w32explore))

(after! ob-plantuml
  (let ((cmdline (string-join `("-charset" "utf-8" "-config") " ")))
    (setq org-babel-default-header-args:plantuml
          `((:cmdline . ,cmdline)
            (:results . "file")
            (:exports . "results")))))
