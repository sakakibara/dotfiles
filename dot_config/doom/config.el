;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-coding-system-priority 'utf-8 'utf-8-with-signature 'cp932 'euc-jp 'iso-2022-jp)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq system-time-locale "C"
      ;; frame-title-format '("%b – Emacs")
      frame-title-format (format "emacs %s - %%b" emacs-version)
      icon-title-format frame-title-format
      delete-by-moving-to-trash t
      server-client-instructions nil
      byte-compile-warnings '(not cl-functions obsolete))

;; (and IS-MAC
;;      (not (fboundp 'system-move-file-to-trash))
;;      (defalias 'system-move-file-to-trash #'ns-move-file-to-trash))

(setq-default indicate-empty-lines t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)
(setq-default evil-symbol-word-search t)

(remove-hook! 'doom-first-input-hook #'evil-snipe-override-mode)

(setq fancy-splash-image (concat doom-user-dir "splash.svg"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(setq +doom-dashboard-banner-padding '(2 . 2))

(add-hook 'after-change-major-mode-hook #'treat-underscore-as-word)

(after! ps-print
  (setq ps-printer-name t
        ps-print-header nil ;; hide header
        ps-paper-type 'a4))

(after! ps-mule
  (setq ps-multibyte-buffer 'non-latin-printer))

(after! undo-tree
  (setq undo-tree-visualizer-diff nil)
  (add-hook! '(evil-local-mode-hook wdired-mode-hook) #'turn-on-undo-tree-mode))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-skip-leading-whitespace nil))

(setq all-the-icons-scale-factor 0.9)

(setq +modeline-height 22)

(when (modulep! :editor lispy)
  (after! lispyville
    (remove-hook! 'evil-escape-inhibit-functions #'+lispy-inhibit-evil-escape-fn)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sho Sakakibara"
      user-mail-address "foo@bar.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Sarasa Term J Nerd Font" :size 14)
      doom-big-font (font-spec :family "Sarasa Term J Nerd Font" :size 19)
      ;; doom-unicode-font (font-spec :family "Sarasa Term J Nerd Font")
      doom-variable-pitch-font (font-spec :family "Sarasa Term J Nerd Font" :size 14))

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::"))

(when (modulep! :ui deft)
  (after! deft
    (setq deft-directory org-directory)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map!
 ;; :textobj "e" #'+evil:whole-buffer-txtobj #'+evil:whole-buffer-txtobj
 :textobj "l" #'+evil:inner-line-txtobj #'+evil:outer-line-txtobj

 :leader

 (:prefix ("E" . "encoding")
  :desc "Buffer coding system" "b" #'revert-buffer-with-coding-system
  :desc "File coding system" "f" #'set-buffer-file-coding-system)

 (:prefix "o"
  :desc "Grep" "g" #'rg)

 (:when (modulep! :emacs undo +tree)
   (:prefix "s"
    :desc "Undo history" "u" #'undo-tree-visualize)))

(use-package! eaw
  :hook (doom-first-input . eaw-fullwidth))

(when (or IS-WINDOWS IS-WSL)
  (after! projectile
    (dolist (file '(".vs") )
      (push file projectile-project-root-files-bottom-up))
    (setq! projectile-files-cache-expire 14400
           projectile-project-name-function #'+projectile-svn-aware-project-name
           projectile-project-root-functions '(projectile-root-local
                                               projectile-root-bottom-up
                                               projectile-root-top-down
                                               projectile-root-top-down-recurring
                                               +projectile-root-parent))))

(after! company
  (setq company-idle-delay nil))

(after! company-box
  (when (daemonp)
    (setq company-box-enable-icon nil))

  ;; Temporary fix until the following patch has been merged
  ;; https://github.com/sebastiencs/company-box/pull/177
  (defun company-box-doc--make-buffer (object)
    (let* ((buffer-list-update-hook nil)
           (inhibit-modification-hooks t)
           (string (cond ((stringp object) object)
                         ((bufferp object) (with-current-buffer object (buffer-string))))))
      (when (and string (> (length (string-trim string)) 0))
        (with-current-buffer (company-box--get-buffer "doc")
          (erase-buffer)
          (insert string)
          (setq mode-line-format nil
                display-line-numbers nil
                header-line-format nil
                show-trailing-whitespace nil
                cursor-in-non-selected-windows nil)

          (toggle-truncate-lines -1) ;; PATCHED HERE

          (current-buffer)))))

  ;; Temporary fix for child frame height until the following patch has been merged
  ;; https://github.com/sebastiencs/company-box/pull/181
  (advice-add #'company-box--compute-frame-position :override #' +company-box--compute-frame-position-a))

(after! dired-x
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\desktop\\.ini\\'"
                "\\|^~\\$.+\\.\\(xls\\|doc\\|ppt\\)?.\\'"))
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start \"\" ")))
    (add-to-list 'dired-guess-shell-alist-user `("\\.xls.?\\'" ,cmd))
    (add-to-list 'dired-guess-shell-alist-user `("\\.doc.?\\'" ,cmd))
    (add-to-list 'dired-guess-shell-alist-user `("\\.ppt.?\\'" ,cmd))))

(use-package! rg
  :commands rg rg-literal rg-dwim
  :config
  (add-to-list 'rg-custom-type-aliases '("tsv" . "*.tsv"))
  (add-to-list 'rg-custom-type-aliases '("dat" . "*.dat")))

(after! org
  ;; (add-hook 'org-mode-hook #'display-line-numbers--turn-off)
  (add-to-list 'org-file-apps '("\\.xls.?\\'" . default))
  (add-to-list 'org-file-apps '("\\.doc.?\\'" . default))
  (setq org-startup-folded 'show2levels
        org-ellipsis " ... "
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-block-separator ?─
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

  (setq! org-log-into-drawer t)
  (add-to-list 'org-modules 'org-habit)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s!)"  ; A task that is in progress
           "WAIT(w@/!)"  ; Something external is holding up this task
           "HOLD(h@/!)"  ; This task is paused/on hold because of me
           "IDEA(i!)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k@/!)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o!)"
           "YES(y!)"
           "NO(n!)")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("d" "Deadline" entry (file+headline "todo.org" "Schedule")
           "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "Schedule" entry (file+headline "todo.org" "Schedule")
           "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t))))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :config
  (let ((active-bg "#a60000")
        (hold-bg "#813e00")
        (project-bg "#2a486a"))
    (setq org-modern-todo-faces
          `(("TODO"
             :foreground "white"
             :background ,active-bg
             :weight bold)
            ("[-]"
             :foreground "white"
             :foreground ,active-bg
             :weight bold)
            ("STRT"
             :foreground "white"
             :background ,active-bg
             :weight bold)
            ("[?]"
             :foreground "white"
             :foreground ,hold-bg
             :weight bold)
            ("WAIT"
             :foreground "white"
             :background ,hold-bg
             :weight bold)
            ("HOLD"
             :foreground "white"
             :background ,hold-bg
             :weight bold)
            ("PROJ"
             :foreground "white"
             :background ,project-bg
             :weight bold)
            ("NO"
             :background gray90
             :weight bold)
            ("KILL"
             :background gray90
             :weight bold)))))

(use-package org-modern-indent
  :hook (org-mode . org-modern-indent-mode))

(setq org-roam-directory org-directory
      org-roam-db-location (concat org-directory ".org-roam.db")
      org-roam-dailies-directory "journal/")

(map! :nv "zL" #'evil-scroll-right
      :nv "zH" #'evil-scroll-left)

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line)))
       :n "zq"  #'org-babel-hide-result-toggle))

(after! org-roam
  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)
  (add-hook 'doom-switch-buffer-hook #'org-roam-update-slug-on-save-h)

  (setq org-roam-capture-templates
        `(("n" "Note" plain
           "%?"
           :target (file+head "note/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("r" "Thought" plain
           "%?"
           :target (file+head "thought/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("t" "Topic" plain
           "%?"
           :target (file+head "topic/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "Contact" plain
           "%?"
           :target (file+head "contact/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "Project" plain
           "%?"
           :target (file+head "project/%<%Y%m%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("f" "Ref" plain
           "%?"
           :target (file+head "ref/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("w" "Works" plain
           "%?"
           :target (file+head "works/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))

        org-roam-dailies-capture-templates
        '(("d" "Default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))))

;; (when IS-WINDOWS
;;   (use-package! org-wild-notifier
;;     :hook (org-load . org-wild-notifier-mode)
;;     :config
;;     (setq! org-wild-notifier-alert-time '(10 5 1 0))
;;     (add-hook 'kill-emacs-hook #'org-wild-notifier--stop)))

(after! ox
  (setq org-export-with-sub-superscripts nil))

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}
[NO-DEFAULT-PACKAGES]
\\usepackage{luatexja}
\\usepackage{textcomp}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{hyperref}
\\hypersetup{pdfencoding=auto, linkbordercolor={0 1 0}}
\\usetheme{metropolis}
\\setmainfont[BoldFont=TeXGyreHeros, Ligatures=TeX]{TeXGyreTermes}
\\setsansfont[Ligatures=TeX]{TeXGyreHeros}
\\usepackage{luacode}
\\usepackage{luatexja-otf}
\\usepackage[ipaex]{luatexja-preset}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
\\setbeamercovered{transparent}
\\setbeamertemplate{navigation symbols}{}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("ltjsarticle"
                 "\\documentclass[12pt,a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{luatexja}
\\usepackage{luatexja-fontspec}
\\usepackage[hiragino-pron]{luatexja-preset}
\\usepackage{float}
\\usepackage{textcomp}
\\usepackage{graphicx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage[dvipsnames,svgnames,x11names,hyperref]{xcolor}
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true,
  urlcolor=DodgerBlue4,
  linkcolor=DodgerBlue4,
  citecolor=RubineRed,
  pdfencoding=auto
}
\\usepackage[nameinlink]{cleveref}
\\crefname{equation}{式}{式}
\\crefname{figure}{図}{図}
\\crefname{table}{表}{表}
\\crefname{algorithm}{Algorithm}{Algorithm}
\\crefname{section}{第}{第}
\\creflabelformat{section}{#2#1節#3}
\\crefname{subsection}{第}{第}
\\creflabelformat{subsection}{#2#1小節#3}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "ltjsarticle"
        org-beamer-outline-frame-title "目次")
  (setq! org-latex-compiler "lualatex"
         org-latex-reference-command "\\cref{%s}"
         org-latex-toc-command "\\tableofcontents \\clearpage")
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(after! lsp-mode
  (setq lsp-ui-sideline-show-code-actions nil))

(after! evil
  (add-hook 'evil-insert-state-entry-hook #'evil-insert-state-entry-display-numbers-h)
  (add-hook 'evil-insert-state-exit-hook #'evil-insert-state-exit-display-numbers-h))

(after! whitespace
  (setq whitespace-display-mappings
        '((newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  (set-face-underline 'whitespace-tab t))

(after! dired
  (map! :map dired-mode-map
        :ng "q" #'quit-window))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(use-package! xonsh-mode
  :defer t)

(use-package! elvish-mode
  :defer t)

(after! rjsx-mode
  (setq rjsx-comment-start-skip "[[:space:]]*\\(?://+\\|{?/\\*+\\)")
  (advice-add #'rjsx-uncomment-region-function :override #'+rjsx--uncomment-region-function))

(use-package! ligature
  :hook (doom-first-buffer . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                               "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                               "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                               ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;; Load local lisp file if it exists
(load! "local" nil t)
