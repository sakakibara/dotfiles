;;; init.el -*- lexical-binding: t; -*-

;;;; Initial buffer

(setq inhibit-startup-screen t
      initial-buffer-choice t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil) ;;*scratch*

;;;; Package manager

;;;;; Elpaca

;; Bootstrapping elpaca package manager
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(when IS-WINDOWS
  (setq elpaca-queue-limit 12))

;; Block until current queue processed.
(elpaca-wait)

;;;; Prioritized packages

;;;;; Benchmark

;; Package to benchmark emacs requiring and loading functions at startup.
;; It can be used to keep track of where time is being spent during Emacs startup in order to optimize startup times.

(use-package benchmark-init
  ;; :disabled
  :demand t
  :hook
  (elpaca-after-init . benchmark-init/deactivate))

;;;;; Theme

(load-theme 'modus-operandi :no-confirm)

(use-package modus-themes
  :disabled
  :config
  (load-theme 'modus-operandi :no-confirm))

(use-package catppuccin-theme
  :disabled
  :demand t
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

(elpaca-wait)

;;;;; Data file management

;; The default paths used to store configuration files and persistent data are not consistent across Emacs packages.
;; This package makes those files be kept in consistent locations withtin the config directory.

(use-package no-littering
  :demand t)

;;;;; Keybinding

(use-package general
  :demand t
  :config

  ;;; General settings

  (general-override-mode)
  (general-auto-unbind-keys)

  ;;; Helper macros to make keybinding easier

  (defmacro cmd! (&rest body)
    "Returns (cmd! ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
    (declare (doc-string 1) (pure t) (side-effect-free t))
    `(lambda (&rest _) (interactive) ,@body))

  (defmacro cmd-with-prefix! (command &optional prefix-arg &rest args)
    "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
    (declare (doc-string 1) (pure t) (side-effect-free t))
    `(lambda (arg &rest _) (interactive "P")
       (let ((current-prefix-arg (or ,prefix-arg arg)))
         (,(if args
               #'funcall-interactively
             #'call-interactively)
          ,command ,@args))))

  ;;; Keybindings for global and local leader menu

  (general-def
    :keymaps 'override
    :states '(insert normal hybrid motion visual operator emacs)
    :prefix-map '+global-leader-prefix-map
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer global-leader-def!
    :wk-full-keys nil
    :keymaps '+global-leader-prefix-map)

  (general-create-definer local-leader-def!
    :keymaps 'override
    :states '(insert normal hybrid motion visual operator)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    "" '(:ignore t
         :wk
         (lambda (arg)
           (cons (cadr (split-string (car arg) " "))
                 (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (defmacro global-leader-menu-def! (name prefix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
  Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "global-leader-" name))
           (nm (concat n "-def!"))
           (prefix-map (intern (concat "+" n "-map"))))
      `(progn
         (general-create-definer ,(intern nm)
           :wrapping global-leader-def!
           :prefix-map (quote ,prefix-map)
           :prefix ,prefix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,name))
         (,(intern nm) ,@body))))

  (global-leader-def!
    ";" '(pp-eval-expression :wk "eval expression")
    ":" '(execute-extended-command :wk "M-x")
    "u" '(universal-argument :wk "universal argument")
    "w" '(evil-window-map :wk "window")
    "'" '(vertico-repeat :wk "resume last search")
    "SPC" '(consult-fd :wk "find file in project")
    "RET" '(bookmark-jump :wk "jump to bookmark")
    "X" '(org-capture :wk "org capture")
    "z" 'text-scale-adjust
    "Z" 'global-text-scale-adjust)

  (global-leader-menu-def! "application" "a"
    "a" '(org-agenda "org agenda")
    "f" '(make-frame "make frame")
    "F" '(select-frame-by-name "select frame by name"))

  (global-leader-menu-def! "buffer" "b"
    "-" '(+toggle-narrow-buffer :wk "toggle narrowing")
    "[" '(previous-buffer :wk "previous buffer")
    "]" '(next-buffer :wk "next buffer")
    "b" '(consult-buffer :wk "switch buffer")
    "c" '(clone-indirect-buffer :wk "clone buffer")
    "C" '(clone-indirect-buffer-other-window :wk "clone buffer other window")
    "d" '(kill-current-buffer :wk "kill buffer")
    "i" '(ibuffer-jump :wk "ibuffer")
    "l" '(evil-switch-to-windows-last-buffer :wk "switch to last buffer")
    "m" '(bookmark-set :wk "switch to last buffer")
    "M" '(bookmark-delete :wk "delete bookmark")
    "n" '(next-buffer :wk "next buffer")
    "N" '(evil-buffer-new :wk "new empty buffer")
    "p" '(previous-buffer :wk "previous buffer")
    "r" '(revert-buffer :wk "revert buffer")
    "R" '(rename-buffer :wk "rename buffer")
    "s" '(basic-save-buffer :wk "save buffer")
    "S" '(evil-write-all :wk "save all buffers"))

  (global-leader-menu-def! "file" "f"
    "c" '(editorconfig-find-current-editorconfig :wk "find current editorconfig")
    "C" '(+copy-this-file :wk "copy this file")
    "D" '(+delete-this-file :wk "delete this file")
    "s" '(save-buffer :wk "save file")
    "S" '(write-file :wk "save file as...")
    "b" '(find-file :wk "find file")
    "f" '(consult-fd :wk "fd")
    "F" '(+consult-fd-cwd :wk "fd (cwd)")
    "g" '(consult-ripgrep :wk "ripgrep")
    "G" '(+consult-ripgrep-cwd :wk "ripgrep (cwd)")
    "r" '(consult-recent-file :wk "recent files")
    "R" '(+move-this-file :wk "recent files")
    "s" '(save-buffer :wk "save file")
    "S" '(write-file :wk "save file as...")
    "y" '(+yank-buffer-path :wk "yank file path")
    "Y" '(+yank-buffer-path-relative-to-project :wk "yank file path from project"))

  (global-leader-menu-def! "encoding" "e"
    "b" 'revert-buffer-with-coding-system
    "f" 'set-buffer-file-coding-system)

  (global-leader-menu-def! "search" "s"
    "b" '+search-project
    "B" (cmd-with-prefix! #'consult-line-multi 'all-buffers)
    "f" 'locate
    "h" 'consult-history
    "i" 'consult-imenu
    "I" '(consult-imenu-multi :wk "jump to symbol in open buffers")
    "j" '(evil-show-jumps :wk "jump to link")
    "k" 'consult-kmacro
    "m" '(bookmark-jump :wk "jump to bookmark")
    "o" 'consult-outline
    "r" '(evil-show-marks :wk "jump to mark")
    "s" '(+search-buffer :wk "search buffer")
    "u" '(undo-tree-visualize :wk "undo history")
    "f" '(find-file :wk "find file")
    "l" '(locate :wk "locate")
    "l" '(ffap-menu :wk "jump to link"))

  (global-leader-menu-def! "git" "g"
    "g" '(:ignore t :wk "git")
    "R" 'vc-revert
    "/" '(magit-dispatch :wk "magit dispatch")
    "." '(magit-file-dispatch :wk "magit file dispatch")
    "'" '(forge-dispatch :wk "forge dispatch")
    "b" '(magit-branch-checkout :wk "magit switch branch")
    "g" '(magit-status :wk "magit status")
    "G" '(magit-status-here :wk "magit status here")
    "D" '(magit-file-delete :wk "magit file delete")
    "B" '(magit-blame-addition :wk "magit blame")
    "C" '(magit-clone :wk "magit clone")
    "F" '(magit-fetch :wk "magit fetch")
    "L" '(magit-log-buffer-file :wk "magit buffer log")
    "S" '(magit-stage-file :wk "git stage file")
    "U" '(magit-unstage-file :wk "git unstage file")
    "l" '(:ignore t :wk "file")
    "fF" (cmd! (consult-fd default-directory))
    "fg" 'consult-ripgrep
    "fG" (cmd! (consult-ripgrep default-directory))
    "fc" '(magit-show-commit :wk "find commit")
    "fi" '(forge-visit-issue :wk "find issue")
    "fp" '(forge-visit-pullreq :wk "find pull request")
    "fn" '(forge-list-notifications :wk "list notifications")
    "ol" '(:ignore t :wk "open in browser")
    "or" '(forge-browse-remote :wk "browse remote")
    "oc" '(forge-browse-commit :wk "browse commit")
    "oi" '(forge-browse-issue :wk "browse an issue")
    "op" '(forge-browse-pullreq :wk "browse a pull request")
    "oI" '(forge-browse-issues :wk "browse issues")
    "oP" '(forge-browse-pullreqs :wk "browse pull requests")
    "l" '(:ignore t :wk "list")
    "lr" '(magit-list-repositories :wk "list repositories")
    "ls" '(magit-list-submodules :wk "list submodules")
    "li" '(forge-list-issues :wk "list issues")
    "lp" '(forge-list-pullreqs :wk "list pull requests")
    "ln" '(forge-list-notifications :wk "list notifications")
    "c" '(:ignore t :wk "list")
    "lr" '(magit-init :wk "initialize repo")
    "lR" '(magit-clone :wk "clone repo")
    "lc" '(magit-commit-create :wk "commit")
    "lf" '(magit-commit-fixup :wk "fixup")
    "lb" '(magit-branch-and-checkout :wk "branch")
    "li" '(forge-create-issue :wk "issue")
    "lp" '(forge-create-pullreq :wk "pull request"))

  (global-leader-menu-def! "help" "h"
    "f" 'describe-function
    "F" 'describe-face
    "v" 'describe-variable
    "b" 'describe-bindings
    "k" 'describe-key
    "'" 'describe-char
    "P" 'find-library)

  (global-leader-menu-def! "notes" "n"
    "a" 'org-agenda
    "c" '(+org-toggle-last-clock :wk "toggle last org-clock")
    "l" '(org-store-link :wk "org store link")
    "C" '(org-clock-cancel :wk "cancel current org-clock")
    "m" '(org-tags-view :wk "tags search")
    "n" '(org-capture :wk "org capture")
    "N" '(org-capture-goto-target :wk "goto capture")
    "t" '(org-todo-list :wk "todo list")
    "s" '(+consult-ripgrep-org :wk "todo list")
    "v" '(org-search-view :wk "view search")
    "r" '(:ignore t :wk "roam")
    "ra" '(org-roam-node-random :wk "open random node")
    "rf" '(org-roam-node-find :wk "find node")
    "rF" '(org-roam-ref-find :wk "find ref")
    "rg" '(org-roam-graph :wk "show graph")
    "ri" '(org-roam-node-insert :wk "insert node")
    "rn" '(org-roam-capture :wk "capture to node")
    "rr" '(org-roam-buffer-toggle :wk "toggle roam buffer")
    "rR" '(org-roam-buffer-display-dedicated :wk "launch roam buffer")
    "rs" '(org-roam-db-sync :wk "sync database")
    "rd" '(:ignore t :wk "date")
    "rdd" '(org-roam-dailies-goto-date :wk "goto date")
    "rdD" '(org-roam-dailies-capture-date :wk "capture date")
    "rdm" '(org-roam-dailies-goto-tomorrow :wk "goto tomorrow")
    "rdM" '(org-roam-dailies-capture-tomorrow :wk "capture tomorrow")
    "rdn" '(org-roam-dailies-capture-today :wk "capture today")
    "rdt" '(org-roam-dailies-goto-today :wk "goto today")
    "rdT" '(org-roam-dailies-capture-today :wk "capture today")
    "rdy" '(org-roam-dailies-goto-yesterday :wk "goto yesterday")
    "rdY" '(org-roam-dailies-capture-yesterday :wk "capture yesterday")
    "rd-" '(org-roam-dailies-find-directory :wk "find directory"))

  (global-leader-menu-def! "toggle" "t"
    "w" 'visual-line-mode
    "n" 'display-line-numbers-mode)

  (global-leader-menu-def! "open" "o"
    "f" 'make-frame
    "F" 'select-frame-by-name)

  (global-leader-menu-def! "project" "p"
    "!" '(projectile-run-shell-command-in-root :wk "run cmd in project root")
    "&" '(projectile-run-async-shell-command-in-root :wk "async cmd in project root")
    "a" '(projectile-add-known-project :wk "add new project")
    "b" '(projectile-switch-to-buffer :wk "switch to project buffer")
    "c" '(projectile-compile-project :wk "compile in project")
    "C" '(projectile-repeat-last-command :wk "repeat last command")
    "d" '(projectile-remove-known-project :wk "remove known project")
    "e" '(projectile-edit-dir-locals :wk "edit project .dir-locals")
    "f" '(projectile-find-file :wk "find file in project")
    "g" '(projectile-configure-project :wk "configure project")
    "i" '(projectile-invalidate-cache :wk "invalidate project cache")
    "k" '(projectile-kill-buffers :wk "kill project buffers")
    "o" '(projectile-find-other-file :wk "find other file")
    "p" '(projectile-switch-project :wk "switch project")
    "r" '(projectile-recentf :wk "find recent project files")
    "R" '(projectile-run-project :wk "run project")
    "s" '(projectile-save-project-buffers :wk "save project files")
    "t" '(magit-todos-list :wk "list project todos")
    "T" '(projectile-test-project :wk "test project"))

  (global-leader-menu-def! "quit" "q"
    "f" '(delete-frame :wk "delete frame")
    "k" '(save-buffers-kill-emacs :wk "kill emacs (and daemon)")
    "K" '(save-buffers-kill-terminal :wk "quit emacs")
    "p" '(evil-quit-all-with-error-code :wk "quit emacs without saving")))

(elpaca-wait)

;;;;; elpaca

(use-package elpaca
  :ensure nil
  :general
  (:keymaps 'elpaca-ui-mode-map
   :states '(normal)
   "U" 'elpaca-ui-mark-merge))

;;;; Builtin

;;;;; seq

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq
  :ensure `(seq
            ;; :type git
            ;; :host nil
            ;; :repo "https://git.savannah.gnu.org/git/emacs/elpa.git"
            ;; :branch "externals/seq"
            :build ,(+elpaca-seq-build-steps)))

;;;;; emacs

(use-package emacs
  :ensure nil
  :config
  (setq byte-compile-warnings '(not cl-functions obsolete)
        completion-styles '(flex basic partial-completion)
        create-lockfiles nil
        delete-by-moving-to-trash t
        enable-recursive-minibuffers t
        find-library-include-other-files nil
        frame-title-format (format "emacs - %%b")
        history-delete-duplicates t
        hscroll-margin 0
        hscroll-step 0
        icon-title-format frame-title-format
        indent-tabs-mode nil
        inhibit-compacting-font-caches t
        make-backup-files nil
        pgtk-use-im-context-on-new-connection nil
        ring-bell-function #'ignore
        scroll-conservatively 101
        sentence-end-double-space nil
        system-time-locale "C"
        tab-stop-list (number-sequence 2 120 2)
        tab-width 2
        truncate-lines t
        auto-save-include-big-deletions t)

  ;;; Coding system setup

  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (set-coding-system-priority 'utf-8 'utf-8-with-signature 'cp932 'euc-jp 'iso-2022-jp)
  (setq-default buffer-file-coding-system 'utf-8-unix)

  (when IS-WINDOWS
    (prefer-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-file-name-coding-system 'cp932)
    (setq locale-coding-system 'utf-8-unix
          default-process-coding-system '(undecided-dos . utf-8-unix))
    (setq-default selection-coding-system 'utf-16le-dos))

  (when IS-WINDOWS
    ;; Encode subprocess parameters as cp932
    (cl-loop for (func args-pos) in '((call-process        4)
                                      (call-process-region 6)
                                      (start-process       3))
             do (eval `(advice-add ',func
                                   :around (lambda (orig-fun &rest args)
                                             (setf (nthcdr ,args-pos args)
                                                   (mapcar (lambda (arg)
                                                             (encode-coding-string arg 'cp932))
                                                           (nthcdr ,args-pos args)))
                                             (apply orig-fun args))
                                   '((depth . 99)))))

    (modify-coding-system-alist 'file "\\.sql\\'" 'cp932-dos)
    (modify-coding-system-alist 'process "[rR][gG]" '(utf-8-dos . utf-8-unix))

    (add-hook 'comint-output-filter-functions '+coding-system-for-buffer-process)

    (defun +coding-system-for-rw-utf-8-cp932 (fn &rest args)
      "Use utf-8 for coding-system-for-read and cp932 for coding-system-for-write"
      (let ((coding-system-for-read 'utf-8)
            (coding-system-for-write 'cp932))
        (apply fn args)))

    (defun +coding-system-for-rw-undecided-utf-8 (fn &rest args)
      "Use undecided for coding-system-for-read and utf-8 for coding-system-for-write"
      (let ((coding-system-for-read 'undecided)
            (coding-system-for-write 'utf-8))
        (apply fn args)))

    (defun +coding-system-for-buffer-process (_)
      (when-let ((process (get-buffer-process (current-buffer))))
        (let ((coding-system (process-coding-system process)))
          (set-process-coding-system process
                                     (coding-system-change-text-conversion
                                      (car coding-system) 'undecided)
                                     (cdr coding-system))))))

  ;;; Data directories
  (defvar +data-dir
    (if IS-WINDOWS
        (expand-file-name "emacs/data/" (getenv-internal "APPDATA"))
      (expand-file-name "emacs/" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share")))
    "Where global data files are stored.")

  (defvar +cache-dir
    (if IS-WINDOWS
        (expand-file-name "emacs/cache/" (getenv-internal "APPDATA"))
      (expand-file-name "emacs/" (or (getenv-internal "XDG_CACHE_HOME") "~/.cache")))
    "Where global cache files are stored.")

  (defvar +state-dir
    (if IS-WINDOWS
        (expand-file-name "emacs/state/" (getenv-internal "APPDATA"))
      (expand-file-name "emacs/" (or (getenv-internal "XDG_STATE_HOME") "~/.local/state")))
    "Where global state files are stored.")

  ;;; Buffer utility
  (defun +yank-buffer-path (&optional root)
    "Copy the current buffer's path to the kill ring."
    (interactive)
    (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                          (bound-and-true-p list-buffers-directory)))
        (let ((path (abbreviate-file-name
                     (if root
                         (file-relative-name filename root)
                       filename))))
          (kill-new path)
          (if (string= path (car kill-ring))
              (message "Copied path: %s" path)
            (user-error "Couldn't copy filename in current buffer")))
      (error "Couldn't find filename in current buffer")))

  (defun +yank-buffer-path-relative-to-project (&optional include-root)
    "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
    (interactive "P")
    (+yank-buffer-path
     (if include-root
         (file-name-directory (directory-file-name (+project-root)))
       (+project-root))))

  ;;; Narrowing
  (defvar +narrowed-base-buffer nil)

  (defun +narrow-buffer-indirectly (beg end)
    "Restrict editing in this buffer to the current region, indirectly.

This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`+widen-indirectly-narrowed-buffer' to undo it (incrementally)."
    (interactive
     (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
           (or (bound-and-true-p evil-visual-end)       (region-end))))
    (unless (region-active-p)
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (deactivate-mark)
    (let ((orig-buffer (current-buffer)))
      (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
        (narrow-to-region beg end)
        (setq-local +narrowed-base-buffer orig-buffer))))

  (defun +widen-indirectly-narrowed-buffer (&optional arg)
    "Widens narrowed buffers.

This command will incrementally kill indirect buffers (under the assumption they
were created by `+narrow-buffer-indirectly') and switch to their base
buffer.

If ARG, then kill all indirect buffers, return the base buffer and widen it.

If the current buffer is not an indirect buffer, it is `widen'ed."
    (interactive "P")
    (unless (buffer-narrowed-p)
      (user-error "Buffer isn't narrowed"))
    (let ((orig-buffer (current-buffer))
          (base-buffer +narrowed-base-buffer))
      (cond ((or (not base-buffer)
                 (not (buffer-live-p base-buffer)))
             (widen))
            (arg
             (let ((buffer orig-buffer)
                   (buffers-to-kill (list orig-buffer)))
               (while (setq buffer (buffer-local-value '+narrowed-base-buffer buffer))
                 (push buffer buffers-to-kill))
               (switch-to-buffer (buffer-base-buffer))
               (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
            ((switch-to-buffer base-buffer)
             (kill-buffer orig-buffer)))))

  (defun +toggle-narrow-buffer (beg end)
    "Narrow the buffer to BEG END. If narrowed, widen it."
    (interactive
     (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
           (or (bound-and-true-p evil-visual-end)       (region-end))))
    (if (buffer-narrowed-p)
        (widen)
      (unless (region-active-p)
        (setq beg (line-beginning-position)
              end (line-end-position)))
      (narrow-to-region beg end)))

  (advice-add #'x-apply-session-resources :override #'ignore)

  (setq-default indicate-empty-lines t)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (defun +buffers-in-mode (modes &optional buffer-list derived-p)
    "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
    (let ((modes (ensure-list modes)))
      (cl-remove-if-not (if derived-p
                            (lambda (buf)
                              (apply #'provided-mode-derived-p
                                     (buffer-local-value 'major-mode buf)
                                     modes))
                          (lambda (buf)
                            (memq (buffer-local-value 'major-mode buf) modes)))
                        (or buffer-list (buffer-list))))))

;;;; All other packages

;;;;; anzu

(use-package anzu
  :defer t)

;;;;; apheleia

(use-package apheleia
  :disabled
  :hook (elpaca-after-init . apheleia-global-mode)
  :config
  (defun apheleia-lsp-formatter-buffer (buffer scratch)
    (with-current-buffer buffer
      (if (lsp-feature? "textDocument/formatting")
          (let ((edits (lsp-request
                        "textDocument/formatting"
                        (lsp--make-document-formatting-params))))
            (unless (seq-empty-p edits)
              (with-current-buffer scratch
                (lsp--apply-text-edits edits 'format)))))))

  (cl-defun apheleia-lsp-formatter
      (&key buffer scratch formatter callback &allow-other-keys)
    (apheleia-lsp-formatter-buffer buffer scratch)
    (funcall callback))

  (add-to-list 'apheleia-formatters '(apheleia-lsp . apheleia-lsp-formatter))

  (setf (alist-get 'powershell-mode apheleia-mode-alist)
        '(apheleia-lsp)))

;;;;; autorevert

(use-package autorevert
  :ensure nil
  :defer 2
  :config
  (setopt auto-revert-interval 0.01)
  (global-auto-revert-mode t))

;;;;; avy

(use-package avy
  :ensure nil
  :general
  (:states '(normal visual)
   "s" 'avy-goto-char-2)
  (:states '(operator)
   "z" 'avy-goto-char-2))

;;;;; chezmoi

(use-package chezmoi
  :commands (chezmoi-mode
             chezmoi-write
             chezmoi-magit-status
             chezmoi-diff
             chezmoi-ediff
             chezmoi-find
             chezmoi-write-files
             chezmoi-open-other
             chezmoi-template-buffer-display)
  :config
  (defun +chezmoi--evil-insert-state-enter ()
    "Run after evil-insert-state-entry."
    (chezmoi-template-buffer-display nil (point))
    (remove-hook 'after-change-functions #'chezmoi-template--after-change 1))

  (defun +chezmoi--evil-insert-state-exit ()
    "Run after evil-insert-state-exit."
    (chezmoi-template-buffer-display nil)
    (chezmoi-template-buffer-display t)
    (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

  (defun +chezmoi-evil ()
    (if chezmoi-mode
        (progn
          (add-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter nil 1)
          (add-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit nil 1))
      (progn
        (remove-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter 1)
        (remove-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit 1))))

  (add-hook 'chezmoi-mode-hook '+chezmoi-evil))

;;;;; corfu

(use-package corfu
  :defer 5
  :general
  (:states '(insert)
   "C-SPC" 'completion-at-point)
  (:keymaps 'corfu-map
   [return] 'corfu-insert
   "RET" 'corfu-insert
   [tab] 'corfu-next
   [backtab] 'corfu-previous
   "TAB" 'corfu-next
   "S-TAB" 'corfu-previous
   "M-m" '+corfu-move-to-minibuffer
   "M-J" '+corfu-move-to-minibuffer)
  :init
  (defun +corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          (completion-cycle-threshold completion-cycling))
      (apply #'consult-completion-in-region completion-in-region--data)))

  (defvar +cape-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
    "Size limit for a buffer to be scanned by `cape-line' or `cape-dabbrev'.
As an exception, `cape-line' will also scan buffers with the same
major mode regardless of size.")

  (with-eval-after-load 'orderless
    (setq orderless-component-separator #'orderless-escapable-split-on-space))

  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-separator ?\s
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  :config
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point))))

  (setq corfu-auto t
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary t
        corfu-quit-no-match 'separator
        tab-always-indent 'complete)

  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  ;;   (unless (corfu-disable-in-minibuffer-p)
  ;;     (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
  ;;                 corfu-popupinfo-delay nil)
  ;;     (corfu-mode +1)))

  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (defun corfu-visible-p ()
    (or (and (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
        (and (featurep 'corfu-terminal)
             (popon-live-p corfu-terminal--popon))))

  (defun +corfu--insert-before-exit-minibuffer-a ()
    (when (corfu-visible-p)
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  (advice-add #'exit-minibuffer :before #'+corfu--insert-before-exit-minibuffer-a)

  (add-to-list 'corfu-auto-commands #'lispy-colon)

  (defun +corfu-quit-on-evil-insert-state-exit-h ()
    ;; This predicate a workaround for unexpected calls to `corfu-quit' in
    ;; :company-doc-buffer buffers. This was specifically happening when using
    ;; `yasnippet-capf' and `company-yasnippet'.
    (when (eq (current-buffer) (window-buffer (selected-window)))
      (corfu-quit)))

  (add-hook 'evil-insert-state-exit-hook #'+corfu-quit-on-evil-insert-state-exit-h)

  (defun +corfu--reset-or-passthrough (cmd)
    (when (and (modulep! +tng)
               (> corfu--index -1)
               (eq corfu-preview-current 'insert))
      cmd))
  (defun +corfu--backward-toggle-escape-sep (cmd)
    (save-excursion
      (backward-char 1)
      (if (looking-back "\\\\" -1)
          #'corfu-reset
        (lambda ()
          (interactive)
          (save-excursion
            (backward-char 1)
            (insert-char ?\\))))))
  (defun +corfu--insert-separator-or-toggle-escape (cmd)
    (if (char-equal (char-before) corfu-separator)
        (+corfu--backward-toggle-escape-sep cmd)
      cmd))

  (global-corfu-mode))

;;;;; consult

(use-package consult
  :after (vertico)
  :general
  ([remap switch-to-buffer] 'consult-buffer)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-bookmark consult--source-file-register
                     consult--source-recent-file consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<")

  (defun +consult--windows-regexp-compiler-fn (input type ignore-case)
    "Compile the INPUT string to a list of regular expressions.
It is an extended version of `consult--default-regexp-compiler' which handles
Japanese strings to be interpreted as cp932 on Windows machines.
The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
    (setq input (consult--split-escaped input))
    (cons (mapcar (lambda (x) (let ((y (consult--convert-regexp x type)))
                                (encode-coding-string y 'cp932))) input)
          (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
            (apply-partially #'consult--highlight-regexps regexps ignore-case))))

  (when IS-WINDOWS
    (setq consult--regexp-compiler #'+consult--windows-regexp-compiler-fn))

  ;;; Consult search functions

  (defun +consult-fd-cwd ()
    "Consult fd relative to current buffer"
    (interactive)
    (consult-fd default-directory))

  (defun +consult-ripgrep-cwd ()
    "Consult ripgrep relative to current buffer"
    (interactive)
    (consult-ripgrep default-directory))

  (defun +consult-ripgrep-org ()
    "Consult ripgrep in org directory"
    (interactive)
    (consult-ripgrep org-directory))

  (defun +search-buffer ()
    "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
    (interactive)
    (let (start end multiline-p)
      (save-restriction
        (when (region-active-p)
          (setq start (region-beginning)
                end   (region-end)
                multiline-p (/= (line-number-at-pos start)
                                (line-number-at-pos end)))
          (deactivate-mark)
          (when multiline-p
            (narrow-to-region start end)))
        (if (and start end (not multiline-p))
            (consult-line
             (replace-regexp-in-string
              " " "\\\\ "
              (rxt-quote-pcre
               (buffer-substring-no-properties start end))))
          (call-interactively #'consult-line))))))

;;;;; consult

(use-package consult-yasnippet
  :after consult)

;;;;; dired

(use-package dired
  :ensure nil
  :general
  (:states '(normal)
   "-" 'dired-jump)
  :config
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)

  (when IS-WINDOWS
    (setq dired-use-ls-dired t
          dired-listing-switches "-ahl --group-directories-first"))

  (defun +dired-quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (mapc #'kill-buffer (+buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers"))
  (general-def
    :states '(normal)
    :keymaps 'dired-mode-map
    "q" '+dired-quit-all)

  (add-hook 'evil-collection-setup-hook
            (lambda (&rest _) (general-def
                                :states '(normal)
                                :keymaps 'dired-mode-map
                                "q" '+dired-quit-all))))

;;;;; dired-rsync

(use-package dired-rsync
  :general
  (:keymaps 'dired-mode-map
   "C-c C-r" #'dired-rsync))

;;;;; dired-aux

(use-package dired-aux
  :ensure nil
  :defer t
  :config
  (setq dired-do-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

;;;;; dired-x

(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-clean-confirm-killing-deleted-buffers nil
        dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start \"\" ")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)
            ("\\.xls.?\\'" ,cmd)
            ("\\.doc.?\\'" ,cmd)
            ("\\.ppt.?\\'" ,cmd))))

  ;; (add-hook 'evil-collection-setup-hook
  ;;           (lambda (&rest _) (local-leader-def!
  ;;                               :major-modes '(dired-mode)
  ;;                               :keymaps '(dired-mode-map)
  ;;                               "h" 'dired-omit-mode)))

  (local-leader-def!
    :major-modes '(dired-mode)
    :keymaps '(dired-mode-map)
    "h" 'dired-omit-mode))

;;;;; diredfl

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;;;; display-line-numbers

(use-package display-line-numbers
  :ensure nil
  :init
  (setq-default display-line-numbers-width 3
                display-line-numbers-widen t))

;;;;; doom-modeline

(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-icon t
        doom-modeline-modal-icon nil
        doom-modeline-always-show-macro-register t)
  (column-number-mode 1)
  (doom-modeline-mode))

;;;;; dtrt-indent

(use-package dtrt-indent
  :hook ((change-major-mode-after-body read-only-mode) . dtrt-indent-mode)
  :config
  (setq dtrt-indent-run-after-smie t)
  (setq dtrt-indent-max-lines 2000)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

;;;;; eat

(use-package eat
  :commands (eat
             eat-mode)
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-mouse t))

;;;;; ediff

(use-package ediff
  :ensure nil
  :config
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

;;;;; elec-pair

(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode))

;;;;; eldoc

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

;;;;; elisp-mode

(use-package elisp-mode
  :ensure nil
  :config
  (defun +lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  (add-hook 'emacs-lisp-mode-hook
            #'(lambda () (setq-local lisp-indent-function #'+lisp-indent-function))))

;;;;; embark

(use-package embark
  :defer t
  :general
  ("C-." 'embark-dwim)
  ("C-;" 'embark-act)
  ("C-c C-;" 'embark-export)
  ("C-c C-l" 'embark-collect)
  ("C-h B" 'embark-bindings)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (defun +embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(+embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun +embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'+embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;;; embark-consult

(use-package embark-consult
  :after (embark consult)
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; evil

(use-package evil
  :demand t
  :general
  (:states '(normal)
   "] SPC" '+evil-insert-newline-below
   "[ SPC" '+evil-insert-newline-above
   "]b" 'next-buffer
   "[b" 'previous-buffer
   "]f" '+evil-next-file
   "[f" '+evil-previous-file
   "]F" '+evil-next-frame
   "[F" '+evil-previous-frame
   "[o" '+evil-insert-newline-above
   "]o" '+evil-insert-newline-below
   "gp" '+evil-reselect-paste
   "zN" '+widen-indirectly-narrowed-buffer
   [escape] '+evil-force-normal-state)
  (:states '(motion)
   "]u" '+evil:url-encode
   "[u" '+evil:url-decode
   "]y" '+evil:c-string-encode
   "[y" '+evil:c-string-decode
   "]#" '+evil-next-preproc-directive
   "[#" '+evil-previous-preproc-directive
   "]a" 'evil-forward-arg
   "[a" 'evil-backward-arg
   "]c" '+evil-next-comment
   "[c" '+evil-previous-comment
   "]e" 'next-error
   "[e" 'previous-error
   "]h" 'outline-next-visible-heading
   "[h" 'outline-previous-visible-heading
   "]m" '+evil-next-beginning-of-method
   "[m" '+evil-previous-beginning-of-method
   "]M" '+evil-next-end-of-method
   "[M" '+evil-previous-end-of-method)
  (:states '(visual)
   "gp" '+evil-alt-paste)
  (:keymaps '(normal visual)
   "zn" '+evil:narrow-buffer)
  (:keymaps '(evil-ex-completion-map evil-ex-search-keymap)
   "C-a" 'evil-beginning-of-line
   "C-b" 'evil-backward-char
   "C-f" 'evil-forward-char
   :states '(insert)
   "C-j" 'next-complete-history-element
   "C-k" 'previous-complete-history-element)
  (:keymaps 'universal-argument-map
   "u" 'universal-argument-more)
  (:keymaps 'evil-inner-text-objects-map
   "g" '+evil:whole-buffer-txtobj
   "f" '+evil:defun-txtobj
   "u" '+evil:inner-url-txtobj
   "q" '+evil:inner-any-quote
   "l" '+evil:inner-line-txtobj)
  (:keymaps 'evil-outer-text-objects-map
   "g" '+evil:whole-buffer-txtobj
   "f" '+evil:defun-txtobj
   "u" '+evil:outer-url-txtobj
   "q" '+evil:outer-any-quote
   "l" '+evil:outer-line-txtobj)
  :preface
  (setq evil-want-keybinding nil
        evil-want-C-g-bindings t
        evil-want-C-i-jump t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-respect-visual-line-mode nil
        evil-search-module 'evil-search
        evil-undo-system 'undo-tree)
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-symbol-word-search t
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t)

  (defun +evil-kill-minibuffer ()
    (interactive)
    (when (windowp (active-minibuffer-window))
      (evil-ex-search-exit)))

  (add-hook 'mouse-leave-buffer-hook #'+evil-kill-minibuffer)
  (evil-declare-abort-repeat 'outline-cycle)

  ;; Clear highlight
  (defun +evil-force-normal-state ()
    "Delegate to evil-force-normal-state but also clear search highlighting"
    (interactive)
    (evil-force-normal-state)
    (evil-ex-nohighlight))

  ;; Text objects

  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))

  (evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
    "Text object to select the inner url at point.

This excludes the protocol and querystring."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'url)
      (evil-range
       (save-excursion
         (goto-char beg)
         (re-search-forward "://" end t))
       (save-excursion
         (goto-char end)
         (- (if-let (pos (re-search-backward "[?#]" beg t))
                pos
              end)
            (if (evil-visual-state-p)
                1
              0)))
       type)))

  (evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
    "Text object to select the whole url at point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'url)
      (evil-range
       beg (- end (if (evil-visual-state-p) 1 0))
       type)))

  (evil-define-text-object +evil:inner-line-txtobj (count &optional _beg _end type)
    "Text object to select the inner line at point."
    (let ((start (save-excursion
                   (back-to-indentation)
                   (point)))
          (end (save-excursion
                 (goto-char (line-end-position))
                 (skip-syntax-backward " " (line-beginning-position))
                 (point))))
      (evil-range start end)))

  (evil-define-text-object +evil:outer-line-txtobj (count &optional _beg _end type)
    "Text object to select the outer line at point."
    (evil-range (line-beginning-position) (line-end-position)))

  ;;; Unimpaired

  (defun +evil-next-beginning-of-method (count)
    "Jump to the beginning of the COUNT-th method/function after point."
    (interactive "p")
    (beginning-of-defun (- count)))

  (defun +evil-previous-beginning-of-method (count)
    "Jump to the beginning of the COUNT-th method/function before point."
    (interactive "p")
    (beginning-of-defun count))

  (defalias #'+evil-next-end-of-method #'end-of-defun
    "Jump to the end of the COUNT-th method/function after point.")

  (defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
    "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

  (defun +evil-previous-end-of-method (count)
    "Jump to the end of the COUNT-th method/function before point."
    (interactive "p")
    (end-of-defun (- count)))

  (defun +evil-next-preproc-directive (count)
    "Jump to the COUNT-th preprocessor directive after point.

By default, this only recognizes C preproc directives. To change this see
`+evil-preprocessor-regexp'."
    (interactive "p")
    ;; TODO More generalized search, to support directives in other languages?
    (if (re-search-forward +evil-preprocessor-regexp nil t count)
        (goto-char (match-beginning 0))
      (user-error "No preprocessor directives %s point"
                  (if (> count 0) "after" "before"))))

  (defun +evil-previous-preproc-directive (count)
    "Jump to the COUNT-th preprocessor directive before point.

See `+evil-next-preproc-directive' for details."
    (interactive "p")
    (+evil-next-preproc-directive (- count)))

  (defun +evil-next-comment (count)
    "Jump to the beginning of the COUNT-th commented region after point."
    (interactive "p")
    (let ((orig-pt (point)))
      (require 'newcomment)
      (dotimes (_ (abs count))
        (cond ((> count 0)
               (while (and (not (eobp)) (sp-point-in-comment))
                 (forward-line 1))
               (unless (comment-search-forward (point-max) 'noerror)
                 (goto-char orig-pt)
                 (user-error "No comment after point")))
              (t
               (while (and (not (bobp)) (sp-point-in-comment))
                 (forward-line -1))
               (unless (comment-search-backward nil 'noerror)
                 (goto-char orig-pt)
                 (user-error "No comment before point")))))))

  (defun +evil-previous-comment (count)
    "Jump to the beginning of the COUNT-th commented region before point."
    (interactive "p")
    (+evil-next-comment (- count)))


  ;; Insert newline below/above
  (defun +evil-insert-newline-below (count)
    "Insert COUNT blank line(s) below current line. Does not change modes."
    (interactive "p")
    (dotimes (_ count)
      (save-excursion (evil-insert-newline-below))))

  (defun +evil-insert-newline-above (count)
    "Insert COUNT blank line(s) above current line. Does not change modes."
    (interactive "p")
    (dotimes (_ count)
      (save-excursion (evil-insert-newline-above))))

  ;; Next/previous frame
  (defun +evil-next-frame (count)
    "Focus next frame."
    (interactive "p")
    (dotimes (_ (abs count))
      (let ((frame (if (> count 0) (next-frame) (previous-frame))))
        (if (eq frame (selected-frame))
            (user-error "No other frame")
          (select-frame-set-input-focus frame)))))

  (defun +evil-previous-frame (count)
    "Focus previous frame."
    (interactive "p")
    (+evil-next-frame (- count)))

  (defun +evil--next-file (n)
    (unless buffer-file-name
      (user-error "Must be called from a file-visiting buffer"))
    (let* ((directory (file-name-directory buffer-file-name))
           (filename (file-name-nondirectory buffer-file-name))
           (files (cl-remove-if #'file-directory-p (+glob (file-name-directory buffer-file-name) "[!.]*")))
           (index (cl-position filename files :test #'file-equal-p)))
      (when (null index)
        (user-error "Couldn't find this file in current directory"))
      (let ((index (+ index n)))
        (cond ((>= index (length files))
               (user-error "No files after this one"))
              ((< index 0)
               (user-error "No files before this one"))
              ((expand-file-name (nth index files) directory))))))

  ;; Next/previous file
  (defun +evil-next-file (count)
    "Open file following this one, alphabetically, in the same directory."
    (interactive "p")
    (find-file (+evil--next-file count)))

  (defun +evil-previous-file (count)
    "Open file preceding this one, alphabetically, in the same directory."
    (interactive "p")
    (find-file (+evil--next-file (- count))))

  (defun +evil--encode (beg end fn)
    (save-excursion
      (goto-char beg)
      (let* ((end (if (eq evil-this-type 'line) (1- end) end))
             (text (buffer-substring-no-properties beg end)))
        (delete-region beg end)
        (insert (funcall fn text)))))

  (evil-define-operator +evil:url-encode (_count &optional beg end)
    "URL encode"
    (interactive "<c><r>")
    (+evil--encode beg end #'url-encode-url))

  (evil-define-operator +evil:url-decode (_count &optional beg end)
    "URL decode"
    (interactive "<c><r>")
    (+evil--encode beg end #'url-unhex-string))

  (evil-define-operator +evil:c-string-encode (_count &optional beg end)
    "C string encode"
    (interactive "<c><r>")
    (+evil--encode
     beg end
     (lambda (text)
       (replace-regexp-in-string "[\"\\]" (lambda (ch) (concat "\\" ch)) text))))

  (evil-define-operator +evil:c-string-decode (_count &optional beg end)
    "C string decode"
    (interactive "<c><r>")
    (+evil--encode
     beg end
     (lambda (text)
       (replace-regexp-in-string "\\\\[\"\\]" (lambda (str) (substring str 1)) text))))

  (evil-define-operator +evil:narrow-buffer (beg end &optional bang)
    "Narrow the buffer to region between BEG and END.

Widens narrowed buffers first. If BANG, use indirect buffer clones instead."
    :move-point nil
    (interactive "<r><!>")
    (if (not bang)
        (if (buffer-narrowed-p)
            (widen)
          (narrow-to-region beg end))
      (when (buffer-narrowed-p)
        (+widen-indirectly-narrowed-buffer t))
      (+narrow-buffer-indirectly beg end)))

  (defun +evil-reselect-paste ()
    "Return to visual mode and reselect the last pasted region."
    (interactive)
    (cl-destructuring-bind (_ _ _ beg end &optional _)
        evil-last-paste
      (evil-visual-make-selection
       (save-excursion (goto-char beg) (point-marker))
       end)))

  (defun +evil-alt-paste ()
    "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents."
    (interactive)
    (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
      (call-interactively #'evil-paste-after)))

  (evil-mode))

;;;;; evil-anzu

(use-package evil-anzu
  :config
  (global-anzu-mode +1))

;;;;; evil-args

(use-package evil-args
  :general
  (:keymaps 'evil-inner-text-objects-map
   "a" 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
   "a" 'evil-outer-arg))

;;;;; evil-collection

(use-package evil-collection
  :after (evil)
  :config
  ;; (setq evil-collection-key-blacklist '("dired"))
  (evil-collection-init))

;;;;; evil-escape

(use-package evil-escape
  :after (evil)
  :config
  (setq evil-escape-key-sequence "jk")
  (push 'visual evil-escape-excluded-states)
  (push 'normal evil-escape-excluded-states)
  (evil-escape-mode))

;;;;; evil-indent-plus

(use-package evil-indent-plus
  :general
  (:keymaps 'evil-inner-text-objects-map
   "i" 'evil-indent-plus-i-indent
   "j" 'evil-indent-plus-i-indent
   "k" 'evil-indent-plus-i-indent)
  (:keymaps 'evil-outer-text-objects-map
   "i" 'evil-indent-plus-a-indent
   "j" 'evil-indent-plus-a-indent
   "k" 'evil-indent-plus-a-indent))

;;;;; evil-lion

(use-package evil-lion
  :config
  (evil-lion-mode))

;;;;; evil-matchit

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;;;;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  (:keymaps 'evil-inner-text-objects-map
   "c" 'evilnc-inner-comment)
  (:keymaps 'evil-outer-text-objects-map
   "c" 'evilnc-outer-commenter)
  (:states '(normal visual)
   "gc" 'evilnc-comment-operator)
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

;;;;; evil-snipe

(use-package evil-snipe
  :disabled
  :config
  (setq evil-snipe-scope 'visible)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1))

;;;;; evil-surround

(use-package evil-surround
  :config
  (global-evil-surround-mode))

;;;;; evil-textobj-anyblock

(use-package evil-textobj-anyblock
  :general
  (:keymaps 'evil-inner-text-objects-map
   "a" 'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
   "a" 'evil-textobj-anyblock-outer-block)
  :init
  (evil-define-text-object +evil:inner-any-quote (count &optional beg end type)
    "Select the closest inner quote."
    (require 'evil-textobj-anyblock)
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("‘" . "’")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object +evil:outer-any-quote (count &optional beg end type)
    "Select the closest outer quote."
    (require 'evil-textobj-anyblock)
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("‘" . "’")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t))))

;;;;; exato

(use-package exato
  :general
  (:keymaps 'evil-inner-text-objects-map
   "a" 'evil-inner-xml-attr)
  (:keymaps 'evil-outer-text-objects-map
   "a" 'evil-outer-xml-attr))

;;;;; fd-dired

(use-package fd-dired
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

;;;;; files

(use-package files
  :ensure nil
  :config
  (setq make-backup-files nil
        version-control t
        backup-by-copying t
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        auto-save-default t
        auto-save-list-file-prefix (concat +cache-dir "autosave/")
        tramp-auto-save-directory  (concat +cache-dir "tramp-autosave/")
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))

  ;; Fix encoding when using external ls from uutils-coreutils
  (when IS-WINDOWS
    (if-let (ls (executable-find "ls"))
        (setq insert-directory-program ls))
    (advice-add #'insert-directory
                :around #'+coding-system-for-rw-undecided-utf-8))

  (defun +glob (&rest segments)
    "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
    (declare (side-effect-free t))
    (let* (case-fold-search
           file-name-handler-alist
           (path (apply #'file-name-concat segments)))
      (if (string-suffix-p "/" path)
          (cl-delete-if-not #'file-directory-p (file-expand-wildcards (substring path 0 -1)))
        (file-expand-wildcards path))))

  (defun +enable-require-final-newline ()
    "Enable `require-final-newline'"
    (setq-local require-final-newline t))

  (defun +disable-require-final-newline ()
    "Disable `require-final-newline'"
    (setq-local require-final-newline nil))

  (defun +files-update-refs (&rest files)
    "Ensure FILES are updated in `recentf', `magit' and `save-place'."
    (let (toplevels)
      (dolist (file files)
        (when (featurep 'vc)
          (vc-file-clearprops file)
          (when-let (buffer (get-file-buffer file))
            (with-current-buffer buffer
              (vc-refresh-state))))
        (when (featurep 'magit)
          (when-let (default-directory (magit-toplevel (file-name-directory file)))
            (cl-pushnew default-directory toplevels)))
        (unless (file-readable-p file)
          (when (bound-and-true-p recentf-mode)
            (recentf-remove-if-non-kept file))
          (when (and (bound-and-true-p projectile-mode)
                     (+project-p)
                     (projectile-file-cached-p file (+project-root)))
            (projectile-purge-file-from-cache file))))
      (dolist (default-directory toplevels)
        (magit-refresh))
      (when (bound-and-true-p save-place-mode)
        (save-place-forget-unreadable-files))))

  (defun +delete-this-file (&optional path force-p)
    "Delete PATH, kill its buffers and expunge it from vc/magit cache.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
    (interactive
     (list (buffer-file-name (buffer-base-buffer))
           current-prefix-arg))
    (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
           (short-path (and path (abbreviate-file-name path))))
      (unless path
        (user-error "Buffer is not visiting any file"))
      (unless (file-exists-p path)
        (error "File doesn't exist: %s" path))
      (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
        (user-error "Aborted"))
      (let ((buf (current-buffer)))
        (unwind-protect
            (progn (delete-file path t) t)
          (if (file-exists-p path)
              (error "Failed to delete %S" short-path)
            ;; Ensures that windows displaying this buffer will be switched to
            ;; real buffers (`+real-buffer-p')
            (+kill-this-buffer-in-all-windows buf t)
            (+files-update-refs path)
            (message "Deleted %S" short-path))))))

  (defun +copy-this-file (new-path &optional force-p)
    "Copy current buffer's file to NEW-PATH then open NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
    (interactive
     (list (read-file-name "Copy file to: ")
           current-prefix-arg))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (make-directory (file-name-directory new-path) 't)
      (copy-file old-path new-path (or force-p 1))
      (find-file new-path)
      (+files-update-refs old-path new-path)
      (message "File copied to %S" (abbreviate-file-name new-path))))

  (defun +move-this-file (new-path &optional force-p)
    "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
    (interactive
     (list (read-file-name "Move file to: ")
           current-prefix-arg))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (concat new-path (file-name-nondirectory old-path))))
      (make-directory (file-name-directory new-path) 't)
      (rename-file old-path new-path (or force-p 1))
      (set-visited-file-name new-path t t)
      (+files-update-refs old-path new-path)
      (message "File moved to %S" (abbreviate-file-name new-path)))))

;;;;; flycheck

(use-package flycheck
  :config
  (when IS-WINDOWS
    (defun +windows--flycheck-sanitize-output-a (fn output &rest args)
      "Sanitized output of flycheck"
      (let ((sanitized-output (replace-regexp-in-string "\r" "" output)))
        (apply fn sanitized-output args)))

    (advice-add #'flycheck-parse-output :around #'+windows--flycheck-sanitize-output-a)))

;;;;; forge

(use-package forge
  :after magit
  :general
  (:keymaps 'forge-topic-list-mode-map
   "q" 'kill-current-buffer)
  (:keymaps 'magit-mode-map
   [remap magit-browse-thing] 'forge-browse-dwim)
  (:keymaps 'magit-remote-section-map
   [remap magit-browse-thing] 'forge-browse-remote)
  (:keymaps 'magit-branch-section-map
   [remap magit-browse-thing] 'forge-browse-branch))

;;;;; format-all

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode))

;;;;; help

(use-package help-mode
  :ensure nil
  :general
  (:keymaps '(help-mode-map)
   :states '(normal)
   "o" 'link-hint-open-link)

  (:keymaps '(Info-mode-map)
   :states '(normal)
   "o" 'link-hint-open-link)

  (:keymaps '(apropos-mode-map)
   :states '(normal)
   "o" 'link-hint-open-link
   "TAB" 'forward-button
   [tab] 'forward-button
   [backtab] 'backward-button)

  (:keymaps '(view-mode-map)
   [escape] 'View-quit-all)

  (:keymaps '(Man-mode-map)
   "q" 'kill-current-buffer)

  (:states '(normal)
   "C-S-f" 'toggle-frame-fullscreen))

;;;;; helpful

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))

;;;;; indent-bars

(use-package indent-bars
  :ensure
  (indent-bars :host github
               :repo "jdtsmith/indent-bars")
  :hook ((python-mode yaml-mode) . indent-bars-mode)
  :config
  (when IS-WINDOWS
    (setq indent-bars-prefer-character t)))

;;;;; image-dired

(use-package image-dired
  :ensure nil
  :config
  (setq image-dired-dir (concat +cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150))

;;;;; ls-lisp

(use-package ls-lisp
  :ensure nil
  :config
  (when IS-WINDOWS
    (setq ls-lisp-use-insert-directory-program t
          ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")
          ls-lisp-dirs-first t
          ls-lisp-ignore-case t
          ls-lisp-UCA-like-collation nil)))

;;;;; transient

(use-package transient
  :ensure t)

;;;;; magit

(use-package magit
  :defer t
  :after (general)
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  (define-key transient-map [escape] #'transient-quit-one))

;;;;; marginalia

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;;; migemo

(use-package migemo
  :after (consult orderless)
  :init
  (setq search-default-regexp-mode nil
        ;; migemo-options '("-q" "--emacs" "-i" "\a")
        migemo-options '("--quiet" "--nonewline" "--emacs")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix
        migemo-accept-process-output-timeout-msec 80)
  (if IS-WINDOWS
      (setq migemo-dictionary (expand-file-name "~/.local/share/migemo/utf-8/migemo-dict"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  :config
  (setq consult-async-min-input 1)
  (defun +orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))

  (migemo-init)

  (with-eval-after-load 'orderless
    (orderless-define-completion-style +orderless-default
      (orderless-matching-styles '(orderless-literal
                                   orderless-regexp)))

    (orderless-define-completion-style +orderless-migemo
      (orderless-matching-styles '(orderless-literal
                                   orderless-regexp
                                   +orderless-migemo)))

    (orderless-define-completion-style +orderless-initialism
      (orderless-matching-styles '(orderless-initialism
                                   orderless-literal)))

    (add-to-list 'completion-styles '+orderless-migemo t)

    (setq completion-category-overrides
          '((command (styles +orderless-initialism))
            (file (styles orderless partial-completion
                          +orderless-migemo))
            (buffer (styles +orderless-migemo))
            (symbol (styles +orderless-default))
            (consult-location (styles +orderless-migemo))
            (consult-multi (styles +orderless-migemo))
            (org-roam-node (styles +orderless-migemo))
            (unicode-name (styles +orderless-migemo))
            (variable (styles +orderless-default))
            (project-file (styles +orderless-migemo))))))

;;;;; avy-migemo

(use-package avy-migemo
  :ensure
  (avy-migemo :host github
              :repo "tam17aki/avy-migemo")
  :after (avy)
  :config (avy-migemo-mode 1))

;;;;; minibuffer

(use-package minibuffer
  :ensure nil
  :general

  ;;; Minibuffer keybindings to be largely the same as readline

  (:keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map
              read-expression-map)
   [escape] 'abort-recursive-edit
   "C-a" 'move-beginning-of-line
   "C-r" 'evil-paste-from-register
   "C-u" 'evil-delete-back-to-indentation
   "C-w" '+delete-backward-word
   "C-v" 'yank
   "C-z" (cmd! (ignore-errors (call-interactively #'undo)))
   "C-S-j" 'scroll-up-command
   "C-S-k" 'scroll-down-command)

  ;;; Minibuffer keybindings to choose candidate using ctrl j and k

  (:keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   "C-j" 'next-line
   "C-k" 'previous-line)

  (:keymaps '(read-expression-map)
   "C-j" 'next-line-or-history-element
   "C-k" 'previous-line-or-history-element)

  :init
  (setq completion-cycle-threshold 3)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)

  (defun +delete-backward-word (arg)
    "Like `backward-kill-word', but doesn't affect the kill-ring."
    (interactive "p")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (ignore-errors (backward-kill-word arg)))))

;;;;; orderless

(use-package orderless
  :after (vertico)
  :config
  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ;; Migemo matching
     ((string-prefix-p "@" pattern) `(+orderless-migemo . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(+orderless-migemo . ,(substring pattern 0 -1)))))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")

  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;;;; org

(use-package org
  :ensure (:autoloads "org-loaddefs.el")
  ;; :ensure (:host github
  ;;          :autoloads "org-loaddefs.el"
  ;;          :repo "emacs-straight/org-mode"
  ;;          :tag "release_9.6.12"
  ;;          :depth 1)
  :defer t
  :general
  (:keymaps 'org-mode-map
   [tab] 'org-cycle
   "C-c C-i" 'org-toggle-inline-images
   "S-RET" '+org-shift-return
   "C-RET" '+org-insert-item-below
   "C-S-RET" '+org-insert-item-above
   "C-M-RET" 'org-insert-subheading
   [C-return] '+org-insert-item-below
   [C-S-return] '+org-insert-item-above
   [C-M-return] 'org-insert-subheading)
  (:keymaps 'org-mode-map
   :states '(normal insert)
   "M-h" 'org-metaleft
   "M-j" 'org-metadown
   "M-k" 'org-metaup
   "M-l" 'org-metaright
   "M-H" 'org-shiftmetaleft
   "M-J" 'org-shiftmetadown
   "M-K" 'org-shiftmetaup
   "M-L" 'org-shiftmetaright)
  (:states '(motion)
   :keymaps 'org-mode-map
   "RET" 'org-open-at-point)
  (local-leader-def!
    :major-modes '(org-mode)
    :keymaps '(org-mode-map)
    "#" 'org-update-statistics-cookies
    "'" 'org-edit-special
    "*" 'org-ctrl-c-star
    "+" 'org-ctrl-c-minus
    "," 'org-switchb
    "." 'consult-org-heading
    "/" 'consult-org-agenda
    "@" 'org-cite-insert
    "e" 'org-export-dispatch
    "f" 'org-footnote-action
    "h" 'org-toggle-heading
    "i" 'org-toggle-item
    "I" 'org-id-get-create
    "k" 'org-babel-remove-result
    "K" '+org-remove-result-blocks
    "n" 'org-store-link
    "o" 'org-set-property
    "q" 'org-set-tags-command
    "t" 'org-todo
    "T" 'org-todo-list
    "x" 'org-toggle-checkbox
    "a" '(:ignore t :wk "attachments")
    "aa" 'org-attach
    "ad" 'org-attach-delete-one
    "aD" 'org-attach-delete-all
    "af" '+org-find-file-in-attachments
    "al" '+org-attach-file-and-insert-link
    "an" 'org-attach-new
    "ao" 'org-attach-open
    "aO" 'org-attach-open-in-emacs
    "ar" 'org-attach-reveal
    "aR" 'org-attach-reveal-in-emacs
    "au" 'org-attach-url
    "as" 'org-attach-set-directory
    "aS" 'org-attach-sync
    "ac" 'org-download-screenshot
    "ap" 'org-download-clipboard
    "aP" 'org-download-yank
    "b" '(:ignore t :wk "tables")
    "b-" 'org-table-insert-hline
    "ba" 'org-table-align
    "bb" 'org-table-blank-field
    "bc" 'org-table-create-or-convert-from-region
    "be" 'org-table-edit-field
    "bf" 'org-table-edit-formulas
    "bh" 'org-table-field-info
    "bs" 'org-table-sort-lines
    "br" 'org-table-recalculate
    "bR" 'org-table-recalculate-buffer-tables
    "d" '(:ignore t :wk "delete")
    "dc" 'org-table-delete-column
    "dr" 'org-table-kill-row
    "i" '(:ignore t :wk "insert")
    "ic" 'org-table-insert-column
    "ih" 'org-table-insert-hline
    "ir" 'org-table-insert-row
    "iH" 'org-table-hline-and-move
    "o" '(:ignore t :wk "toggle")
    "of" 'org-table-toggle-formula-debugger
    "oo" 'org-table-toggle-coordinate-overlays
    "c" '(:ignore t :wk "clock")
    "cd" 'org-clock-mark-default-task
    "cE" 'org-set-effort
    "cG" (cmd! org-clock-goto 'select)
    "cl" '+org-toggle-last-clock
    "cr" 'org-resolve-clocks
    "ct" 'org-evaluate-time-range
    "d" '(:ignore t :wk "date/deadline")
    "dd" 'org-deadline
    "ds" 'org-schedule
    "dt" 'org-time-stamp
    "dT" 'org-time-stamp-inactive
    "g" '(:ignore t :wk "goto")
    "gg" 'consult-org-headings
    "gG" 'consult-org-agenda
    "gi" 'org-id-goto
    "gr" 'org-refile-goto-last-stored
    "gx" 'org-capture-goto-last-stored
    "l" '(:ignore t :wk "links")
    "lc" 'org-cliplink
    "ld" '+org-remove-link
    "li" 'org-id-store-link
    "ll" 'org-insert-link
    "lL" 'org-insert-all-links
    "ls" 'org-store-link
    "lS" 'org-insert-last-stored-link
    "lt" 'org-toggle-link-display
    "P" '(:ignore t :wk "publish")
    "Pa" 'org-publish-all
    "Pf" 'org-publish-current-file
    "Pp" 'org-publish
    "PP" 'org-publish-current-project
    "Ps" 'org-publish-sitemap
    "r" '(:ignore t :wk "refile")
    "r." '+org-refile-to-current-file
    "rc" '+org-refile-to-running-clock
    "rl" '+org-refile-to-last-location
    "rf" '+org-refile-to-file
    "ro" '+org-refile-to-other-window
    "rO" '+org-refile-to-other-buffer
    "rv" '+org-refile-to-visible
    "rr" 'org-refile
    "rR" 'org-refile-reverse
    "s" '(:ignore t :wk "tree/subtree")
    "sa" 'org-toggle-archive-tag
    "sb" 'org-tree-to-indirect-buffer
    "sc" 'org-clone-subtree-with-time-shift
    "sd" 'org-cut-subtree
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sn" 'org-narrow-to-subtree
    "sr" 'org-refile
    "ss" 'org-sparse-tree
    "sA" 'org-archive-subtree
    "sN" 'widen
    "sS" 'org-sort
    "p" '(:ignore t :wk "priority")
    "pd" 'org-priority-down
    "pp" 'org-priority
    "pu" 'org-priority-up)
  (:states '(normal)
   :keymaps 'org-mode-map
   "gk" (cmd! (if (org-at-heading-p)
                  (org-backward-element)
                (evil-previous-visual-line)))
   "gj" (cmd! (if (org-at-heading-p)
                  (org-forward-element)
                (evil-next-visual-line))))
  :config
  (setq org-archive-location (concat org-directory ".archive/%s::")
        org-auto-align-tags nil
        org-directory "~/org/"
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-image-actual-width nil
        org-imenu-depth 6
        org-indirect-buffer-display 'current-window
        org-tags-column 0
        org-fold-catch-invisible-edits 'show-and-error
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-startup-indented nil
        org-tags-column 0
        org-use-sub-superscripts '{}
        org-startup-folded nil
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        )
  (plist-put org-format-latex-options :scale 1.5) ; larger previews

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setq org-todo-keywords '((sequence
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
                             "NO(n!)"))
        org-todo-keyword-faces '(("[-]"  . +org-todo-active)
                                 ("STRT" . +org-todo-active)
                                 ("[?]"  . +org-todo-onhold)
                                 ("WAIT" . +org-todo-onhold)
                                 ("HOLD" . +org-todo-onhold)
                                 ("PROJ" . +org-todo-project)
                                 ("NO"   . +org-todo-cancel)
                                 ("KILL" . +org-todo-cancel)))

  (setq org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-src-window-setup 'other-window
        org-babel-lisp-eval-fn #'sly-eval)

  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
           "* TODO %?\n%i\n%a"
           :prepend t)
          ("d" "Deadline" entry (file+headline "todo.org" "Schedule")
           "* TODO %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "Schedule" entry (file+headline "todo.org" "Schedule")
           "* TODO %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)))

  (defun +org-shift-return (&optional arg)
    "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
    (interactive "p")
    (if (org-at-table-p)
        (org-table-copy-down arg)
      (org-return nil arg)))

  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         (let ((orig-point (point)))
           ;; Position determines where org-insert-todo-heading and `org-insert-item'
           ;; insert the new list item.
           (if (eq direction 'above)
               (org-beginning-of-item)
             (end-of-line))
           (let* ((ctx-item? (eq 'item (org-element-type context)))
                  (ctx-cb (org-element-property :contents-begin context))
                  ;; Hack to handle edge case where the point is at the
                  ;; beginning of the first item
                  (beginning-of-list? (and (not ctx-item?)
                                           (= ctx-cb orig-point)))
                  (item-context (if beginning-of-list?
                                    (org-element-context)
                                  context))
                  ;; Horrible hack to handle edge case where the
                  ;; line of the bullet is empty
                  (ictx-cb (org-element-property :contents-begin item-context))
                  (empty? (and (eq direction 'below)
                               ;; in case contents-begin is nil, or contents-begin
                               ;; equals the position end of the line, the item is
                               ;; empty
                               (or (not ictx-cb)
                                   (= ictx-cb
                                      (1+ (point))))))
                  (pre-insert-point (point)))
             ;; Insert dummy content, so that `org-insert-item'
             ;; inserts content below this item
             (when empty?
               (insert " "))
             (org-insert-item (org-element-property :checkbox context))
             ;; Remove dummy content
             (when empty?
               (delete-region pre-insert-point (1+ pre-insert-point))))))
        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (run-hooks 'org-insert-heading-hook)
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  ;; I use these instead of `org-insert-item' or `org-insert-heading' because they
  ;; impose bizarre whitespace rules depending on cursor location and many
  ;; settings. These commands have a much simpler responsibility.
  (defun +org-insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  (defun +org-insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))

  (add-to-list 'org-file-apps '("\\.xls.?\\'" . default))
  (add-to-list 'org-file-apps '("\\.doc.?\\'" . default))

  (setq org-startup-folded 'show2levels
        org-log-into-drawer t)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)

  ;; Save target buffer after archiving a node.
  (setq org-archive-subtree-save-file-p t)

  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  (defun +org-dwim-at-point (&optional arg)
    "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
    (interactive "P")
    (if (button-at (point))
        (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
                type (org-element-type context)))
        (pcase type
          ((or `citation `citation-reference)
           (org-cite-follow context arg))

          (`headline
           (cond ((memq (bound-and-true-p org-goto-map)
                        (current-active-maps))
                  (org-goto-ret))
                 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
                         (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
                         (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
                 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
                 (goto-char (org-element-property :contents-begin context))
                 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage)))
             (if (or (equal (org-element-property :type lineage) "img")
                     (and path (image-type-from-file-name path)))
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))
               (org-open-at-point arg))))

          (`paragraph
           (+org--toggle-inline-images-in-subtree))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
             (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil  t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context))))))))

  (defun +org-shift-return (&optional arg)
    "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
    (interactive "p")
    (if (org-at-table-p)
        (org-table-copy-down arg)
      (org-return nil arg)))

  (defun +org-remove-link ()
    "Unlink the text at point."
    (interactive)
    (unless (org-in-regexp org-link-bracket-re 1)
      (user-error "No link at point"))
    (save-excursion
      (let ((label (if (match-end 2)
                       (match-string-no-properties 2)
                     (org-link-unescape (match-string-no-properties 1)))))
        (delete-region (match-beginning 0) (match-end 0))
        (insert label))))

  (defun +org-remove-result-blocks (remove-all)
    "Remove all result blocks located after current point."
    (interactive "P")
    (let ((pos (point)))
      (org-babel-map-src-blocks nil
        (if (or remove-all (< pos end-block))
            (org-babel-remove-result)))))

  (defun +org-find-file-in-attachments ()
    "Open a file from `org-attach-id-dir'."
    (interactive)
    (+project-browse org-attach-id-dir))

  (defun +org-attach-file-and-insert-link (path)
    "Downloads the file at PATH and insert an org link at point.
PATH (a string) can be an url, a local file path, or a base64 encoded datauri."
    (interactive "sUri/file: ")
    (unless (eq major-mode 'org-mode)
      (user-error "Not in an org buffer"))
    (require 'org-download)
    (condition-case-unless-debug e
        (let ((raw-uri (url-unhex-string path)))
          (cond ((string-match-p "^data:image/png;base64," path)
                 (org-download-dnd-base64 path nil))
                ((image-type-from-file-name raw-uri)
                 (org-download-image raw-uri))
                ((let ((new-path (expand-file-name (org-download--fullname raw-uri))))
                   ;; Download the file
                   (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") path)
                       (url-copy-file raw-uri new-path)
                     (copy-file path new-path))
                   ;; insert the link
                   (org-download-insert-link raw-uri new-path)))))
      (error
       (user-error "Failed to attach file: %s" (error-message-string e)))))

  (defun +org-refile-to-current-file (arg &optional file)
    "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-targets `((,file :maxlevel . 10)))
          (org-refile-use-outline-path t)
          (org-refile-keep arg)
          current-prefix-arg)
      (call-interactively #'org-refile)))

  (defun +org-refile-to-file (arg file)
    "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
    (interactive
     (list current-prefix-arg
           (read-file-name "Select file to refile to: "
                           default-directory
                           (buffer-file-name (buffer-base-buffer))
                           t nil
                           (lambda (f) (string-match-p "\\.org$" f)))))
    (+org-refile-to-current-file arg file))

  (defun +org-refile-to-other-window (arg)
    "Refile current heading to an org buffer visible in another window.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-keep arg)
          org-refile-targets
          current-prefix-arg)
      (dolist (win (delq (selected-window) (window-list)))
        (with-selected-window win
          (let ((file (buffer-file-name (buffer-base-buffer))))
            (and (eq major-mode 'org-mode)
                 file
                 (cl-pushnew (cons file (cons :maxlevel 10))
                             org-refile-targets)))))
      (call-interactively #'org-refile)))

  (defun +org-refile-to-other-buffer (arg)
    "Refile current heading to another, living org buffer.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-keep arg)
          org-refile-targets
          current-prefix-arg)
      (dolist (buf (delq (current-buffer) (+buffers-in-mode 'org-mode)))
        (when-let (file (buffer-file-name (buffer-base-buffer buf)))
          (cl-pushnew (cons file (cons :maxlevel 10))
                      org-refile-targets)))
      (call-interactively #'org-refile)))

  (defun +org-refile-to-running-clock (arg)
    "Refile current heading to the currently clocked in task.
If prefix ARG, copy instead of move."
    (interactive "P")
    (unless (bound-and-true-p org-clock-current-task)
      (user-error "No active clock to refile to"))
    (let ((org-refile-keep arg))
      (org-refile 2)))

  (defun +org-refile-to-last-location (arg)
    "Refile current heading to the last node you refiled to.
If prefix ARG, copy instead of move."
    (interactive "P")
    (or (assoc (plist-get org-bookmark-names-plist :last-refile)
               bookmark-alist)
        (user-error "No saved location to refile to"))
    (let ((org-refile-keep arg)
          (completing-read-function
           (lambda (_p _coll _pred _rm _ii _h default &rest _)
             default)))
      (org-refile)))

  (defvar org-after-refile-insert-hook)
  ;; Inspired by org-teleport and alphapapa/alpha-org

  (defun +org-refile-to-visible ()
    "Refile current heading as first child of visible heading selected with Avy."
    (interactive)
    (when-let (marker (+org-headline-avy))
      (let* ((buffer (marker-buffer marker))
             (filename
              (buffer-file-name (or (buffer-base-buffer buffer)
                                    buffer)))
             (heading
              (org-with-point-at marker
                (org-get-heading 'no-tags 'no-todo)))
             ;; Won't work with target buffers whose filename is nil
             (rfloc (list heading filename nil marker))
             (org-after-refile-insert-hook (cons #'org-reveal org-after-refile-insert-hook)))
        (org-refile nil nil rfloc))))

  (defun +org-open-fold ()
    "Open the current fold (not but its children)."
    (interactive)
    (+org-toggle-fold t))

  (defalias #'+org-close-fold #'outline-hide-subtree)

  (defun +org-close-all-folds (&optional level)
    "Close all folds in the buffer (or below LEVEL)."
    (interactive "p")
    (outline-hide-sublevels (or level 1)))

  (defun +org-open-all-folds (&optional level)
    "Open all folds in the buffer (or up to LEVEL)."
    (interactive "P")
    (if (integerp level)
        (outline-hide-sublevels level)
      (outline-show-all)))

  (defun +org--get-foldlevel ()
    (let ((max 1))
      (save-restriction
        (narrow-to-region (window-start) (window-end))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (org-next-visible-heading 1)
            (when (memq (get-char-property (line-end-position)
                                           'invisible)
                        '(outline org-fold-outline))
              (let ((level (org-outline-level)))
                (when (> level max)
                  (setq max level))))))
        max)))

  (defun +org-show-next-fold-level (&optional count)
    "Decrease the fold-level of the visible area of the buffer. This unfolds
another level of headings on each invocation."
    (interactive "p")
    (let ((new-level (+ (+org--get-foldlevel) (or count 1))))
      (outline-hide-sublevels new-level)
      (message "Folded to level %s" new-level)))

  (defun +org-hide-next-fold-level (&optional count)
    "Increase the global fold-level of the visible area of the buffer. This folds
another level of headings on each invocation."
    (interactive "p")
    (let ((new-level (max 1 (- (+org--get-foldlevel) (or count 1)))))
      (outline-hide-sublevels new-level)
      (message "Folded to level %s" new-level)))

  (defun +org-toggle-last-clock (arg)
    "Toggles last clocked item.

Clock out if an active clock is running (or cancel it if prefix ARG is non-nil).

If no clock is active, then clock into the last item. See `org-clock-in-last' to
see how ARG affects this command."
    (interactive "P")
    (require 'org-clock)
    (cond ((org-clocking-p)
           (if arg
               (org-clock-cancel)
             (org-clock-out)))
          ((and (null org-clock-history)
                (or (org-on-heading-p)
                    (org-at-item-p))
                (y-or-n-p "No active clock. Clock in on current item?"))
           (org-clock-in))
          ((org-clock-in-last arg)))))

;;;;; org-agenda

(use-package org-agenda
  :ensure nil
  :after (general evil)
  :general
  (with-eval-after-load 'org-agenda
    (evil-make-intercept-map org-agenda-mode-map)
    (general-def
      :keymaps '(org-agenda-mode-map)
      "d" '(:ignore t :wk "date/deadline")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule
      "c" '(:ignore t :wk "clock")
      "cc" 'org-agenda-clock-cancel
      "cg" 'org-agenda-clock-goto
      "ci" 'org-agenda-clock-in
      "co" 'org-agenda-clock-out
      "cr" 'org-agenda-clockreport-mode
      "cs" 'org-agenda-show-clocking-issues
      "p" '(:ignore t :wk "priority")
      "pd" 'org-agenda-priority-down
      "pp" 'org-agenda-priority
      "pu" 'org-agenda-priority-up
      "A" '+org-agenda-archives
      "C" 'org-agenda-clockreport-mode
      "D" 'org-agenda-goto-date
      "E" 'epoch-agenda-todo
      "H" 'org-habit-toggle-habits
      "J" 'org-agenda-next-item
      "K" 'org-agenda-previous-item
      "R" 'org-agenda-refile
      "S" 'org-agenda-schedule
      "RET" 'org-agenda-recenter
      "a" '+org-capture-again
      "c" 'org-agenda-capture
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "m" 'org-agenda-month-view
      "t" 'org-agenda-set-tags
      "T" 'org-agenda-todo
      "u" 'org-agenda-undo))
  :config
  (setq org-agenda-skip-unavailable-files t
        org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-files (list org-directory)
        org-agenda-window-setup 'current-window
        org-agenda-deadline-faces '((1.001 . error)
                                    (1.0 . org-warning)
                                    (0.5 . org-upcoming-deadline)
                                    (0.0 . org-upcoming-distant-deadline)))
  (evil-set-initial-state 'org-agenda-mode 'motion))

;;;;; org-modern

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :config
  (setq
   ;; Edit settings
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " [...] "

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"))

;;;;; org-modern-indent

(use-package org-modern-indent
  :disabled
  :ensure (org-modern-indent
           :host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

;;;;; org-roam

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :after (org)
  :general
  (local-leader-def!
    :major-modes '(org-mode)
    :keymaps 'org-mode-map
    "D" 'org-roam-demote-entire-buffer
    "D" 'org-roam-demote-entire-buffer
    "f" 'org-roam-node-find
    "F" 'org-roam-ref-find
    "g" 'org-roam-graph
    "i" 'org-roam-node-insert
    "I" 'org-id-get-create
    "m" 'org-roam-buffer-toggle
    "M" 'org-roam-buffer-display-dedicated
    "n" 'org-roam-capture
    "r" 'org-roam-refile
    "R" 'org-roam-link-replace-all
    "cc" 'org-clock-cancel
    "ce" 'org-clock-modify-effort-estimate
    "cg" 'org-clock-goto
    "ci" 'org-clock-in
    "cI" 'org-clock-in-last
    "co" 'org-clock-out
    "cR" 'org-clock-report
    "c=" 'org-clock-timestamps-up
    "c-" 'org-clock-timestamps-down
    "gc" 'org-clock-goto
    "gC" (cmd! org-clock-goto 'select))
  :config
  (setq org-roam-directory org-directory
        org-roam-db-location (concat org-directory ".org-roam.db")
        org-roam-dailies-directory "journal/"
        org-roam-completion-everywhere t
        org-roam-list-files-commands '(fd fdfind rg find))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (advice-add #'org-roam-link-follow-link :filter-args #'org-roam-link-follow-link-with-description-a)
  (advice-add #'org-roam-link-replace-at-point :override #'org-roam-link-replace-at-point-a))

;;;;; outli

(use-package outli
  :ensure
  (outli :host github
         :repo "jdtsmith/outli")
  :hook
  ((prog-mode text-mode) . outli-mode))

;; ;;;;; pcre2el

;; (use-package pcre2el)

;;;;; popper

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;;;;; projectile

(use-package projectile
  :after (general)
  :config
  (setq projectile-files-cache-expire 14400
        projectile-project-name-function #'+projectile-svn-aware-project-name
        projectile-project-root-functions '(projectile-root-local
                                            projectile-root-bottom-up
                                            projectile-root-top-down
                                            projectile-root-top-down-recurring
                                            +projectile-root-parent))
  (push ".vs" projectile-project-root-files-bottom-up)
  (push "*node_modules" projectile-globally-ignored-directories)
  (push "bin" projectile-globally-ignored-directories)
  (push "obj" projectile-globally-ignored-directories)
  (push "TAGS" projectile-globally-ignored-directories)

  (defvar +project-directory-parent-directories nil
    "List of directories to consider as project parent")

  (defun +project-root (&optional dir)
    "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
    (let ((projectile-project-root
           (unless dir (bound-and-true-p projectile-project-root)))
          projectile-require-project-root)
      (projectile-project-root dir)))

  (defun +project-p (&optional dir)
    "Return t if DIR (defaults to `default-directory') is a valid project."
    (and (+project-root dir)
         t))

  (defun +projectile-root-parent (dir)
    "Identify a project root in DIR by checking if it is under PATH.
Return the first (topmost) matched directory or nil if not found."
    (let ((parent-dir (directory-file-name (file-name-directory (directory-file-name dir)))))
      (when (member parent-dir +project-directory-parent-directories)
        dir)))

  (defun +projectile-svn-aware-project-name (project-root)
    "Windows specific function used to create project name to be display based on the value of PROJECT-ROOT"
    (let ((project-name (projectile-default-project-name project-root))
          (ignore-project-directory-names '("trunk")))
      (while (member project-name ignore-project-directory-names)
        (setq project-root (file-name-directory (directory-file-name project-root)))
        (setq project-name (downcase (file-name-nondirectory (directory-file-name project-root)))))
      (concat (upcase (substring project-name 0 1)) (substring project-name 1))))
  (projectile-mode)

  (when IS-WINDOWS
    (advice-add #'projectile-files-via-ext-command
                :around #'+coding-system-for-rw-utf-8-cp932)))

;;;;; ps-print

(use-package ps-print
  :ensure nil
  :commands (ps-print-buffer)
  :config
  (setq ps-printer-name t
        ps-print-header nil ;; hide header
        ps-paper-type 'a4)
  (when IS-WINDOWS
    (setq ps-lpr-command (executable-find "gswin64c")
          ps-lpr-switches '("-q" "-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI"))))

;;;;; ps-mule

(use-package ps-mule
  :ensure nil
  :after (ps-print)
  :config
  (setq ps-multibyte-buffer 'non-latin-printer)
  (when IS-WINDOWS
    (setq ps-multibyte-buffer 'bdf-font-except-latin
          bdf-directory-list
          (mapcar (lambda (d)
                    (concat (expand-file-name "~/.intlfonts/") d))
                  '("Japanese" "Japanese.X" "Japanese.BIG"
                    "Misc" "TrueType" "Type1")))))

;;;;; queue

(use-package queue
  :ensure (queue
           :host github :repo "emacs-straight/queue" :branch "master"))

;;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;;;; recentf

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :config
  (setq recentf-max-menu-items 1000
        recentf-max-saved-items 1000
        recentf-auto-cleanup nil
        recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;;;;; rg
(use-package rg
  :commands (rg rg-literal rg-dwim)
  :config
  (add-to-list 'rg-custom-type-aliases '("tsv" . "*.tsv"))
  (add-to-list 'rg-custom-type-aliases '("dat" . "*.dat")))

;;;;; savehist

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode))

;;;;; server

(use-package server
  :ensure nil
  :when (display-graphic-p)
  :defer 1
  :config
  (setq server-client-instructions nil)
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))

;;;;; simple

(use-package simple
  :ensure nil
  :config
  (defun +region-active-p ()
    "Return non-nil if selection is active.
Detects evil visual mode as well."
    (declare (side-effect-free t))
    (or (use-region-p)
        (and (bound-and-true-p evil-local-mode)
             (evil-visual-state-p))))

  (defun +region-beginning ()
    "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
    (declare (side-effect-free t))
    (or (and (bound-and-true-p evil-local-mode)
             (evil-visual-state-p)
             (markerp evil-visual-beginning)
             (marker-position evil-visual-beginning))
        (region-beginning)))

  (defun +region-end ()
    "Return end position of selection.
Uses `evil-visual-end' if available."
    (declare (side-effect-free t))
    (if (and (bound-and-true-p evil-local-mode)
             (evil-visual-state-p))
        evil-visual-end
      (region-end))))

;;;;; sql

(use-package sql
  :ensure nil
  :config
  (setq sql-ms-program "sqlcmd"
        sql-ms-options nil
        sql-ms-login-params '(user password server database)))

;;;;; sql-indent

(use-package sql-indent
  :disabled
  :hook (sql-mode . sqlind-minor-mode)
  :hook (sql-interactive-mode . sqlind-minor-mode))

;;;;; sql-up

(use-package sqlup-mode
  :hook (sql-mode . sqlup-mode)
  :hook (sql-interactive-mode . sqlup-mode)
  :config
  (setq sqlup-blacklist '("data")))

;;;;; tempel

(use-package tempel
  :disabled
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;;;; tempel-collection

(use-package tempel-collection)

;;;;; text-mode

(use-package text-mode
  :ensure nil
  :init
  (when IS-WINDOWS
    (add-hook 'text-mode-hook #'+disable-require-final-newline)))

;;;;; thingatpt

(use-package thingatpt
  :ensure nil
  :config
  (defun +thing-at-point-or-region (&optional thing prompt)
    "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
    (declare (side-effect-free t))
    (cond ((stringp thing)
           thing)
          ((+region-active-p)
           (buffer-substring-no-properties
            (+region-beginning)
            (+region-end)))
          (thing
           (thing-at-point thing t))
          ((require 'xref nil t)
           ;; Eglot, nox (a fork of eglot), and elpy implementations for
           ;; `xref-backend-identifier-at-point' betray the documented purpose of
           ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
           ;; the line number to the symbol.
           (if (memq (xref-find-backend) '(eglot elpy nox))
               (thing-at-point 'symbol t)
             ;; A little smarter than using `symbol-at-point', though in most
             ;; cases, xref ends up using `symbol-at-point' anyway.
             (xref-backend-identifier-at-point (xref-find-backend))))
          (prompt
           (read-string (if (stringp prompt) prompt ""))))))

;;;;; tr-ime

(use-package tr-ime
  :when IS-WINDOWS
  :config
  (defun +tr-ime-w32-frame-setup ()
    "Setup frame for w32-ime"
    (when (eq (framep (selected-frame)) 'w32)
      ;; Remove the hook since this only needs to be called once per session
      (remove-hook 'server-after-make-frame-hook #'+tr-ime-w32-frame-setup)

      ;; IME font setting
      (modify-all-frames-parameters '((ime-font . "Sarasa Term J-10")))

      ;; tr-ime
      (tr-ime-advanced-install t)

      ;; w32-ime settings
      (when (featurep 'w32-ime)
        ;; Set IME as the default input method
        (setq default-input-method "W32-IME")

        ;; IME modeline settings
        (setq-default w32-ime-mode-line-state-indicator "")
        (setq w32-ime-mode-line-state-indicator-list '("" "" ""))

        ;; Initialize IME
        (w32-ime-initialize)
        ;; IME controls (e.g. disabling IME for y/n confirmation)
        (wrap-function-to-control-ime 'universal-argument t nil)
        (wrap-function-to-control-ime 'read-string nil nil)
        (wrap-function-to-control-ime 'read-char nil nil)
        (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
        (wrap-function-to-control-ime 'y-or-n-p nil nil)
        (wrap-function-to-control-ime 'yes-or-no-p nil nil)
        (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
        (wrap-function-to-control-ime 'register-read-with-preview nil nil)
        (wrap-function-to-control-ime 'evil-normal-state nil nil))))

  ;; Setup frame if the selected frame is an ordinary frame (w32)
  ;; Add the setup function to a hook if it is console or daemon mode
  (if (eq (framep (selected-frame)) 'w32)
      (+tr-ime-w32-frame-setup)
    (add-hook 'server-after-make-frame-hook #'+tr-ime-w32-frame-setup)))

;;;;; treemacs

(use-package treemacs
  :defer t
  :config
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat +cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat +cache-dir "treemacs-last-error-persist"))
  (treemacs-follow-mode -1)
  (treemacs-resize-icons 12))

;;;;; treemacs-nerd-icons

(use-package treemacs-nerd-icons
  :disabled
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

;;;;; treemacs-evil

(use-package treemacs-evil
  :ensure nil
  :after (treemacs evil)
  :general
  (:keymaps 'evil-treemacs-state-map
   "ov" 'treemacs-visit-node-horizontal-split
   "os" 'treemacs-visit-node-vertical-split))

;;;;; treemacs-projectile

(use-package treemacs-projectile
  :after (treemacs))

;;;;; treemacs-magit

(use-package treemacs-magit
  :after (treemacs magit))

;;;;; treesit

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        `((ada . ("https://github.com/briot/tree-sitter-ada"))
          (agda . ("https://github.com/AusCyberman/tree-sitter-agda"))
          (angular . ("https://github.com/dlvandenberg/tree-sitter-angular"))
          (apex . ("https://github.com/aheber/tree-sitter-sfapex" nil "apex/src"))
          (arduino . ("https://github.com/ObserverOfTime/tree-sitter-arduino"))
          (asm . ("https://github.com/RubixDev/tree-sitter-asm"))
          (astro . ("https://github.com/virchau13/tree-sitter-astro"))
          (authzed . ("https://github.com/mleonidas/tree-sitter-authzed"))
          (awk . ("https://github.com/Beaglefoot/tree-sitter-awk"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (bass . ("https://github.com/vito/tree-sitter-bass"))
          (beancount . ("https://github.com/polarmutex/tree-sitter-beancount"))
          (bibtex . ("https://github.com/latex-lsp/tree-sitter-bibtex"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (comment . ("https://github.com/stsewd/tree-sitter-comment"))
          (commonlisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (csv . ("https://github.com/amaanq/tree-sitter-csv" nil "csv/src"))
          (cuda . ("https://github.com/theHamsta/tree-sitter-cuda"))
          (d . ("https://github.com/gdamore/tree-sitter-d"))
          (dart . ("https://github.com/UserNobody14/tree-sitter-dart"))
          (devicetree . ("https://github.com/joelspadin/tree-sitter-devicetree"))
          (diff . ("https://github.com/the-mikedavis/tree-sitter-diff"))
          (disassembly . ("https://github.com/ColinKennedy/tree-sitter-disassembly"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (dot . ("https://github.com/rydesun/tree-sitter-dot"))
          (doxygen . ("https://github.com/amaanq/tree-sitter-doxygen"))
          (dtd . ("https://github.com/tree-sitter-grammars/tree-sitter-xml" nil "dtd/src"))
          (eex . ("https://github.com/connorlay/tree-sitter-eex"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          (elm . ("https://github.com/elm-tooling/tree-sitter-elm"))
          (elvish . ("https://github.com/ckafi/tree-sitter-elvish"))
          (erlang . ("https://github.com/AbstractMachinesLab/tree-sitter-erlang"))
          (fennel . ("https://github.com/travonted/tree-sitter-fennel"))
          (fidl . ("https://github.com/google/tree-sitter-fidl"))
          (fish . ("https://github.com/ram02z/tree-sitter-fish"))
          (fluent . ("https://github.com/tree-sitter/tree-sitter-fluent"))
          (fortran . ("https://github.com/stadelmanma/tree-sitter-fortran"))
          (fsharp . ("https://github.com/ionide/tree-sitter-fsharp.git"))
          (fusion . ("https://gitlab.com/jirgn/tree-sitter-fusion.git"))
          (git-config . ("https://github.com/the-mikedavis/tree-sitter-git-config"))
          (git-rebase . ("https://github.com/the-mikedavis/tree-sitter-git-rebase"))
          (gitattributes . ("https://github.com/ObserverOfTime/tree-sitter-gitattributes"))
          (gitcommit . ("https://github.com/gbprod/tree-sitter-gitcommit"))
          (gitignore . ("https://github.com/shunsambongi/tree-sitter-gitignore"))
          (gleam . ("https://github.com/J3RN/tree-sitter-gleam"))
          (glimmer . ("https://github.com/alexlafroscia/tree-sitter-glimmer"))
          (glsl . ("https://github.com/theHamsta/tree-sitter-glsl"))
          (gnuplot . ("https://github.com/dpezto/tree-sitter-gnuplot"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (go-mod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (go-work . ("https://github.com/omertuc/tree-sitter-go-work"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (gotmpl . ("https://github.com/ngalaiko/tree-sitter-go-template"))
          (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))
          (graphql . ("https://github.com/bkegley/tree-sitter-graphql"))
          (groovy . ("https://github.com/murtaza64/tree-sitter-groovy"))
          (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (hcl . ("https://github.com/MichaHoffmann/tree-sitter-hcl"))
          (heex . ("https://github.com/connorlay/tree-sitter-heex"))
          (helm . ("https://github.com/ngalaiko/tree-sitter-go-template" nil "dialects/helm/src"))
          (hjson . ("https://github.com/winston0410/tree-sitter-hjson"))
          (hlsl . ("https://github.com/theHamsta/tree-sitter-hlsl"))
          (hocon . ("https://github.com/antosha417/tree-sitter-hocon"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (http . ("https://github.com/rest-nvim/tree-sitter-http"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (jq . ("https://github.com/flurie/tree-sitter-jq"))
          (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (json5 . ("https://github.com/Joakker/tree-sitter-json5"))
          (jsonnet . ("https://github.com/sourcegraph/tree-sitter-jsonnet"))
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (kusto . ("https://github.com/Willem-J-an/tree-sitter-kusto"))
          (lalrpop . ("https://github.com/traxys/tree-sitter-lalrpop"))
          (latex . ("https://github.com/latex-lsp/tree-sitter-latex" ))
          (ledger . ("https://github.com/cbarrete/tree-sitter-ledger"))
          (llvm . ("https://github.com/benwilliamgraham/tree-sitter-llvm"))
          (lua . ("https://github.com/MunifTanjim/tree-sitter-lua"))
          (m68k . ("https://github.com/grahambates/tree-sitter-m68k"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
          (matlab . ("https://github.com/acristoffers/tree-sitter-matlab"))
          (mediawiki . ("https://github.com/Ordoviz/tree-sitter-mediawiki"))
          (menhir . ("https://github.com/Kerl13/tree-sitter-menhir"))
          (mermaid . ("https://github.com/monaqa/tree-sitter-mermaid"))
          (meson . ("https://github.com/Decodetalkers/tree-sitter-meson"))
          (nasm . ("https://github.com/naclsn/tree-sitter-nasm"))
          (nim . ("https://github.com/alaviss/tree-sitter-nim"))
          (ninja . ("https://github.com/alemuller/tree-sitter-ninja"))
          (nix . ("https://github.com/cstrahan/tree-sitter-nix"))
          (norg . ("https://github.com/nvim-neorg/tree-sitter-norg"))
          (objc . ("https://github.com/amaanq/tree-sitter-objc"))
          (objdump . ("https://github.com/ColinKennedy/tree-sitter-objdump"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src"))
          (ocaml-interface . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/interface/src"))
          (ocaml-type . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/type/src"))
          (ocamllex . ("https://github.com/atom-ocaml/tree-sitter-ocamllex"))
          (pascal . ("https://github.com/Isopod/tree-sitter-pascal"))
          (perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))
          (pgn . ("https://github.com/rolandwalker/tree-sitter-pgn.git"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php" nil "php/src"))
          (pioasm . ("https://github.com/leo60228/tree-sitter-pioasm"))
          (powershell . ("https://github.com/airbus-cert/tree-sitter-powershell"))
          (prisma . ("https://github.com/victorhqc/tree-sitter-prisma"))
          (proto . ("https://github.com/treywood/tree-sitter-proto"))
          (psv . ("https://github.com/amaanq/tree-sitter-csv" nil "psv/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ql . ("https://github.com/tree-sitter/tree-sitter-ql"))
          (qmljs . ("https://github.com/yuja/tree-sitter-qmljs"))
          (query . ("https://github.com/nvim-treesitter/tree-sitter-query"))
          (r . ("https://github.com/r-lib/tree-sitter-r.git"))
          (racket . ("https://github.com/6cdh/tree-sitter-racket"))
          (rasi . ("https://github.com/Fymyte/tree-sitter-rasi"))
          (regex . ("https://github.com/tree-sitter/tree-sitter-regex"))
          (rego . ("https://github.com/FallenAngel97/tree-sitter-rego"))
          (rnoweb . ("https://github.com/bamonroe/tree-sitter-rnoweb"))
          (rst . ("https://github.com/stsewd/tree-sitter-rst"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (scheme . ("https://github.com/6cdh/tree-sitter-scheme.git"))
          (scss . ("https://github.com/serenadeai/tree-sitter-scss"))
          (solidity . ("https://github.com/JoranHonig/tree-sitter-solidity"))
          (sparql . ("https://github.com/BonaBeavis/tree-sitter-sparql"))
          (sql . ("https://github.com/derekstride/tree-sitter-sql" "gh-pages"))
          (supercollider . ("https://github.com/madskjeldgaard/tree-sitter-supercollider"))
          (surface . ("https://github.com/connorlay/tree-sitter-surface"))
          (svelte . ("https://github.com/Himujjal/tree-sitter-svelte"))
          (swift . ("https://github.com/alex-pinkus/tree-sitter-swift" ))
          (sxhkdrc . ("https://github.com/RaafatTurki/tree-sitter-sxhkdrc"))
          (teal . ("https://github.com/euclidianAce/tree-sitter-teal" ))
          (tiger . ("https://github.com/ambroisie/tree-sitter-tiger"))
          (tlaplus . ("https://github.com/tlaplus-community/tree-sitter-tlaplus"))
          (tmux . ("https://github.com/Freed-Wu/tree-sitter-tmux"))
          (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
          (tsv . ("https://github.com/amaanq/tree-sitter-csv" nil "tsv/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (turtle . ("https://github.com/BonaBeavis/tree-sitter-turtle"))
          (twig . ("https://github.com/gbprod/tree-sitter-twig"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (vala . ("https://github.com/vala-lang/tree-sitter-vala"))
          (verilog . ("https://github.com/tree-sitter/tree-sitter-verilog.git"))
          (vhs . ("https://github.com/charmbracelet/tree-sitter-vhs"))
          (vim . ("https://github.com/neovim/tree-sitter-vim"))
          (vimdoc . ("https://github.com/neovim/tree-sitter-vimdoc"))
          (vls . ("https://github.com/vlang/vls" nil "tree_sitter_v/src"))
          (vue . ("https://github.com/ikatyang/tree-sitter-vue"))
          (wgsl . ("https://github.com/szebniok/tree-sitter-wgsl"))
          (xml . ("https://github.com/tree-sitter-grammars/tree-sitter-xml" nil "xml/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (yang . ("https://github.com/Hubro/tree-sitter-yang"))
          (zig . ("https://github.com/maxxnino/tree-sitter-zig")))))

;;;;; undo-fu

(use-package undo-fu
  :disabled
  :general
  ([remap undo] 'undo-fu-only-undo
   [remap redo] 'undo-fu-only-redo
   "C-_" 'undo-fu-only-undo
   "M-_" 'undo-fu-only-redo
   "C-M-_" 'undo-fu-only-redo-all)
  :config
  (setq undo-fu-ignore-keyboard-quit t
        undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)) ; 48mb  (default is 24mb)

;;;;; undo-fu-session

(use-package undo-fu-session
  :hook (elpaca-after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst))

  (defun +undo-fu-make-hashed-session-file-name (file)
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file))
            (undo-fu-session--file-name-ext)))
  (advice-add #'undo-fu-session--make-file-name
              :override #'+undo-fu-make-hashed-session-file-name))

;;;;; undo-tree

(use-package undo-tree
  :ensure (undo-tree
           :host gitlab :repo "tsc25/undo-tree")
  :hook (elpaca-after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat +cache-dir "undo-tree-hist/")))
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000) ; 128mb (default is 24mb)

  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).

  ;; (when (executable-find "zstd")
  ;;   (defadvice +undo--append-zst-extension-to-file-name-a (file)
  ;;     :filter-return #'undo-tree-make-history-save-file-name
  ;;     (concat file ".zst")))

  ;; (defadvice +undo--strip-text-properties-a (&rest _)
  ;;   :before #'undo-list-transfer-to-tree
  ;;   (dolist (item buffer-undo-list)
  ;;     (and (consp item)
  ;;          (stringp (car item))
  ;;          (setcar item (substring-no-properties (car item))))))
  )

;;;;; vertico

(use-package vertico
  :general
  (:keymaps 'vertico-map
   "M-RET" 'vertico-exit-input
   "C-j" 'vertico-next
   "C-M-j" 'vertico-next-group
   "C-k" 'vertico-previous
   "C-M-k" 'vertico-previous-group
   "DEL" 'vertico-directory-delete-char)
  :init
  (vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;;;;; vundo

(use-package vundo
  :disabled
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display nil))

;;;;; web-mode

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.jsx?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.vue\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :hook (web-mode . lsp)
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1))

;;;;; which-key

(use-package which-key
  :demand t
  :config
  (which-key-mode))

;;;;; window

(use-package window
  :ensure nil
  :config
  (setq even-window-sizes nil))

;;;;; ws-butler

(use-package ws-butler
  :ensure (ws-butler
           :host github :repo "hlissner/ws-butler")
  :hook (elpaca-after-init . ws-butler-global-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

;;;;; xref

(use-package xref
  :defer t
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep))

;;;;; yasnippet

(use-package yasnippet
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :hook (elpaca-after-init . yas-global-mode))

;;;; Language specific packages

(use-package eglot
  :disabled
  :commands eglot eglot-ensure
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-size 0
        eglot-auto-display-help-buffer nil))

;;;;; lsp

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((powershell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-completion-provider :none)
  (add-hook 'lsp-mode-hook #'lsp-completion-mode))

;;;;; lsp-ui

(use-package lsp-ui
  :disabled
  :commands lsp-ui-mode)

;;;;; lsp-treemacs

(use-package lsp-treemacs
  :disabled
  :defer t
  :config
  (lsp-treemacs-sync-mode 1))

;;;;; dap-mode

(use-package dap-mode
  :disabled)

;;;;; dart

;;;;;; dart

(use-package dart-mode
  ;; :hook (dart-mode . eglot-ensure)
  )

;;;;;; flutter

(use-package flutter
  :disabled
  :after dart-mode
  :general
  (local-leader-def!
    :major-modes '(dart-mode)
    :keymaps 'dart-mode-map
    "r" 'flutter-run-or-hot-reload)
  :config
  (setq flutter-sdk-path "/Applications/flutter/"))

;;;;;; lsp

(use-package lsp-dart
  :disabled
  :hook (dart-mode . lsp))

;;;;; go
(use-package go-mode
  :mode "\\.go\\'")

;;;;; JSON
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

;;;;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;;;;; PHP
(use-package php-mode
  :mode "\\.inc\\'")

;;;;; Plantuml

(use-package plantuml-mode
  :commands plantuml-download-jar)

;;;;; Powershell

(use-package powershell
  :defer t)

;;;;; Python

(use-package cython-mode
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s"))

;;;;; Rust

(use-package rustic
  :mode ("\\.rs$" . rustic-mode))

;;;;; Typescript

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . lsp))

;;;;; Markdown

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode))

;;;;; VB.NET

(use-package vbnet-mode
  :ensure (:host github
           :repo "sakakibara/vbnet-mode")
  :mode ("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)
  :general
  (:keymaps 'vbnet-mode-map
   :states '(insert)
   "TAB" 'indent-for-tab-command)
  :config
  (setq vbnet-namespace-face 'font-lock-constant-face)
  (setq vbnet-funcall-face 'font-lock-function-call-face))
