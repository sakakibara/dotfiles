;;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;; OS constants

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

;;;; Performance Settings

;; Disable package.el
(setq package-enable-at-startup nil)

;; Load default library
(setq inhibit-default-init t)

;; Native comp
(with-eval-after-load 'comp
  (setq native-comp-async-report-warnings-errors nil
        native-comp-async-jobs-number 8
        native-comp-speed 3))

;;;;; File name handler alist

;; Store the default file-name-handler-alist in a variable
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set file-name-handler-alist to nil during initialization
(setq file-name-handler-alist nil)

;;;;; Garbage collection

;; Temporarily increase gc-cons-threshold during initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

;;;;; Reset changed values

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold (* 16 1024 1024))
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;;;; UI settings

;;;;; Frame

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;;;; Cursor

(blink-cursor-mode -1)
(setq blink-matching-paren nil
      x-stretch-cursor nil)

;;;;; Font

(defvar +standard-fontset
  (create-fontset-from-fontset-spec standard-fontset-spec)
  "Standard fontset for user.")

(defvar +font-size 14
  "Default font size in px.")

(defvar +cjk-font "Sarasa Term J"
  "Default font for cjk characters.")

(defvar +latin-font "Iosevka"
  "Default font for Latin characters.")

(defvar +unicode-font "Noto Emoji"
  "Default font for Unicode characters, including emojis.")

(defvar +cjk-font-scale
  '((14 . 1.0)
    (15 . 1.0)
    (16 . 1.0)
    (17 . 1.0)
    (18 . 1.0))
  "Scaling factor to use for cjk font of given size.")

(defvar +unicode-font-scale
  '((14 . 0.9)
    (15 . 1.0)
    (16 . 1.0)
    (17 . 1.0)
    (18 . 1.0))
  "Scaling factor to use for Unicode font of given size.")

(defun +set-font ()
  "Set Unicode, Latin and CJK font for +standard-fontset."
  (set-fontset-font +standard-fontset 'unicode
                    (font-spec :family +unicode-font) nil 'prepend)

  (setq face-font-rescale-alist
        (list (cons +cjk-font
                    (cdr (assoc +font-size +cjk-font-scale)))
              (cons +unicode-font
                    (cdr (assoc +font-size +unicode-font-scale)))))

  (set-fontset-font +standard-fontset 'latin
                    (font-spec :family +latin-font :size +font-size) nil 'prepend)

  (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
    (set-fontset-font +standard-fontset charset
                      (font-spec :family +cjk-font) nil 'prepend))

  (dolist (charset '((#x2018 . #x2019)    ;; Curly single quotes "‘’"
                     (#x201c . #x201d)))  ;; Curly double quotes "“”"
    (set-fontset-font +standard-fontset charset
                      (font-spec :family +cjk-font) nil 'prepend)))

(+set-font)

(add-to-list 'default-frame-alist (cons 'font +standard-fontset))
(add-to-list 'initial-frame-alist (cons 'font +standard-fontset))

;; (push '(font . "Iosevka") default-frame-alist)
;; (set-face-font 'default "Iosevka")
;; (set-face-font 'variable-pitch "Iosevka")
;; (copy-face 'default 'fixed-pitch)

;; (when IS-WINDOWS
;;   (set-face-attribute 'default nil :height 100))

;;;; Integrations

(setq use-package-enable-imenu-support t)

;;;; Modifiers

(cond
 (IS-MAC
  (setq mac-command-modifier 'super
        ns-command-modifier 'super
        mac-option-modifier 'meta
        ns-option-modifier 'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier 'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))
