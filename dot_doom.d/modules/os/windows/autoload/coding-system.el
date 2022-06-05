;;; os/windows/autoload/coding-system.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +windows--coding-system-setup ()
  "Setup windows coding system"
  (setq locale-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'cp932)
  (setq-default selection-coding-system 'utf-16le-dos))

;;;###autoload
(defun +windows--coding-system-for-rw-cp932-a (fn &rest args)
  "Use cp932 for coding-system-for-read and coding-system-for-write"
  (let ((coding-system-for-read 'cp932)
        (coding-system-for-write 'cp932))
    (apply fn args)))

;;;###autoload
(defun +windows--coding-system-for-rw-cp932-utf-8-a (fn &rest args)
  "Use cp932 for coding-system-for-read and utf-8 for coding-system-for-write"
  (let ((coding-system-for-read 'cp932)
        (coding-system-for-write 'utf-8))
    (apply fn args)))

;;;###autoload
(defun +windows--coding-system-for-rw-utf-8-cp932-a (fn &rest args)
  "Use utf-8 for coding-system-for-read and cp932 for coding-system-for-write"
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'cp932))
    (apply fn args)))
