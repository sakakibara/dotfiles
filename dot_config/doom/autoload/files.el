;;; autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun enable-require-final-newline ()
  (setq-local require-final-newline t))

;;;###autoload
(defun disable-require-final-newline ()
  (setq-local require-final-newline nil))

;;;###autoload
(defun ns-move-file-to-trash (filename)
  "Move FILENAME to trash.

Use Finder to move the file to trash through `ns-do-applescript'."
  (ns-do-applescript
   (format
    "tell application \"Finder\" to move {the POSIX file \"%s\"} to trash"
    (replace-regexp-in-string (rx (group (any ?\\ ?\")))
                              "\\\\\\1"
                              (expand-file-name filename)))))
