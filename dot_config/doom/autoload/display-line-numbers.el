;;; autoload/display-line-numbers.el -*- lexical-binding: t; -*-

(defvar pre-insert-state-display-line-numbers display-line-numbers
  "Variable to hold the value of display line numbers before insert state")

;;;###autoload
(defun display-line-numbers--turn-off ()
  "Turn off `display-line-numbers-mode'."
  (unless (minibufferp)
    (display-line-numbers-mode -1)))

;;;###autoload
(defun evil-insert-state-entry-display-numbers-h ()
  (when display-line-numbers
    (setq pre-insert-state-display-line-numbers display-line-numbers)
    (setq display-line-numbers t)))

;;;###autoload
(defun evil-insert-state-exit-display-numbers-h ()
  (when pre-insert-state-display-line-numbers
    (setq display-line-numbers pre-insert-state-display-line-numbers)
    (setq pre-insert-state-display-line-numbers nil)))
