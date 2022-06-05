;;; autoload/textobjects.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil:outer-line-txtobj "autoload/textobjects" nil nil)
(evil-define-text-object +evil:outer-line-txtobj (count &optional _beg _end type)
  "Text object to select the outer line at point."
  (evil-range (line-beginning-position) (line-end-position)))

;;;###autoload (autoload '+evil:inner-line-txtobj "autoload/textobjects" nil nil)
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
