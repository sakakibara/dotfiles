;;; autoload/rjsx.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +rjsx--comment-search-forward (limit &optional noerror)
  "Find a comment start between point and LIMIT.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil if NOERROR is non-nil.

Ensure that `comment-normalize-vars' has been called before you use this."
  (if (not comment-use-syntax)
      (if (re-search-forward rjsx-comment-start-skip limit noerror)
          (or (match-end 1) (match-beginning 0))
        (goto-char limit)
        (unless noerror (error "No comment")))
    (let* ((pt (point))
           ;; Assume (at first) that pt is outside of any string.
           (s (parse-partial-sexp pt (or limit (point-max)) nil nil
                                  (if comment-use-global-state (syntax-ppss pt))
                                  t)))
      (when (and (nth 8 s) (nth 3 s) (not comment-use-global-state))
        ;; The search ended at eol inside a string.  Try to see if it
        ;; works better when we assume that pt is inside a string.
        (setq s (parse-partial-sexp
                 pt (or limit (point-max)) nil nil
                 (list nil nil nil (nth 3 s) nil nil nil nil)
                 t)))
      (if (or (not (and (nth 8 s) (not (nth 3 s))))
              ;; Make sure the comment starts after PT.
              (< (nth 8 s) pt))
          (unless noerror (error "No comment"))
        ;; We found the comment.
        (let ((pos (point))
              (start (nth 8 s))
              (bol (line-beginning-position))
              (end nil))
          (while (and (null end) (>= (point) bol))
            (if (looking-at rjsx-comment-start-skip)
                (setq end (min (or limit (point-max)) (match-end 0)))
              (backward-char)))
          (goto-char (or end pos))
          start)))))

;;;###autoload
(defun +rjsx--comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment` but only for positive N and can use regexps instead of syntax."
  (setq n (or n 1))
  (if (< n 0) (error "No comment-backward")
    (if comment-use-syntax (forward-comment n)
      (while (> n 0)
        (setq n
              (if (or (forward-comment 1)
                      (and (looking-at rjsx-comment-start-skip)
                           (goto-char (match-end 0))
                           (re-search-forward comment-end-skip nil 'move)))
                  (1- n) -1)))
      (= n 0))))

;;;###autoload
(defun +rjsx--uncomment-region-function (beg end &optional _)
  (js2-mode-wait-for-parse
   (lambda ()
     (goto-char beg)
     (setq end (copy-marker end))
     (let (cs ts te ce matched-start)
       ;; find comment start
       (while (and (<= (point) end)
                   (setq ipt (point))
                   (setq spt (+rjsx--comment-search-forward end t)))
         (let ((ept (progn
                      (goto-char spt)
                      (unless (or (+rjsx--comment-forward)
                                  (eobp))
                        (error "Can't find the comment end"))
                      (point))))
           (save-restriction
             (narrow-to-region spt ept)
             ;; delete comment-start
             (goto-char ipt)
             (setq matched-start
                   (and (re-search-forward comment-start-skip end t 1)
                        (match-string-no-properties 0)))
             (setq cs (match-beginning 1))
             (setq ts (match-end 1))
             (goto-char cs)
             (delete-region cs ts)
             ;; delete comment-padding start
             (when (and comment-padding (looking-at (regexp-quote comment-padding)))
               (delete-region (point) (+ (point) (length comment-padding))))
             ;; find comment end
             (when (re-search-forward (if (string-match "//+" matched-start) "\n" "\\*/}?") end t 1)
               (setq te (or (match-beginning 1) (match-beginning 0)))
               (setq ce (or (match-end 1) (match-end 0)))
               (goto-char te)
               ;; delete commend-end if it's not a newline
               (unless (string= "\n" (match-string-no-properties 0))
                 (delete-region te ce)
                 ;; delete comment-padding end
                 (when comment-padding
                   (backward-char (length comment-padding))
                   (when (looking-at (regexp-quote comment-padding))
                     (delete-region (point) (+ (point) (length comment-padding))))))
               ;; unescape inner comments if any
               (save-restriction
                 (narrow-to-region cs (point))
                 (comment-quote-nested "{/*" "*/}" t)))
             (goto-char (point-max))))))
     (rjsx-maybe-unwrap-expr beg end)
     (set-marker end nil))))
