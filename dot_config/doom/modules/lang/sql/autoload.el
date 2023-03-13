;;; lang/sql/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sql-connect-ms ()
  "Connect to SQL Server DB in a comint buffer."
  ;; Do something with `sql-user', `sql-password',
  ;; `sql-database', and `sql-server'.
  (let ((f #'(lambda (op val)
               (unless (string= "" val)
                 (setq sql-ms-options
                       (append (list op val) sql-ms-options)))))
        (params `(("-U" . ,sql-user)("-P" . ,sql-password)
                  ("-S" . ,sql-server)("-d" . ,sql-database))))
    (dolist (pair params)
      (funcall f (car pair)(cdr pair)))
    (sql-connect-1 sql-ms-program sql-ms-options)))
