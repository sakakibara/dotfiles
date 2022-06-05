;;; lang/sql/config.el -*- lexical-binding: t; -*-

(after! sql
  (setq! sql-ms-program "sqlcmd"
         sql-ms-options nil
         sql-ms-login-params '(user password server database)))

(use-package! ejc-sql
  :hook (sql-mode . ejc-sql-mode)
  :config
  (setq nrepl-sync-request-timeout 180
        ejc-completion-system 'standard))

(use-package! ejc-company
  :when (featurep! :completion company)
  :after ejc-sql
  :config
  (set-company-backend! 'ejc-sql-mode 'ejc-company-backend))
