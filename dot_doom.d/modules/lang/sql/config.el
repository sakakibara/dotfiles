;;; lang/sql/config.el -*- lexical-binding: t; -*-

(after! sql
  (setq! sql-ms-program "sqlcmd"
         sql-ms-options nil
         sql-ms-login-params '(user password server database)))

(use-package! sql-indent
  :hook (sql-mode . sqlind-minor-mode)
  :hook (sql-interactive-mode . sqlind-minor-mode))

(use-package! sqlup-mode
  :hook (sql-mode . sqlup-mode)
  :hook (sql-interactive-mode . sqlup-mode)
  :config
  (setq sqlup-blacklist '("data"))
  (map! :map sql-mode-map
        :localleader
        (:prefix ("u" . "upcase")
        "b" #'sqlup-capitalize-keywords-in-buffer
        "r" #'sqlup-capitalize-keywords-in-region)))

(use-package! ejc-sql
  :hook (sql-mode . ejc-sql-mode)
  :config
  (setq nrepl-sync-request-timeout 180
        ejc-completion-system 'standard))

(use-package! ejc-company
  :when (modulep! :completion company)
  :after ejc-sql
  :config
  (set-company-backend! 'ejc-sql-mode 'ejc-company-backend))
