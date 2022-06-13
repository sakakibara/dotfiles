;; -*- no-byte-compile: t; -*-
;;; lang/sql/packages.el

(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "5a3ce9e52cc3fb55ec02f3a38f43d687a9e3672d")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! ejc-sql :pin "5e100426be90fc193154b1219ab1058534a4fb52")
