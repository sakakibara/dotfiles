;;; os/windows/cli.el -*- lexical-binding: t; -*-

(setq straight-base-dir "c:/.local/")

(add-to-list 'doom-print-class-alist
             '(success . (lambda (str &rest args)
                 (apply #'doom-print--style 'green
                        (doom-print--indent str "o ")
                        args))))
