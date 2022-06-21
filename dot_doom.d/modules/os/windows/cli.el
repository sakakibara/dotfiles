;;; os/windows/cli.el -*- lexical-binding: t; -*-

(add-to-list 'doom-print-class-alist
             '(success . (lambda (str &rest args)
                 (apply #'doom-print--style 'green
                        (doom-print--indent str "o ")
                        args))))
