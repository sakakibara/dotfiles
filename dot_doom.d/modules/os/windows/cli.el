;;; os/windows/cli.el -*- lexical-binding: t; -*-

(add-to-list 'doom-output-class-alist
             '(success . (lambda (str &rest args)
                           (apply #'doom--output-color 'green (format "o %s" str) args))))
