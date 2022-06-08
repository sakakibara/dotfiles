;;; input/japanese/doctor.el -*- lexical-binding: t; -*-

(when (and (featurep! +migemo)
           (require 'migemo nil t))
  (unless (executable-find migemo-command)
    (warn! "Cannot find %S, migemo won't work" migemo-command)))
