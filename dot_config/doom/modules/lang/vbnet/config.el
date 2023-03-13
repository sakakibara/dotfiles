;;; lang/vbnet/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! vbnet-mode
  :mode ("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)
  :config
  (custom-set-faces!
    `(vbnet-funcall-face
      :foreground ,(doom-color 'functions))
    `(vbnet-namespace-face
      :foreground ,(doom-color 'constants)))

  (map! :map vbnet-mode-map
        :i "TAB" #'indent-for-tab-command))
