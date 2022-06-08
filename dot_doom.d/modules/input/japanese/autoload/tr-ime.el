;;; input/japanese/autoload/tr-ime.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +japanese--w32-frame-setup ()
  "Setup frame for w32-ime"
  (when (eq (framep (selected-frame)) 'w32)
    ;; Remove the hook since this only needs to be called once per session
    (remove-hook 'server-after-make-frame-hook #'+windows--w32-frame-setup)

    ;; IME font setting
    (modify-all-frames-parameters '((ime-font . "PlemolJP-10")))

    ;; tr-ime
    (tr-ime-advanced-install)
    ;; (tr-ime-advanced-initialize)

    ;; w32-ime settings
    (when (featurep 'w32-ime)
      ;; Set IME as the default input method
      (setq default-input-method "W32-IME")

      ;; IME modeline settings
      (setq-default w32-ime-mode-line-state-indicator "")
      (setq w32-ime-mode-line-state-indicator-list '("" "" ""))

      ;; Initialize IME
      (w32-ime-initialize)
      ;; IME controls (e.g. disabling IME for y/n confirmation)
      (wrap-function-to-control-ime 'universal-argument t nil)
      (wrap-function-to-control-ime 'read-string nil nil)
      (wrap-function-to-control-ime 'read-char nil nil)
      (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
      (wrap-function-to-control-ime 'y-or-n-p nil nil)
      (wrap-function-to-control-ime 'yes-or-no-p nil nil)
      (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
      (wrap-function-to-control-ime 'register-read-with-preview nil nil)
      (wrap-function-to-control-ime 'evil-normal-state nil nil))))
