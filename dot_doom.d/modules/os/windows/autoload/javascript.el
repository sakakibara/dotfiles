;;; os/windows/autoload/javascript.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +windows--build-add-node-modules-path-command ()
  "Build `add-node-modules-path-command' manually to work around issues with npm.cmd."
  (let* ((node (executable-find "node"))
         (base (file-name-directory node))
         (npm-cli (concat base "node_modules/npm/bin/npm-cli.js"))
         (bin-cmd "bin"))
    (mapconcat #'shell-quote-argument (list node npm-cli bin-cmd) " ")))

;;;###autoload
(defun +windows--remove-javascript-npm-mode-hook-h ()
  (dolist (mode '(html-mode
                  css-mode
                  web-mode
                  markdown-mode
                  js-mode
                  json-mode
                  typescript-mode
                  solidity-mode))
    (remove-hook (intern (format "%s-hook" mode))
                 (intern (format "doom--enable-%s-in-%s-h"
                                 '+javascript-npm-mode mode)))))
