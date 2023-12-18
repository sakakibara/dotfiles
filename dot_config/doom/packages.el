;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! hl-line :disable t)
(package! form-feed :pin "ac1f0ef30a11979f5dfe12d8c05a666739e486ff" :disable t)
(package! eaw
  :recipe (:host github :repo "hamano/locale-eaw"
           :files ("eaw.el"))
  :pin "37456e118bdfd6a97508c3998f06153148366a7a" :disable t)
(package! openwith :pin "1dc89670822966fab6e656f6519fdd7f01e8301a" :disable t)
(package! rg :pin "dcbaa48689d3d73f8a142a1ab5f1e722d7434ff9")

(when (modulep! :lang csharp)
  (package! font-lock-ext
    :recipe (:host github :repo "sensorflo/font-lock-ext"
             :files ("font-lock-ext.el"))
    :pin "b6c82e8ac7996d96494a54454015a98ceb883feb"))

(when (modulep! :lang org)
  ;; (package! org-wild-notifier)
  (package! org-modern)
  (package! org-modern-indent
    :recipe (:host github :repo "jdtsmith/org-modern-indent")))

(when (modulep! :lang sh +xonsh)
  (package! xonsh-mode
    :pin "7fa581524533a9b6b770426e4445e571a69e469d"))

(when (modulep! :lang sh +elvish)
  (package! elvish-mode
    :pin "a13fcaf209d803e2e450ca2bf80dea94b40a0141"))

;; Disable osx-trash since it has problems with newer macos
;; (when (modulep! :os macos)
;;   (package! osx-trash :disable t))

;; (when (modulep! :lang org +roam2)
;;   (unpin! org-roam)
;;   (package! org-roam-ui))
(when (modulep! :lang org)
  (unpin! org))

(when (modulep! :lang org +roam2)
  (unpin! org-roam)
  (package! org-roam-ui))

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
