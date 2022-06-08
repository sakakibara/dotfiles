;; -*- no-byte-compile: t; -*-
;;; input/japanese/packages.el

(when (featurep! +migemo)
  (package! migemo :pin "f756cba3d5268968da361463c2e29b3a659a3de7")
  (package! avy-migemo
    :recipe (:host github :repo "tam17aki/avy-migemo")
    :pin "88738a6657dd3e25ee3ef814512c402505da8669"))
(when IS-WINDOWS
  (package! tr-ime :pin "4bbbbfb00f1007e955fe856cd99108c9979f4295"))
(package! pangu-spacing :pin "f92898949ba3bf991fd229416f3bbb54e9c6c223")
