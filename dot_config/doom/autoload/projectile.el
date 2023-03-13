;;; os/windows/autoload/projectile.el -*- lexical-binding: t; -*-

(defvar +project-directory-parent-directories nil
  "List of directories to consider as project parent")

;;;###autoload
(defun +projectile-root-parent (dir)
  "Identify a project root in DIR by checking if it is under PATH.
Return the first (topmost) matched directory or nil if not found."
  (let ((parent-dir (directory-file-name (file-name-directory (directory-file-name dir)))))
    (when (member parent-dir +project-directory-parent-directories)
      dir)))

;;;###autoload
(defun +projectile-svn-aware-project-name (project-root)
  "Windows specific function used to create project name to be display based on the value of PROJECT-ROOT"
  (let ((project-name (projectile-default-project-name project-root))
        (ignore-project-directory-names '("trunk")))
    (while (member project-name ignore-project-directory-names)
      (setq project-root (file-name-directory (directory-file-name project-root)))
      (setq project-name (downcase (file-name-nondirectory (directory-file-name project-root)))))
    (concat (upcase (substring project-name 0 1)) (substring project-name 1))))
