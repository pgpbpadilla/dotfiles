(defvar gpg-org-regex "\\.org.gpg$")

(defun my/org-agenda-files ()
  (directory-files-recursively my-org-agenda-dir gpg-org-regex))

(defun my/org-journal-files ()
  (directory-files-recursively my-org-journal-dir gpg-org-regex))

(defun my/org-archive-files ()
  (directory-files-recursively my-org-archive-dir gpg-org-regex))

(defun my/org-refile-targets ()
  (setq org-refile-targets
        '(
          (nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)
          (my/org-journal-files :maxlevel . 3)
          (my/org-archive-files :maxlevel . 3)
          )
        ))


(defun my/org-extra-files ()
  (append
   (my/org-journal-files)
   (my/org-archive-files))
  )


(defun org-refresh () 
  "Reload agenda files, usually to include newly created files."
  (interactive)
  (setq org-agenda-files (my/org-agenda-files))
  (my/org-refile-targets)
  (message "All Org agenda files have been reloaded."))


;;; Export as an Emacs package
(provide 'pgpb-org-utils)
