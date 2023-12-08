;;; Bootstrap Emacs configuration

;; This is bad! Can result in file corruption
;; Remove asap.
;; My guess is that gnupg@2.2.42 brought this bug as a hotfix
;; should try to downgrade to 2.2.41
(fset 'epg-wait-for-status 'ignore)

;; set directory to use for loading agenda files
(defvar my-org-agenda-dir "~/org/31e520d4df")

;; set directory for journal files
(defvar my-org-journal-dir "~/org/0f6de25076")

;; set directory for archived files
(defvar my-org-archive-dir "~/org/2204c36fc7")

(load "~/dotfiles/emacs/init.el")
