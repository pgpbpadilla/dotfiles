;; Packages: Org Mode extensions
(unless (package-installed-p 'helm-org)
  (package-install 'helm-org))
(unless (package-installed-p 'org-projectile)
  (package-install 'org-projectile))

(when (eq system-type 'darwin)
  (message "Setting variables for Darwin")
  (custom-set-variables
   '(ns-command-modifier 'meta))
  ;; I wanted load my Org-Mode personal files only on this host
  ;; (when (string= (sytem-name) "pgpb-macbook13-2020.fritz.box")
  ;;   (message (system-name)))
  )

;; Org-Mode Options
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(defun org-refresh () 
  "Evaluate the variable `org-agenda-files` as defined in the emacs init file"
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org.gpg$"))
  (message "All Org agenda files have been reloaded."))
(org-refresh)
(setq org-agenda-span 'day)



;; MobileOrg options
;; https://mobileorg.github.io/documentation/#using-dropbox
;; Set to the location of your Org files on your local system
;; (setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; TODO State Configuration

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "WONTDO" "IGNORE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) 
        ("WONTDO" . (:foreground "blue" :weight bold))))

;; track progress history across emacs sessions

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; All Notes captured will go to this file
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c h") 'helm-org-agenda-files-headings)

;; refile to another file
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-log-into-drawer t)

;; Configures Org Mode: how to identify Stuck projects

(setq org-stuck-projects 
      '(
        "-notes+PROJECT/-MAYBE-DONE-WONTDO" 
        ("NEXT" "TODO") 
        ("@shop") 
        "\\<IGNORE\\>")
      )

;; Inline image size
(setq org-image-actual-width nil)

;; enable evaluation of Shell code blocks
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
        (gnuplot . t)
    )
)

;; Show fancy bullets
;; https://github.com/sabof/org-bullets
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; Hide duplicate lines when SCHEDULED and DEADLINE overlap
;; https://superuser.com/a/530450/148349
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)


;; Log state changes
(setq org-log-done 'note)
;; Hide DONE items in the Agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
