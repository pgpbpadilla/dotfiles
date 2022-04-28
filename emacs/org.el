;;; Configure Agenda

(defun org-refresh () 
  "Evaluate the variable `org-agenda-files` as defined in the emacs init file"
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org.gpg$"))
  (message "All Org agenda files have been reloaded."))

(org-refresh)

(setq org-agenda-span 'day)


;; Packages: Org Mode extensions
(unless (package-installed-p 'helm-org)
  (package-install 'helm-org))
(unless (package-installed-p 'org-projectile)
  (package-install 'org-projectile))
(global-set-key (kbd "C-c h") 'helm-org-agenda-files-headings)

;; Auto-insert new lines for long lines
(add-hook 'org-mode-hook 'turn-on-auto-fill)


;; MobileOrg options
;; https://mobileorg.github.io/documentation/#using-dropbox
;; Set to the location of your Org files on your local system
;; (setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;;; TODO states
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "WONTDO" "IGNORE" "INFO")))

(setq org-todo-keyword-faces
      '(
        ("TODO" . org-warning) 
        ("WONTDO" . (:foreground "blue" :weight bold))
        ("INFO" . (:foreground "blue" :waight bold))
        )
      )

;;; track progress history across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;;; Shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; All Notes captured will go to this file
(setq org-default-notes-file (concat org-directory "/notes.org.gpg"))


;; refile to another file
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; (setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-log-into-drawer t)

;; Configures Org Mode: how to identify Stuck projects
(setq org-stuck-projects 
      '(
        ;; Exclude items with tags: notes, improve, info, template
        ;; Exclude items with TODO keywords: MAYBE, DONE, WONTDO, INFO
        ;; Include items that part of a =PROJECT=
        "-template-info-notes-improve+PROJECT/-MAYBE-DONE-WONTDO-INFO" 
        ("NEXT" "TODO") 
        ("@shop") 
        ;; Exclude every children of an item marked with the IGNORE TODO keyword
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


;; Export backends
(setq org-export-backends '(ascii html icalendar latex md odt))


;; Custom Agenda commands
;; Examples: https://orgmode.org/manual/Matching-tags-and-properties.html
;; 
;; ‘work/WAITING’ = means => ‘work+TODO​="WAITING"’.
;; List all improvements that are not DONE.
(setq org-agenda-custom-commands 
      '(
        ("i" "Improvements" tags "+improve-info/-DONE-INFO-WONTDO")
        ("z" "Ziele" tags "+goal+life-backlog-reminder/-INFO")
        ("p" "Public tasks (public)" ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template")))
	       )
        ("P" . "Project tasks")
        ("Pm" "My tasks (public)" ((agenda ""))
         ;; Remove those for which I'm not responsible
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template" "-I" "-C")))
	       )
        ("Pp" "Private tasks" ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+private")))
	       )
        ("Pa" "All tasks" ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-template")))
	       )
        ("R" . "Reminders")
        ("Rd" "Daily reminders"
	       ((agenda ""))
         ((org-agenda-tag-filter-preset '("+daily" "+reminder" "-private" "-template")))
	       )
        ("Ra" "All Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template")))
	       )
        ("Ro" "Other Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template" "-daily")))
	       )
        ("j" "All meetings"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+termin")))
	       )
	      )
      )


;;; Macros

;; Time-range: <time>--<time>
(fset 't-range
   (kmacro-lambda-form [?\C-u ?\C-c ?. return ?- ?- ?\C-u ?\C-c ?. return] 0 "%d"))


;;; Capture templates

;; Minutes of meeting 
(defun my/mom-template ()
  
  (setq title "* %u %?\n  %T--%T\n\n"
        people "** People [/]\n\n   - [ ] abc\n\n"
        minutes "** Minutes of meeting\n\n"
        actions "** Action Items\n\n*** TODO xyz\n\n")
  ;; return the template as a string
  (concat title people minutes actions))

(setq org-capture-templates
      '(
        ("m" "Minutes of meeting" entry (file "") (function my/mom-template))
        ))
