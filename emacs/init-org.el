(require 'org-utils)

;;; Encryption for Org-files
(require 'epa-file)
(epa-file-enable)
(require 'epg)
(setq epg-gpg-program "gpg"
      epg-pinentry-mode 'loopback)


;;; Adapt indentation to heading level
;; https://orgmode.org/manual/Org-Indent-Mode.html
(setq org-adapt-indentation t)
;; Disable indent-mode since it disables =org-adapt-indentation=
(setq org-startup-indented nil)

;;; Hide drawers on startup
;; https://orgmode.org/manual/Initial-visibility.html
(setq org-startup-folded 'overview)


;;; Configure Agenda
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
        ("INFO" . (:foreground "blue" :weight bold))
        )
      )

;;; track progress history across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;; Always create ID for links
;; https://orgmode.org/manual/Handling-Links.html#FOOT29
(require 'org-id)
;; https://emacs.stackexchange.com/a/64240/11978
(setq org-id-link-to-org-use-id t)

;;; Shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c i") 'org-insert-last-stored-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; All Notes captured will go to this file
(setq org-default-notes-file (concat org-directory "/notes.org.gpg"))

;;; Define refile targets
(my/org-refile-targets)

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
        "-template-info-notes-improve+PROJECT/-MAYBE-DONE-WONTDO-INFO-IGNORE" 
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
                               (latex . t)
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
(setq org-export-backends '(ascii html icalendar latex beamer md odt))


(require 'init-org-agenda)


;;; Macros

;; Time-range: <time>--<time>
(fset 't-range
   (kmacro-lambda-form [?\C-u ?\C-c ?. return ?- ?- ?\C-u ?\C-c ?. return] 0 "%d"))


;;; Capture templates

;; Minutes of meeting 
(defun my/mom-template ()
  
  (setq title "* %?\t:termin:mom:\n  %T\n\n"
        people "** People \n\n   1. xyz\n\n"
        minutes "** Minutes of meeting\n\n"
        actions "** Action Items [/]\n\n*** TODO xyz\n\n")
  ;; return the template as a string
  (concat title people minutes actions))

(setq org-capture-templates
      '(
        ("m" "Minutes of meeting" entry (file "") (function my/mom-template))
        ("j" "Journal entry" entry (file "") "* %?\n %U")
        ))


;;; Load Online images
;; https://emacs.stackexchange.com/questions/42281/org-mode-is-it-possible-to-display-online-images
;; example: [[imghttp://tn-home.de/Pic/tn-home.png]]
;; To show images use: =C-c C-x C-v=
  
;; Requires cloning: https://github.com/TobiasZawada/org-yt
(load-relative "./org-yt/org-yt.el")
(defun org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (cl-assert (string-match "\\`img" protocol) nil
             "Expected protocol type starting with img")
  (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
    (cl-assert buf nil
               "Download of image \"%s\" failed." link)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (buffer-substring-no-properties (point) (point-max)))))

(org-link-set-parameters
 "imghttp"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "imghttps"
 :image-data-fun #'org-image-link)

;;; https://www.orgroam.com/
;;; (use-package org-roam
;;;  :ensure t)


;;; Set colors for headings
;; DONE items are hard to read, I'd like them more prominent
;; Alternatives:
;; - SpringGreen4
;; - MidnightBlue
(set-face-attribute 'org-headline-done nil :foreground "MidnightBlue")

;;; Handle custom protocols

(when (eq system-type 'gnu/linux)
  (message "Inside a Linux system")
  ;;; Should open Gnucash links with Gnucash app
  (org-add-link-type "gnucash" (lambda (path) (browse-url-xdg-open path)))
  ;;; Should open Gnucash links with PDF viewer
  (org-add-link-type "pdf" (lambda (path) (browse-url-xdg-open path)))
  ;;; Export PDF with Latex on Linux
  (setq org-latex-pdf-process '("latexmk -pdf -interaction=nonstopmode -output-directory=%o %f"))

  )

(when (eq system-type 'darwin)
  (message "Inside a Darwin system")
  (org-add-link-type "gnucash" (lambda (path)
                                 (shell-command (concat "open " path))))
  )



;;; Org Slides
;; https://github.com/takaxp/org-tree-slide
(use-package org-tree-slide
  :ensure t
  :custom
  (org-image-actual-width nil))
(global-set-key (kbd "C-c s") 'org-tree-slide-mode)


;;; Additional places to use for indexing Org IDs
(setq org-id-extra-files (my/org-extra-files))


;;; Export as an Emacs package
(provide 'init-org)
