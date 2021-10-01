;;; Enable MELPA packages
;;; https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;;; Shortcut for neotree sidebar
;; (require 'neotree)
 (global-set-key [f8] 'neotree-toggle)


;; Projectile + Helm
(require 'helm-config)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)


;;; My own values for variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(desktop-save-mode t)
 '(electric-indent-mode nil)
 '(global-auto-complete-mode t)
 '(global-auto-highlight-symbol-mode t)
 '(indent-tabs-mode nil)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-include-node-externs t)
 '(ns-command-modifier (quote meta))
 '(show-paren-mode t)
 '(tab-width 2)
 '(word-wrap nil)
 '(helm-mode t)
 )

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(set-face-attribute 'default nil :height 140)

;; auto-load js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; hide toolbar
(ns-toggle-toolbar)

;; Show-hide
(global-set-key (kbd "C-c +") 'hs-show-block)
(global-set-key (kbd "C-c <") 'hs-show-all)
(global-set-key (kbd "C-c -") 'hs-hide-block)
(global-set-key (kbd "C-c >") 'hs-hide-all)

(global-visual-line-mode)

;; show right margin at 80 chars
(display-fill-column-indicator-mode)

;; JS stuff 
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; scrolling
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;; Ansible mode
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))


;; AUCTeX
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))


(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 5) ;; in seconds

;; Flyspell
;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "M-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f9>") 'flyspell-check-next-highlighted-word)

(when (eq system-type 'darwin)
  (message "Setting variables for Darwin")
  (custom-set-variables
   '(ns-command-modifier 'meta))
  ;; I wanted load my Org-Mode personal files only on this host
  ;; (when (string= (sytem-name) "pgpb-macbook13-2020.fritz.box")
  ;;   (message (system-name)))
  )

;; Org-Mode Options

(setq org-agenda-files (list "~/org"))

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; TODO State Configuration

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "WONTDO")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) 
        ("WONTDO" . (:foreground "blue" :weight bold))))

;; track progress history across emacs sessions

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-log-into-drawer t)


;; Save backups and temp files to a central location to avoid
;; certain tools (Grunt-watch) to show annoying ENOENT file because
;; I can't figure out a Glob pattern to exclude them from JSLint

;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name
;;                  (concat user-emacs-directory "backups")))))

;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name
;;                 (concat user-emacs-directory "auto-save")) t)))

;;; Init.el ends here


