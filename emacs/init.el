;;; init.el begins here

;;; Enable MELPA packages
;;; https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Use-package
(eval-when-compile (require 'use-package))

;;; load-path
;;
(dolist (i '(
	     "~/.emacs.d/themes/"
             "~/dotfiles/emacs/"
	     ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;; Set location for customizations file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.config/emacs-custom.el")
(load custom-file)


;; load-relative
(unless (package-installed-p 'load-relative)
  (package-install 'load-relative))


;;; Helm stuff
(require 'init-helm)


;; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(require 'projectile)
(projectile-global-mode)
;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)


;;; Custom variables

;;; If the value is nil, then TAB indents the current line only if
;;; point is at the left margin or in the line’s indentation;
;;; otherwise, it inserts a tab character.
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html
(electric-indent-mode nil)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

(column-number-mode t)
(show-paren-mode t)
(global-visual-line-mode)
(display-line-numbers-mode)
(global-hl-line-mode 1)

;; show right margin at 80 chars
;; TODO: this should not be shown everywhere, only in those modes
;; where it makes sense. For an example look at how automatic new line
;; is configured for Org files.
;; (global-display-fill-column-indicator-mode t)

;;; Indent inserts spaces
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq 
 ansi-color-faces-vector '[default default default italic underline success warning error]
 ansi-color-names-vector '["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
 word-wrap 'nil
 ;; GPG must be installed already
 epg-gpg-program "gpg"
 )


;;; Spacemacs Theme
(setq custom-safe-themes t
      custom-enabled-themes '(spacemacs-light))
(load-theme 'spacemacs-light t)


;;; JavaScript Options
;; auto-load js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))


;; Ansible mode
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))


;;; AUCTeX
(when (eq system-type 'darwin)
  (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
  (setq exec-path (append exec-path '("/Library/TeX/texbin")))
  )
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


;;; Yasnippet
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))


;;; Auto-save
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'org-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 5) ;; in seconds


;;; Flyspell
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


;;; Encryption for Org-files
(require 'epa-file)
(epa-file-enable)


;; Save backups and temp files to a central location to avoid
;; certain tools (Grunt-watch) to show annoying ENOENT file because
;; I can't figure out a Glob pattern to exclude them from JSLint
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "auto-save")) t)))


;;; Load Windows customizations
(when (eq system-type 'windows-nt)
  (load-relative "./win10.el")
  (load-relative "./wsl.el")
  (message "Ok: windows hacks loaded.")
  )


;;; Darwin (MacOS) customizations
(when (eq system-type 'darwin)
  (load-relative "./darwin.el")
  (message "Ok: macos hacks loaded.")
  )


;;; Org Mode
(load-relative "./org.el")


;;; Global Shortcuts

;;; Shortcut for neotree sidebar
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;; Show-hide
(global-set-key (kbd "C-c +") 'hs-show-block)
(global-set-key (kbd "C-c <") 'hs-show-all)
(global-set-key (kbd "C-c -") 'hs-hide-block)
(global-set-key (kbd "C-c >") 'hs-hide-all)

;; Scrolling
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )


;;; Init.el ends here
