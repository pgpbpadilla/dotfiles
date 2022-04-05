;;; init.el begins here

;; https://emacs.stackexchange.com/a/56067/11978
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")

;;; Install use-package
(require 'package)
;;; Enable MELPA packages
;;; https://melpa.org/#/getting-started
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;; list the packages you want
(setq package-list '(use-package
                      command-log-mode
                      neotree
                      auto-highlight-symbol
                      auto-complete
                      auto-dim-other-buffers
                      use-package
                      yasnippet
                      visual-fill
                      spacemacs-theme
                      real-auto-save
                      org-projectile
                      org-bullets
                      magit
                      load-relative
                      imenu-anywhere
                      helm-projectile
                      helm-org))
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
      


;;; Use-package
(eval-when-compile (require 'use-package))

;;; load-path
(dolist (i '(
	           "~/.emacs.d/themes/"
             "~/dotfiles/emacs/"
	           ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;; Set location for customizations file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.config/emacs-custom.el")
;; Purposely ignore all variables written to the =custom-file=
;; (load custom-file)


;; Auto-dim other buffers
;; https://github.com/mina86/auto-dim-other-buffers.el/blob/master/README.md
(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode t)
  )

;; load-relative
(unless (package-installed-p 'load-relative)
  (package-install 'load-relative))


;;; Helm stuff
(require 'init-helm)

;; Projectile
;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  )

;;; helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package imenu-anywhere
  :ensure t)


;;; Custom variables

;;; If the value is nil, then TAB indents the current line only if
;;; point is at the left margin or in the line’s indentation;
;;; otherwise, it inserts a tab character.
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html
(electric-indent-mode nil)

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t)
  )

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t)
  )

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
 word-wrap 'nil)


;;; Spacemacs Theme
;;(setq custom-safe-themes t
;;      custom-enabled-themes '(spacemacs-light))
;;(load-theme 'spacemacs-light t)

;;; Background colors
;; (add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "#fffcf2"))


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

;;; Encryption for Org-files
(require 'epa-file)
(epa-file-enable)
(require 'epg)
(setq epg-gpg-program "gpg"
      epg-pinentry-mode 'loopback)

(when (eq system-type 'gnu/linux)
  (message "Inside a Linux system")
  (use-package pinentry
    :ensure t
    :config
    (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
    (pinentry-start)
    )
  (message "Linux-specific options set")
  )

;;; Darwin (MacOS) customizations
(when (eq system-type 'darwin)
  (load-relative "./darwin.el")
  (message "Ok: macos hacks loaded.")
  )


;;; Org Mode
(load-relative "./org.el")

;; Install command-log-mode
(use-package command-log-mode
  :ensure t)

;;; Global Shortcuts

;;; Shortcut for neotree sidebar
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

;;; Show-hide
(global-set-key (kbd "C-c +") 'hs-show-block)
(global-set-key (kbd "C-c <") 'hs-show-all)
(global-set-key (kbd "C-c -") 'hs-hide-block)
(global-set-key (kbd "C-c >") 'hs-hide-all)

;; Scrolling
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;;; Window resize
;; unfortunately does not play well with Org mode buffers,
;; checkout: https://www.emacswiki.org/emacs/DoReMi
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; Init.el ends here
