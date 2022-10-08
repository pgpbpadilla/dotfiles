;;; init.el begins here

(load "~/dotfiles/emacs/init-packages.el")

(use-package markdown-mode
  :ensure t
  :config
  ;; Auto-insert new lines for long lines
  (add-hook 'markdown-mode-hook #'turn-on-auto-fill))

;;; load-path
(dolist (i '("~/.emacs.d/themes/" "~/dotfiles/emacs/"))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;; Set location for customizations file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.config/emacs-custom.el")
;; Purposely ignore all variables written to the =custom-file=
;; (load custom-file)

(setq default-input-method 'german-postfix)

;; Auto-dim other buffers
;; https://github.com/mina86/auto-dim-other-buffers.el/blob/master/README.md
(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode t)
  )


;;; Helm stuff
(require 'init-helm-projectile)


;;; Custom variables

;;; If the value is nil, then TAB indents the current line only if
;;; point is at the left margin or in the line’s indentation;
;;; otherwise, it inserts a tab character.
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html
(electric-indent-mode nil)
(electric-pair-mode t)

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
;;; If enabled everywhere, it breaks the Org Agenda view
;; (global-display-line-numbers-mode)
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
(setq real-auto-save-interval 15) ;; in seconds


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
  ;;; My system uses =lp= instead of =lpr=
  ;; https://stackoverflow.com/questions/28765952/emacs-printing-error-unknown-option-j
  (setq lpr-command "lp"
        lpr-add-switches nil)
  (message "Linux-specific options set")
  )

;;; Darwin (MacOS) customizations
(when (eq system-type 'darwin)
  (load-relative "./darwin.el")
  (message "Ok: macos hacks loaded.")
  )


;;; Org Mode
(require 'init-org)

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

(load-relative "./abbrev.el")

;;; Magit customisations
;; Open Magit and use Full Window instead of splitting the buffer
;; https://github.com/magit/magit/issues/1953#issuecomment-221134023
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)


;;; Track key frequencies
;; https://github.com/dacap/keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; Fill column options
(global-set-key (kbd "C-x M-f") 'fill-region)

;;; Init.el ends here
