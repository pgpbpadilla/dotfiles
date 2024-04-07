(defun pgpb-ignore-custom-file ()
  ;; Set location for customizations file
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

  (setq custom-file "~/.config/emacs-custom.el")
  ;; Purposely ignore all variables written to the =custom-file=
  ;; (load custom-file)
  )

(defun pgpb-focus-mode ()
  "Disable all tool/menu bars"
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(defun pgpb-customize-appearance ()
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.

   ;; Fix magit word coloring, I need more contrast in the changed words
   ;; Changing the the background color to make the diffs easier to read
   ;; search: magit-diff-color
   '(diff-refine-added ((t (:inherit diff-refine-changed :background "yellow"))))
   '(diff-refine-removed ((t (:inherit diff-refine-changed :background "yellow")))))

  (setq 
   ansi-color-faces-vector '[default default default italic underline success warning error]
   ansi-color-names-vector '["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
   word-wrap 'nil)


  ;; Background colors
  ;; (add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
  (add-to-list 'default-frame-alist '(background-color . "#fffcf2"))
  (pgpb-focus-mode))

(defun pgpb-global-keys ()
  ;; Global Shortcuts

  ;; Show-hide
  (global-set-key (kbd "C-c +") 'hs-show-block)
  (global-set-key (kbd "C-c <") 'hs-show-all)
  (global-set-key (kbd "C-c -") 'hs-hide-block)
  (global-set-key (kbd "C-c >") 'hs-hide-all)

  ;; Scrolling
  (global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
  (global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

  ;; Window resize
  ;; unfortunately does not play well with Org mode buffers,
  ;; checkout: https://www.emacswiki.org/emacs/DoReMi
  ;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
  ;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  ;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
  ;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)


  ;; Fill column options
  (global-set-key (kbd "C-x M-f") 'fill-region))


(defun pgpb-magit ()
  ;; Magit customisations
  ;; Open Magit and use Full Window instead of splitting the buffer
  ;; https://github.com/magit/magit/issues/1953#issuecomment-221134023
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)


  ;; Color words in diff view <<magit-diff-color>>
  ;; https://emacs.stackexchange.com/a/52921/11978
  ;; https://magit.vc/manual/magit/Diff-Options.html
  (setq magit-diff-refine-hunk (quote all)))

(defun pgpb-flyspell ()
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
  (global-set-key (kbd "M-<f9>") 'flyspell-check-next-highlighted-word))


(defun pgpb-autosave ()
  ;;; Auto-save
  ;; Depends on:
  ;; 1. org-mode
  ;; 2. real-auto-save
  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'org-mode-hook 'real-auto-save-mode)

  (setq real-auto-save-interval 15) ;; in seconds

  ;; Save backups and temp files to a central location to avoid
  ;; certain tools (Grunt-watch) to show annoying ENOENT file because
  ;; I can't figure out a Glob pattern to exclude them from JSLint
  (setq backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups")))))

  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name
                  (concat user-emacs-directory "auto-save")) t)))

  )

(defun pgpb-latex-opts ()
  ;; LaTeX stuff
  (use-package tex
    :ensure auctex)
  (use-package auctex-latexmk
    :ensure t)

  ;;; AUCTeX
  (when (eq system-type 'darwin)
    (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
    (setq exec-path (append exec-path '("/Library/TeX/texbin")))
    )
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(defun pgpb-javascript ()
  ;;; JavaScript Options
  ;; auto-load js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(defun pgpb-customize-vars ()
  (setq default-input-method 'german-postfix)

  ;; If the value is nil, then TAB indents the current line only if
  ;; point is at the left margin or in the line’s indentation;
  ;; otherwise, it inserts a tab character.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html
  (electric-indent-mode nil)
  (electric-pair-mode t)

  (column-number-mode t)
  (show-paren-mode t)
  (global-visual-line-mode)

  ;; If enabled everywhere, it breaks the Org Agenda view
  ;; (global-display-line-numbers-mode)
  (global-hl-line-mode 1)

  ;; show right margin at 80 chars
  ;; TODO: this should not be shown everywhere, only in those modes
  ;; where it makes sense. For an example look at how automatic new line
  ;; is configured for Org files.
  ;; (global-display-fill-column-indicator-mode t)

  ;; Indent inserts spaces
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)


  ;; Track key frequencies
  ;; https://github.com/dacap/keyfreq
  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)

  ;;; Yasnippet
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        (list (expand-file-name "~/dotfiles/emacs/yasnippet/snippets")))
  )

(defun pgpb-auto-dim-buffers ()
  ;; Auto-dim other buffers
  ;; https://github.com/mina86/auto-dim-other-buffers.el/blob/master/README.md
  (use-package auto-dim-other-buffers
    :ensure t
    :config
    (auto-dim-other-buffers-mode t)
    ))

;; todo: move to package: pgpb-packages
(defun pgpb-install-packages ()
  (use-package markdown-mode
    :ensure t
    :config
    ;; Auto-insert new lines for long lines
    (add-hook 'markdown-mode-hook #'turn-on-auto-fill))


  ;; Emacs refactor: https://github.com/Wilfred/emacs-refactor
  (use-package emr
    :ensure t
    :bind ("M-RET" . emr-show-refactor-menu)
    )

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


  ;; Install command-log-mode
  (use-package command-log-mode
    :ensure t)

  ;; Shortcut for neotree sidebar
  (use-package neotree
    :ensure t)
  (global-set-key [f8] 'neotree-toggle)

  ;; ace-window
  ;; https://github.com/abo-abo/ace-window
  (use-package ace-window
    :ensure t
    :bind ("M-o" . ace-window))

  ;; ace-jump-mode
  ;; https://www.emacswiki.org/emacs/AceJump
  ;; https://github.com/winterTTr/ace-jump-mode
  ;; Mouse-less navigation
  (use-package ace-jump-mode
    :ensure t
    :bind ("C-c SPC" . ace-jump-mode)) 

  )

(provide 'pgpb-helpers)
