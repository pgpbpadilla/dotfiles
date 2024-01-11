;; https://emacs.stackexchange.com/a/56067/11978
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")

;;; Install use-package
(require 'package)
;;; Enable MELPA packages
;;; https://melpa.org/#/getting-started
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;; list the packages you want
(setq package-list '(use-package
                      keyfreq
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
                      helm-org
                      markdown-preview-eww
                      markdown-preview-mode
                      markdown-mode))
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

(provide 'pgpb-packages)
