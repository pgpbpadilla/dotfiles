;;; MacOS-specific options

(setq
 ;; Makes the Apple key act as the Meta key, as in =M-x=
 ns-command-modifier (quote meta)
 )

;; hide toolbar
(ns-toggle-toolbar)


;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))


;; Fix font size
(set-face-attribute 'default nil :height 130)
(message "Darwin options applied.")


;;; MacOS: pinentry, gpg
(when (eq system-type 'darwin)
  ;; Configure EasyPG to use loopback for pinentry
  (unless (package-installed-p 'pinentry)
    (package-install 'pinentry))
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
