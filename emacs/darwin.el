;;; MacOS-specific options

(setq
 ;; Makes the Apple key act as the Meta key, as in =M-x=
 ns-command-modifier (quote meta)
 )

;; hide toolbar
(ns-toggle-toolbar)


;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 100))


;; Fix font size
(set-face-attribute 'default nil :height 145)
(message "Darwin options applied.")

