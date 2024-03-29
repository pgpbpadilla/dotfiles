;;; MacOS-specific options

(setq
 ;; Makes the Apple key act as the Meta key, as in =M-x=
 ns-command-modifier (quote meta)
 )

;;; hide toolbar
;;; It seems that this call is not necessary when we setup
;;; Focus mode as in:
;;; (org-id-goto "45184FF6-2699-41FF-BD47-2665701D466A")
;; (ns-toggle-toolbar)


;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 100))


;; Fix font size
(set-face-attribute 'default nil :height 140)
(message "Darwin options applied.")

