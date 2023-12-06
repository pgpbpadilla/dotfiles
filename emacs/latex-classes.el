;; https://superuser.com/questions/896741/how-do-i-configure-org-latex-classes-in-emacs
;; quote:
;; >> This adjusts the variable as soon as the ox-latex package is loaded.
;;
;; =org-latex-classes= is nil on Emacs startup before Org is loaded,
;; hence we need to wait until Org is loaded so that this variable has a non-nil value.
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
             '("letter" "\\documentclass{letter}")))



;;; Export as an Emacs package
(provide 'latex-classes)
