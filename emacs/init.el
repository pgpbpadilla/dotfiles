;;; load-path
(dolist (i '("~/.emacs.d/themes/" "~/dotfiles/emacs/"))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

(require 'pgpb)
(require 'weak-pinky)
