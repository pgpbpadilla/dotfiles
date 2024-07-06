;;; https://github.com/microsoft/WSL/issues/6004
(defun fp/ignore-wsl-acls (orig-fun &rest args)
  "Ignore ACLs on WSL. WSL does not provide an ACL, but emacs
     expects there to be one before saving any file. Without this
     advice, files on WSL can not be saved."
  (if (string-match-p "^//wsl\$/" (car args))
      (progn (message "ignoring wsl acls") "")
    (apply orig-fun args)))
(advice-add 'file-acl :around 'fp/ignore-wsl-acls)

(message "Windows Subsystem for Linux options loaded.")

(provide 'pgpb-wsl)
