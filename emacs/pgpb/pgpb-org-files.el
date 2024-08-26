;;; WARNING: Do NOT edit this file by hand.
;; Instead edit the Org file and regenerate this
;; elisp script using: org-babel-tangle (C-c C-v t)

(defvar pgpb-org-file-extension ".org.gpg"
  "File extension for encrypted Org files.")

(defvar pgpb-org-file-regex (concat "\\" pgpb-org-file-extension "$")
  "Regex to search entrypted Org files.")

(defun pgpb-org-agenda-files ()
  (directory-files-recursively pgpb-org-agenda-dir pgpb-org-file-regex))

(defun pgpb-org-journal-files ()
  (directory-files-recursively pgpb-org-journal-dir pgpb-org-file-regex))

(defun pgpb-org-archive-files ()
  (directory-files-recursively pgpb-org-archive-dir pgpb-org-file-regex))

(defun pgpb-org-new ()
  "Create new Org file. Use my GPG keys for encryption."
  (interactive)

(let options (pgpb-org-new-options))
(let selected (completing-read "Choose dir: " options nil t))
(let out-dir (cdr (assoc selected options)))
(let random-filename (format "%s/%s" out-dir (random-name)))
(let new-file (concat random-filename pgpb-org-file-extension))

;; fix: get rid of the EPA key selection dialog
;; https://superuser.com/a/1446730/148349
(setq-local epa-file-encrypt-to my-gpg-key)
(write-region gpg-header nil new-file)
(find-file-other-window new-file)
(message new-file))

(defun pgpb-org-new-options ()
  "Return a list of options from a list of symbols"
  (interactive)

  (mapcar (lambda (symbol)
            (cons
             (symbol-name symbol)
             (symbol-value symbol))
            )
          (pgpb-org-dirs))
  )

(defun pgpb-org-dirs ()
  (interactive)

  (let ((org-dirs
         (cl-loop for symbol being the symbols
                  when (and (boundp symbol)
                            (string-match-p "^pgpb-org-.*-dir$" (symbol-name symbol)))
                  collect symbol)))

    (if (called-interactively-p 'any)
        (if org-dirs
            (with-output-to-temp-buffer "*pgpb-org-dirs*"
              (dolist (var org-dirs)
                (princ (format "%s\n" var))))
          (message "No variables found that match the pattern")
          )
      org-dirs)
    )
  )

(defun random-name ()
  "Return a random file name."
  (interactive)

  (require 'subr-x)
  (setq random-name
        (string-trim
         ;; todo: replace with pure-elisp function
         (shell-command-to-string
          "echo $(openssl rand -hex 5)"))))

(defvar pgpb-gpg-key "pgpb.padilla@gmail.com"
  "GPG key to use for encrypting Org files.")

(defvar pgpb-org-header
  (format "# -*- mode:org; epa-file-encrypt-to: (\"%s\") -*-" pgpb-gpg-key)
  "Emacs header to configure GPG encryption.")

(defun reload-emacs-configuration ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file user-init-file))

;; Bind the function to a key (optional)
(global-set-key (kbd "C-c r") 'reload-emacs-configuration)

(defun pgpb-refresh-org () 
  "Reload agenda files, usually to include newly created files."
  (interactive)
  (setq org-agenda-files (pgpb-org-agenda-files))
  (pgpb-refresh-refile-targets)
  (message "All Org agenda files have been reloaded."))

(defun pgpb-refresh-refile-targets ()
  (setq org-refile-targets
        '(
          (nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)
          (pgpb-org-journal-files :maxlevel . 3)
          (pgpb-org-archive-files :maxlevel . 3)
          )
        ))

(defun pgpb-org-refile ()
  ;; (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

(defun pgpb-extra-files ()
  (append
   (pgpb-journal-files)
   (pgpb-archive-files))
  )

(provide 'pgpb-org-files)
