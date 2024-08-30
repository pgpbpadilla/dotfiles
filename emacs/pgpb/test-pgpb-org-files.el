(mapc (lambda (file)
        (load (expand-file-name (concat "pgpb/" file) user-emacs-directory)))
      '("pgpb-test.el" "pgpb-org-files.el"))

(load (concat user-emacs-directory "early-init.el"))

(ert-deftest pgpb-test-org-dirs ()
  "should return a list of my Org directories"
  (let ((expected '(pgpb-org-journal-dir
                    pgpb-org-archive-dir
                    pgpb-org-agenda-dir)))
    (should (equal (pgpb-org-dirs) expected))))
