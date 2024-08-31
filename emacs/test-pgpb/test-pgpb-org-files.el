(mapc (lambda (file)
        (load (expand-file-name (concat "pgpb/" file) user-emacs-directory)))
      '("pgpb-test.el" "pgpb-org-files.el"))

(load (concat user-emacs-directory "early-init.el"))

(ert-deftest test-pgpb-org-dirs ()
  "should return a list of my Org directories"
  (let ((expected '(pgpb-org-journal-dir
                    pgpb-org-archive-dir
                    pgpb-org-agenda-dir)))
    (should (equal (pgpb-org-dirs) expected))))

(ert-deftest test-pgpb-org-new-options ()
  "should return the list of Org dirs as options"
  (should-not (null (pgpb-org-new-options))))

(ert-deftest test-pgpb-org-new ()
  "should not return nil"
  :expected-result :failed
  (let ((noninteractive t)
        (inhibit-message t)
        (completing-read-response pgpb-journal-dir))
    (flet ((completing-read (prompt choices &optional
                                    predicate require-match
                                    initial-input hist
                                    def inherit-input-method)
                            completing-read-response))
      (should-not (null (pgpb-org-new))))))
