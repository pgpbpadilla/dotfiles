(ert-deftest pgpb-test-org-dirs ()
  (let ((expected '(pgpb-org-journal-dir
                    pgpb-org-archive-dir
                    pgpb-org-agenda-dir)))
    (should (equal (pgpb-org-dirs) expected))))

