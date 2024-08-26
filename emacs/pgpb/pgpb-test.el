(defun pgpb-test ()
  "Eval buffer and run ERT tests"
  (interactive)

  (ert-delete-all-tests)
  (eval-buffer)
  (ert-run-tests-interactively t)
  )

(global-set-key (kbd "C-c t") 'pgpb-test)

(provide 'pgpb-test)
