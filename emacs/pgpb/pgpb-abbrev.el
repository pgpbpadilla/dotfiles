(setq pgpb-abbrevs-file (concat
                         user-emacs-directory
                         "pgpb/abbrev_defs.el"))

(assert-file-exists pgpb-abbrevs-file)

(setq abbrev-file-name pgpb-abbrevs-file)
(setq save-abbrevs 'silent)
(setq-default abbrev-mode t)

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(provide 'pgpb-abbrev)
