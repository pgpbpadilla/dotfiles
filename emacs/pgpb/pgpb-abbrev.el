(setq abbrev-file-name (concat user-emacs-directory "/abbrev_defs.el"))
(setq save-abbrevs 'silent)
(setq-default abbrev-mode t)

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(provide 'pgpb-abbrev)
