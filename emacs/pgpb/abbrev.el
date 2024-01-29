;;; Abbreviations for Emacs
;; https://www.emacswiki.org/emacs/AbbrevMode
(setq abbrev-file-name "~/dotfiles/emacs/abbrev_defs.el")
(setq save-abbrevs 'silent)
(setq-default abbrev-mode t)

;;; make all abbrev tables case-sensitive
(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))
