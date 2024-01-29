;;; WARNING: Do NOT edit this file by hand.
;; Instead edit the Org file and regenerate this
;; elisp script using: org-babel-tangle (C-c C-v t)

(require 'pgpb-org-agenda-commands)

(require 'pgpb-org-files)

(pgpb-refresh-org)

(setq org-agenda-span 'day)

;; Hide duplicate lines when SCHEDULED and DEADLINE overlap
;; https://superuser.com/a/530450/148349
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Log state changes
(setq org-log-done 'note)

;; Hide DONE items in the Agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(provide 'pgpb-org-agenda)
