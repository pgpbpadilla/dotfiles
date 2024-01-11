(defun zsa-start ()
  "Focus on task and clock"
  (interactive)

  ;;  Disabled cause I often make the mistake of trying
  ;;  to start while I'm already within the body of the task.
  ;;  However this function was originally thought to be used
  ;;  in the context of the Agenda view or while searching for
  ;;  a heading with C-c h
  ;;  (funcall (lookup-key (current-local-map) (kbd "RET")))
  (org-narrow-to-subtree)
  (delete-other-windows)
  (org-clock-in)
)

(global-set-key (kbd "C-c u") 'zsa-start)

(provide 'pgpb-org-zsa)


