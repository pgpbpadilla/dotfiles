(setq org-agenda-custom-commands 
      '(
        ("I" . "Improvements")
        ("Ib" "Backlog" tags "+improve-info/-DONE-INFO-WONTDO")
        ("Iw" "work in progress"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+improve")))
         )
        ;; Goal, Task, Action
        ("Z" . "Ziele")
        ("Zg" "Goals" tags "+goal-task-action")
        ("Zt" "Tasks" tags "+task-action")
        ("Za" "Actions" tags "+action")
        ("p" "Public tasks (public)"
         ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template")))
	       )
        ("P" . "Project tasks")
        ("Pm" "My tasks (public)"
         ((agenda ""))
         ;; Remove those for which I'm not responsible
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template" "-I" "-C")))
	       )
        ("Pp" "Private tasks"
         ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+private")))
	       )
        ("Pa" "All tasks"
         ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-template")))
	       )
        ("d" "Daily reminders"
         ((agenda ""))
         ((org-agenda-tag-filter-preset daily-reminder-tags))
         )
        ("R" . "Reminders")
        ("Rd" "Daily reminders"
         ((agenda ""))
         ((org-agenda-tag-filter-preset daily-reminder-tags))
	       )
        ("Ra" "All Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template")))
	       )
        ("Ro" "Other Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template" "-daily")))
	       )
        ("j" "All meetings"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+termin")))
	       )
        ("O" . "Out of Office")
        ("Om" "Monthly view"
         ((agenda ""))
	       ((org-agenda-span 'month)
          (org-agenda-tag-filter-preset '("+ooo")))
         )
        ("Oa" "All events" tags "+ooo/-INFO")
	      )
      )

(provide 'init-org-agenda)
