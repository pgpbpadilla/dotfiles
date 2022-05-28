(setq no-info "/-INFO")

(setq org-agenda-custom-commands 
      '(
        ("I" . "Improvements")
        ("Ib" "Backlog" tags "+improve-info/-DONE-INFO-WONTDO")
        ("Iw" "work in progress"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+improve")))
         )
        ;; Goal, Task, Action
        ;; These terminology is kind of arbitrary, the general idea
        ;; is to break down big chunks of work into smaller more
        ;; manageable chunks.
        ;; Goals tend to be more about what is desired and why.
        ;; Tasks are the first attempt to break down a goal into
        ;; smaller pieces.
        ;; Actions are yet on level to identify small chunks of
        ;; work and in principle a sequence of actions should be
        ;; enough to accomplish a task.
        ("G" . "Goals, Tasks, Actions")
        ("Gg" "Goals" tags (concat "+goal-task-action" no-info))
        ("Gt" "Tasks" tags (concat "+task-action" no-info))
        ("Ga" "Actions" tags (concat "+action" no-info))
        ;; Goals without a Project can be called Dreams cause they'll
        ;; remain unaccomplished unless some work is done.
        ("Gd" "Dreams" tags "+dream")
        ;; Projects without Goals are Hobbies and there's nothing to
        ;; accomplish, most of them are just fun, but mabe having Fun
        ;; is the Goal.
        ("Gh" "Hobbies" tags "+hobby")
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
