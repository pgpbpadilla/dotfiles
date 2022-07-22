(setq no-info "-info-journal/-INFO-DONE-WONTDO-IGNORE")

(setq org-agenda-custom-commands 
      '(
        ("I" . "Improvements")
        ("Ib" "Backlog" tags "+improve-info/-DONE-INFO-WONTDO")
        ("Iw" "work in progress"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+improve")))
         )
        ;;; Goals stuff
        ;; See Org etry: =C-c h goals task action=
        ;; file:~/org/cdc3c97dd4.org.gpg::#goal-task-action
        ("G" . "Goals, Tasks, Actions")

        ;;; Show all items for Specific goals
        ("Gp" "Peace of Mind" tags (concat "+action+pom" no-info))
        ("Gf" "Gesund bleiben" tags (concat "+action+fit" no-info))
        ("Gs" "Deustsche Sprache" tags (concat "+action+de+sprache" no-info))

        ;;; Unfiltered entries
        ("Gg" "Goals" tags (concat "+goal-task-action" no-info))
        ("Gt" "Tasks" tags (concat "+task-action" no-info))
        ("Ga" "Actions" tags (concat "+action" no-info))
        
        ;;; Goals without a Project can be called Dreams cause they'll
        ;; remain unaccomplished unless some work is done.
        ("Gd" "Dreams" tags "+dream")

        ;; Projects without Goals are Hobbies and there's nothing to
        ;; accomplish, most of them are just fun, but mabe having Fun
        ;; is the Goal.
        ("Gh" "Hobbies" tags "+hobby")

        ;; Accomplished goals are results
        ("Gr" "Results" tags "+goal-task-action-info/+DONE-INFO")

        ;; Dangling/Zombie items
        ("Gz" "Dangling/Zombie items" tags "-goal-hobby-dream-journal/-DONE-WONTDO-INFO-IGNORE")

        ("p" "Public tasks (public)"
         ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template")))
	       )

        ;;; Project stuff
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

        ;;; Reminders stuff
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
