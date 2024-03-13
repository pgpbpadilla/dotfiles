;;; Custom Agenda commands
;; Examples: https://orgmode.org/manual/Matching-tags-and-properties.html
;; 
;; ‘work/WAITING’ = means => ‘work+TODO​="WAITING"’.
;; List all improvements that are not DONE.
(setq not-project-tags '("-improve" "-private" "-template" "-I" "-C"))
(setq daily-tags '("+daily" "-private" "-template"))

;;; Hide some tags in Agenda view so that there are no multi-line entries
(setq org-agenda-hide-tags-regexp
      (regexp-opt '("goal"
                    "task"
                    "action"
                    "hobby"
                    "PROJECT"
                    "reminder"
                    "recurring"
                    "time"
                    "info"
                    "journal"
                    "track"
                    "termin"
                    "bookmark")
                  )
      )

(setq hidden "-info-journal-reminder-hobby-dream/-INFO-DONE-WONTDO-IGNORE")

(setq org-agenda-custom-commands 
      '(
        ("f" "Time tracking" tags (concat "+time+track" hidden))

        ("d" "Daily reminders"
         ((agenda ""))
         ((org-agenda-tag-filter-preset daily-tags))
         )

        ;; Filter by priority
        ;; https://emacs-orgmode.gnu.narkive.com/bsur8gfw/orgmode-custom-agenda-view-filter-by-priority-and-scheduled-date
        ("n" "Now! a.k.a. Prio=A"
         ((agenda ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\[#A\\]")))
         )

        ("p" "Public tasks (public)"
         ((agenda ""))
	       ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template")))
	       )

        ("j" "All meetings"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+termin" "-template")))
	       )


        ("F" . "Find/Search")
        ("Fj" "Journal search" search ""
         ((org-agenda-files (my/org-journal-files))))
        ("Fa" "Archive search" search ""
         ((org-agenda-files (my/org-archive-files))))
        ("Fe" "Search everywhere" search ""
         ((org-agenda-text-search-extra-files (my/org-extra-files))))
        

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

        
        ;;; Goals stuff
        ;; See Org etry: =C-c h goals task action=
        ;; file:~/org/cdc3c97dd4.org.gpg::#goal-task-action
        ("G" . "Goals, Tasks, Actions")

        ;;; Show all items for Specific goals
        ("Gp" "Peace of Mind" tags (concat "+action+pom" hidden))
        ("Gf" "Gesund bleiben" tags (concat "+action+fit" hidden))
        ("Gs" "Deustsche Sprache" tags (concat "+action+de+sprache" hidden))

        ;;; Unfiltered entries
        ("Gg" "Goals" tags (concat "+goal-task-action" hidden))
        ("Gt" "Tasks" tags (concat "+task-action" hidden))
        ("Ga" "Actions" tags (concat "+action" hidden))
        
        ;;; Goals without a Project can be called Dreams cause they'll
        ;; remain unaccomplished unless some work is done.
        ("Gd" "Dreams" tags "+dream")

        ;; Projects without Goals are Hobbies and there's nothing to
        ;; accomplish, most of them are just fun, but mabe having Fun
        ;; is the Goal.
        ("Gh" "Hobbies" tags (concat "+hobby-task-action" hidden))

        ;; Accomplished goals are results
        ("Gr" "Results" tags "+goal-task-action-info/+DONE-INFO")

        ;; Dangling/Zombie items
        ("Gz" "Dangling/Zombie items" tags (concat "-goal" hidden))


        ("R" . "Reminders")
        ("Rd" "Daily reminders"
         ((agenda ""))
         ((org-agenda-tag-filter-preset daily-tags))
	       )
        ("Ra" "All Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template")))
	       )
        ("Ro" "Other Reminders"
	       ((agenda ""))
	       ((org-agenda-tag-filter-preset '("+reminder" "-template" "-daily")))
	       )


        ("O" . "Out of Office")
        ("Om" "Monthly view"
         ((agenda ""))
	       ((org-agenda-span 'month)
          (org-agenda-tag-filter-preset '("+ooo")))
         )
        ("Oa" "All events" tags "+ooo/-INFO")

        
        ("I" . "Improvements")
        ("Ib" "Backlog" tags "+improve-info/-DONE-INFO-WONTDO")
        ("Iw" "work in progress"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+improve")))
         )        
	      )
      )

(provide 'pgpb-org-agenda-commands)
