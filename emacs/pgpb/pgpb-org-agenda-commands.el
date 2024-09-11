(setq daily-tags '("+daily" "-private" "-template"))

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
                    "bookmark")))

(setq hidden "-info-journal-reminder-hobby-dream/-INFO-DONE-WONTDO-IGNORE")

(setq org-agenda-custom-commands 
      '(
        ;;; Project stuff
        ("k" "Public tasks"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("-daily" "-improve" "-private" "-template")))
         )

        ("l" "All tasks"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("-daily" "-improve" "-template")))
         )

        ("d" "Daily reminders"
         ((agenda ""))
         ((org-agenda-tag-filter-preset daily-tags))
         )

        ("f" "Time tracking" tags (concat "+time+track" hidden))

        ;; Filter by priority
        ;; https://emacs-orgmode.gnu.narkive.com/bsur8gfw/orgmode-custom-agenda-view-filter-by-priority-and-scheduled-date
        ("n" "Now! a.k.a. Prio=A"
         ((agenda ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\[#A\\]")))
         )

        ("j" "All meetings"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+termin" "-template")))
         )


        ("F" . "Find/Search")
        ("Fj" "Journal search" search ""
         ((org-agenda-files (pgpb-org-journal-files))))
        ("Fa" "Archive search" search ""
         ((org-agenda-files (pgpb-org-archive-files))))
        ("Fe" "Search everywhere" search ""
         ((org-agenda-text-search-extra-files (pgpb-org-extra-files))))


        ;;; Goals stuff
        ;; See Org etry: =C-c h goals task action=
        ;; file:~/org/cdc3c97dd4.org.gpg::#goal-task-action
        ("G" . "Goals, Tasks, Actions")

        ;;; Show all items for Specific goals
        ("Gp" "Peace of Mind" tags (concat "+action+pom" hidden))
        ("Gf" "Gesund bleiben" tags (concat "+action+fit" hidden))
        ("Gs" "Deustsche Sprache" tags (concat "+action+de+sprache" hidden))

        ;;; Unfiltered entries
        ("Gg" "Has tag: =goal=" tags (concat "+goal" hidden))

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
        ;; Items that are not associated to any goal/hobby
        ;; (org-id-goto "A753CCF1-D6DF-47CF-BF16-067BD937E30A")
        ("Gz" "Dangling/Zombie items" tags (concat "-pom-fit-cn-de-blog-meta-job-hobby" hidden))


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
