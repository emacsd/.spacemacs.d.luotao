(defun bh-org/config ()
  (setq
   ;; http://doc.norang.ca/org-mode.html
   org-directory "~/Documents/org/"
   org-diary-file (concat org-directory "diary.org")

   org-agenda-directory "~/Documents/agenda/"
   org-agenda-files (list org-agenda-directory)
   org-default-notes-file (concat org-agenda-directory "refile.org")

   ;;;;; 5 Tasks and States

   ;; special markers ‘!’ (for a timestamp) or ‘@’ (for a note with timestamp)
   ;; The setting for WAIT is even more special: the ‘!’ after the slash means
   ;; that in addition to the note taken when entering the state, a timestamp
   ;; should be recorded when leaving the WAIT state, if and only if the target
   ;; state does not configure logging for entering it.
   ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))

   org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))

   ;; allows changing todo states with S-left and S-right skipping
   ;; all of the normal processing when entering or leaving a todo
   ;; state. This cycles through the todo states but skips setting
   ;; timestamps and entering notes which is very convenient when
   ;; all you want to do is fix up the status of an entry.
   org-treat-S-cursor-todo-selection-as-state-change nil

   org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

   ;;;;; 6 Adding New Tasks Quickly with Org Capture
   org-capture-templates
   '(("t" "todo" entry (file org-default-notes-file)
      "* TODO %?\n  %U\n  %a\n" :clock-in t :clock-resume t)
     ("r" "respond" entry (file org-default-notes-file)
      "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
     ("n" "note" entry (file org-default-notes-file)
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
     ("j" "Journal" entry (file+datetree org-diary-file)
      "* %?\n%U\n" :clock-in t :clock-resume t)
     ("w" "org-protocol" entry (file org-default-notes-file)
      "* TODO Review %c\n%U\n" :immediate-finish t)
     ("m" "Meeting" entry (file org-default-notes-file)
      "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
     ("p" "Phone call" entry (file org-default-notes-file)
      "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
     ("h" "Habit" entry (file org-default-notes-file)
      "* NEXT %?\n  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n  :PROPERTIES:\n  :STYLE: habit\n  :REPEAT_TO_STATE: NEXT\n  :END:\n  %a\n"))

   ;;;;; 7 Refiling Tasks
   ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
   org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9))

   ;; Use full outline paths for refile targets - we file directly with IDO
   org-refile-use-outline-path t

   ;; Targets complete directly with IDO
   org-outline-path-complete-in-steps nil

   ;; Allow refile to create parent tasks with confirmation
   org-refile-allow-creating-parent-nodes (quote confirm)

   ;; Use IDO for both buffer and file completion and ido-everywhere to t
   org-completion-use-ido t
   ido-everywhere t
   ido-max-directory-size 100000
   ;; (ido-mode (quote both))

   ;; Use the current window when visiting files and buffers with ido
   ido-default-file-method 'selected-window
   ido-default-buffer-method 'selected-window

   ;; Use the current window for indirect buffer display
   org-indirect-buffer-display 'current-window

   org-refile-target-verify-function 'bh/verify-refile-target

   ;;;;; 8 Custom agenda views
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil

   ;; Compact the block agenda view
   org-agenda-compact-blocks t

   ;; Custom agenda command definitions
   org-agenda-custom-commands
   '(("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
        '(todo-state-down effort-up category-keep))))

     ("w" "Weekly review"
      agenda ""
      ((org-agenda-span 'week)
       (org-agenda-start-on-weekday 0)
       (org-agenda-start-with-log-mode t)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'nottodo 'done))))

     (" " "Agenda"
      ((agenda "" nil)
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-HOLD-CANCELLED/!"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function 'bh/skip-non-projects)
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                             ""
                                                           " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                   (org-tags-match-list-sublevels t)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header (concat "Project Subtasks"
                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                             ""
                                                           " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-non-project-tasks)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                             ""
                                                           " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-project-tasks)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-CANCELLED+WAITING|HOLD/!"
                  ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                             ""
                                                           " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-non-tasks)
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
              (org-tags-match-list-sublevels nil))))
      nil))

   org-agenda-auto-exclude-function 'bh/org-auto-exclude-function

   ;; 9 Time Clocking
   ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
   org-clock-history-length 23

   ;; Resume clocking task on clock-in if the clock is open
   org-clock-in-resume t
   ;; Change tasks to NEXT when clocking in
   org-clock-in-switch-to-state 'bh/clock-in-to-next
   ;; Separate drawers for clocking and logs
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   ;; Save clock data and state changes and notes in the LOGBOOK drawer
   org-clock-into-drawer t
   ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   ;; Clock out when moving task to a done state
   org-clock-out-when-done t
   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
   org-clock-persist t
   ;; Do not prompt to resume an active clock
   org-clock-persist-query-resume nil
   ;; Enable auto clock resolution for finding open clocks
   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   ;; Include current clocking task in clock reports
   org-clock-report-include-clocking-task t

   bh/keep-clock-running nil

   org-time-stamp-rounding-minutes (quote (1 1))
   org-agenda-clock-consistency-checks
   '(:max-duration "4:00"
                   :min-duration 0
                   :max-gap 0
                   :gap-ok-around ("4:00"))

   ;;;;; 10 Time reporting and tracking
   ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   ;; Agenda clock report parameters
   org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
   ;; Set default column view headings: Task Effort Clock_Summary
   org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
   ;; global Effort estimate values
   ;; global STYLE property values for completion
   org-global-properties
   '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
     ("STYLE_ALL" . "habit"))
   ;; Agenda log mode items to display (closed and state changes by default)
   org-agenda-log-mode-items (quote (closed state))

   ;;;;; 11 Tags
   ;; Tags with fast selection keys
   org-tag-alist
   '((:startgroup)
     ("@errand" . ?e)
     ("@office" . ?o)
     ("@home" . ?H)
     ("@farm" . ?f)
     (:endgroup)
     ("WAITING" . ?w)
     ("HOLD" . ?h)
     ("PERSONAL" . ?P)
     ("WORK" . ?W)
     ("FARM" . ?F)
     ("ORG" . ?O)
     ("NORANG" . ?N)
     ("crypt" . ?E)
     ("NOTE" . ?n)
     ("CANCELLED" . ?c)
     ("FLAGGED" . ??))

   ;; Allow setting single tags without the menu
   org-fast-tag-selection-single-key 'expert
   ;; For tag searches ignore tasks with scheduled and deadline dates
   org-agenda-tags-todo-honor-ignore-options t

   ;;;;; TODO 13 Handling Phone Calls

   ;;;;; 14 GTD stuff
   org-agenda-span 'day

   ;; Projects are 'stuck' if they have no subtask with a NEXT
   ;; todo keyword task defined.
   org-stuck-projects '("" nil nil "")

   ;;;;; 15 Archiving
   org-archive-mark-done nil
   org-archive-location "%s_archive::* Archived Tasks"

   ;;;;; TODO 16 Publishing and Exporting

   ;;;;; 17 Reminders

   ;;;;; 18 Productivity Tools

   ;; TODO 18.1 Abbrev-mode and Skeletons

   ;; 18.2 Focus On Current Work
   org-show-entry-below '((default))
   ;; Limit restriction lock highlighting to the headline only
   org-agenda-restriction-lock-highlight-subtree nil
   ;; Keep tasks with dates on the global todo lists
   org-agenda-todo-ignore-with-date nil

   ;; Keep tasks with deadlines on the global todo lists
   org-agenda-todo-ignore-deadlines nil

   ;; Keep tasks with scheduled dates on the global todo lists
   org-agenda-todo-ignore-scheduled nil

   ;; Keep tasks with timestamps on the global todo lists
   org-agenda-todo-ignore-timestamp nil

   ;; Remove completed deadline tasks from the agenda view
   org-agenda-skip-deadline-if-done t

   ;; Remove completed scheduled tasks from the agenda view
   org-agenda-skip-scheduled-if-done t

   ;; Remove completed items from search results
   org-agenda-skip-timestamp-if-done t

   org-agenda-include-diary nil
   org-agenda-diary-file "~/git/org/diary.org"

   org-agenda-insert-diary-extract-time t


   ;; Include agenda archive files when searching for things
   org-agenda-text-search-extra-files (quote (agenda-archives))

   ;; Show all future entries for repeating tasks
   org-agenda-repeating-timestamp-show-all t

   ;; Show all agenda dates - even if they are empty
   org-agenda-show-all-dates t

   ;; Sorting order for tasks on the agenda
   org-agenda-sorting-strategy
   '((agenda habit-down time-up user-defined-up effort-up category-keep)
     (todo category-up effort-up)
     (tags category-up effort-up)
     (search category-up))

   ;; Start the weekly agenda on Monday
   org-agenda-start-on-weekday 1

   ;; FIXME Enable display of the time grid so we can see the marker for the current time
   ;; org-agenda-time-grid (quote ((daily today remove-match)
   ;;                              #("----------------" 0 16 (org-heading t))
   ;;                              (0900 1100 1300 1500 1700)))

   org-agenda-time-grid
   '((daily today remove-match)
     (800 1000 1200 1400 1600 1800 2000)
     "......"
     "----------------")

   ;; Display tags farther right
   org-agenda-tags-column -102

   ;;
   ;; Agenda sorting functions
   ;;
   org-agenda-cmp-user-defined 'bh/agenda-sort

   ;; Use sticky agenda's so they persist
   org-agenda-sticky t

   ;; 18.6 Handling blocked tasks
   org-enforce-todo-dependencies t

   org-hide-leading-stars nil

   org-startup-indented t

   org-cycle-separator-lines 0

   org-blank-before-new-entry
   '((heading)
     (plain-list-item . auto))

   org-insert-heading-respect-content nil

   org-reverse-note-order nil

   org-show-following-heading t
   org-show-hierarchy-above t
   org-show-siblings (quote ((default)))

   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-yank-adjusted-subtrees t

   org-id-method (quote uuidgen)

   org-deadline-warning-days 30

   org-table-export-default-format "orgtbl-to-csv"

   org-link-frame-setup
   '((vm . vm-visit-folder)
     (gnus . org-gnus-no-new-news)
     (file . find-file))

   ;; Use the current window for C-c ' source editing
   org-src-window-setup 'current-window

   org-log-done (quote time)
   org-log-into-drawer t
   org-log-state-notes-insert-after-drawers nil

   org-clock-sound "/usr/local/lib/tngchime.wav"

   org-habit-graph-column 50

   org-export-with-timestamps nil

   org-return-follows-link t

   org-read-date-prefer-future nil
   org-read-date-prefer-future 'time

   org-list-demote-modify-bullet
   '(("+" . "-")
     ("*" . "-")
     ("1." . "-")
     ("1)" . "-")
     ("A)" . "-")
     ("B)" . "-")
     ("a)" . "-")
     ("b)" . "-")
     ("A." . "-")
     ("B." . "-")
     ("a." . "-")
     ("b." . "-"))

   org-tags-match-list-sublevels t

   org-agenda-persistent-filter t

   org-link-mailto-program (quote (compose-mail "%a" "%s"))

   org-agenda-skip-additional-timestamps-same-entry t

   org-table-use-standard-references (quote from)

   org-agenda-window-setup 'current-window

   org-clone-delete-id t

   org-cycle-include-plain-lists t

   org-src-fontify-natively t

   ;; Startup in folded view
   org-startup-folded t

   ;; Allow alphabetical list entries
   org-alphabetical-lists t

   ;; 18.51 Preserving source block indentation
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0

   ;; Prevent editing invisible text
   org-catch-invisible-edits 'error

   ;; displays clock durations (from C-c C-x C-d in hours and minutes.
   org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)

   ;; Create unique IDs for tasks when linking
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   )

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
   'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
   'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
   'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
   'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
   'append)

  (add-hook
   'org-agenda-mode-hook
   '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
   'append)

  ;; Always hilight the current agenda line
  (add-hook
   'org-agenda-mode-hook
   '(lambda () (hl-line-mode 1))
   'append)

  (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
            'append)

  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (bh/org-agenda-to-appt)

  ;; Activate appointments so we get notifications
  (appt-activate t)

  ;; If we leave Emacs running overnight - reset the appointments one minute
  ;; after midnight
  (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

  )

;;;;; bh functions

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;;;;; 13 Handling Phone Calls
;; (require 'bbdb)
;; (require 'bbdb-com)

;; (global-set-key (kbd "<f9> p") 'bh/phone-call)

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
                                        ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

                                        ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

;;;;; 14 GTD stuff
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;;;;; 15 Archiving
(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


;;;;; 17 Reminders
;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))


;;;;;18.2 Focus On Current Work
(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ;; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ;; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ;; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ;; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ;; time specific items are already sorted first by org-agenda-sorting-strategy

     ;; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ;; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ;; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

     ;; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ;; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ;; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ;; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ;; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ;; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ;; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ;; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;; TODO 18.6 Handling blocked tasks

;; 18.21 Insert inactive timestamps and exclude from export
(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))
