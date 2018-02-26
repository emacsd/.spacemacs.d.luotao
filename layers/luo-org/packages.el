;;; packages.el --- luo-org layer packages file for Spacemacs.

;;; Code:

(defconst luo-org-packages
  '(
    org
    ))

(defun luo-org/post-init-org ()
  (setq-default
   org-directory "~/Documents/org/"
   org-default-notes-file (concat org-directory "TODO.org")
   org-agenda-files (list (concat org-directory "TODO.org"))

   org-todo-keywords
   '((sequence "TODO(t)" "READY(n)" "RUNNING(r)" "BLOCKED(b)" "DELEGATED(p)" "|" "DONE(d)" "CANCELED(c)")
     (sequence "❢(t)" "⚑(n)" "☯(r)" "⧖(b)" "☺(p)" "|" "✔(d)" "✘(c)"))

   org-capture-templates
   '(("t" "Todo" entry (file org-default-notes-file)
      "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
     ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
      "** READY %? \nDEADLINE: %t"))

   org-agenda-custom-commands
   '(
     ("d" "Daily agenda and all TODOs"
      ((todo "DELEGATED"
             ((org-agenda-overriding-header "--托付他人的事情：--------------------------------------------------------------")))

       (todo "RUNNING"
             ((org-agenda-overriding-header "--正在处理的事情：--------------------------------------------------------------")))
       (todo "BLOCKED"
             ((org-agenda-overriding-header "--------------------------------------------------------------------------------")))

       (todo "READY"
             ((org-agenda-overriding-header "--任务列表：--------------------------------------------------------------------")
              (org-agenda-sorting-strategy '(priority-down category-keep))))
       (todo "TODO"
             ((org-agenda-overriding-header "--------------------------------------------------------------------------------")
              (org-agenda-sorting-strategy '(priority-down category-keep)))))
      ((org-agenda-compact-blocks t)))

     ("w" "Weekly review"
      agenda ""
      ((org-agenda-span 'week)
       (org-agenda-start-on-weekday 0)
       (org-agenda-start-with-log-mode t)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'nottodo 'done))
       ))

     ("m" "Monthly review"
      agenda ""
      ((org-agenda-span 'month)
       (org-agenda-start-day "-15d")
       (org-agenda-start-with-log-mode t)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'nottodo 'done))
       ))))

  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "❢")
                          nil)))
               ("^\\*+ \\(RUNNING\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "☯")
                          nil)))
               ("^\\*+ \\(READY\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                          nil)))
               ("^\\*+ \\(BLOCKED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⧖")
                          nil)))
               ("^\\*+ \\(DELEGATED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "☺")
                          nil)))
               ("^\\*+ \\(DONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))
               ("^\\*+ \\(CANCELED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                          nil)))))

  (eval-after-org-load))

;;; packages.el ends here
