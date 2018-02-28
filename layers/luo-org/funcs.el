(defun luo-org/config-defaults ()
  (setq-default
   org-directory "~/Documents/org/"

   org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . system))

   org-babel-load-languages
   '(
     (dot . t)
     (ditaa . t)
     (plantuml . t)
     )))

(defun luo-org/config-agenda ()
  (setq-default
   org-default-notes-file (concat org-directory "TODO.org")
   org-agenda-files (list (concat org-directory "TODO.org"))

   org-todo-keywords
   '((sequence "TODO(t)" "READY(n)" "RUNNING(r)" "BLOCKED(b)" "DELEGATED(p)" "|" "DONE(d)" "CANCELED(c)")
     (sequence "❢(t)" "⚑(n)" "☯(r)" "⧖(b)" "☺(p)" "|" "✔(d)" "✘(c)"))

   org-log-done 'time
   org-log-into-drawer t
   org-log-state-notes-insert-after-drawers nil

   ;; Change task state to STARTED when clocking in
   org-clock-in-switch-to-state "RUNNING"
   ;; Save clock data and notes in the LOGBOOK drawer
   org-clock-into-drawer t
   ;; Removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t

   org-capture-templates
   '(("d" "Do it" entry (file org-default-notes-file)
      "* RUNNING %?\n  %i\n  %a\n" :clock-in t :clock-keep t)
     ("n" "Next Task" entry (file org-default-notes-file)
      "* READY %?\n  DEADLINE: %t\n  %i\n  %a\n")
     ("t" "Todo" entry (file+headline org-default-notes-file "INBOX")
      "** TODO %?\n   %i\n  %a\n")
     )

   org-agenda-inhibit-startup t
   org-agenda-span 'day
   org-agenda-use-tag-inheritance nil
   org-agenda-window-setup 'current-window
   org-log-done t

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
                          nil))))))

(defun luo-org/config-latex ()
  (setq-default
   org-latex-classes
   '(("article"
      "
%!TEX TS-program = xelatex
%!TEX encoding = UTF-8 Unicode

\\documentclass[12pt,a4paper]{article}
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage[table]{xcolor}
\\definecolor{link}{HTML}{0366D6}
\\definecolor{lightgray}{rgb}{0.83, 0.83, 0.83}
\\definecolor{mintcream}{rgb}{0.96, 1.0, 0.98}
\\rowcolors{3}{lightgray!30}{white}

\\usepackage{fontspec}
\\newfontfamily\\zhfont[BoldFont=PingFang SC]{PingFang SC}
\\newfontfamily\\zhpunctfont{PingFang SC}
\\setmainfont{Times New Roman}
\\setsansfont{Helvetica/Arial}
\\setmonofont{Courier New}
\\usepackage{zhspacing}
\\zhspacing
\\usepackage{indentfirst}

\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true,
  linkcolor=link,
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=link,
  urlcolor=link,
  pagebackref=true,
  linktoc=all,
}

\\usepackage[outputdir=./build/tex]{minted}
\\setminted{
  frame=leftline,
  bgcolor=mintcream,
  fontsize=\\scriptsize,
  tabsize=2,
  breaklines,
  framesep=2mm,
  baselinestretch=1.2,
}
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   ;; minted required:
   ;; 1. xelatex -shell-escape
   ;; 2. pip install pygments
   ;; 3. pip install git+https://github.com/hg2c/terminal-pygments#egg=terminal-pygments
   org-latex-listings 'minted

   org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   )
  (defvar org-build-directory (expand-file-name  "build" org-directory))
  (defadvice org-export-output-file-name (before org-add-export-dir activate)
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir (expand-file-name (substring extension 1) org-build-directory))
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir t))))

  ;; if you want to highlight ipython block, you can add the following to your file:
  ;; (add-to-list 'org-latex-minted-langs '(ipython "python"))
  )
