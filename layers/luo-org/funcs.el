(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun eval-after-org-load ()
  (with-eval-after-load 'org
    (progn
      (luo-org-latex)

      (setq-default
       org-file-apps
       '((auto-mode . emacs)
         (directory . emacs)
         ("\\.mm\\'" . default)
         ("\\.x?html?\\'" . default)
         ("\\.pdf\\'" . system))

       org-log-done 'time
       org-log-into-drawer t
       org-log-state-notes-insert-after-drawers nil

       org-babel-load-languages
       '(
         (dot . t)
         (ditaa . t)
         (plantuml . t)
         )
       )

      (org-defkey org-mode-map "\C-c\C-j" 'org-open-at-point)

      (defvar org-build-directory (expand-file-name  "build" org-directory))
      (defadvice org-export-output-file-name (before org-add-export-dir activate)
        "Modifies org-export to place exported files in a different directory"
        (when (not pub-dir)
          (setq pub-dir (expand-file-name (substring extension 1) org-build-directory))
          (when (not (file-directory-p pub-dir))
            (make-directory pub-dir t))))
      ))
  )

(defun luo-org-latex ()
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
\\usepackage{xcolor}
\\definecolor{link}{HTML}{0366D6}
\\definecolor{lightgray}{rgb}{0.83, 0.83, 0.83}
\\definecolor{mintcream}{rgb}{0.96, 1.0, 0.98}

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

\\usepackage{minted}
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
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     )
   ;; minted required:
   ;; 1. xelatex -shell-escape
   ;; 2. pip install pygments
   ;; 3. pip install git+https://github.com/hg2c/terminal-pygments#egg=terminal-pygments

   org-latex-listings 'minted
   org-latex-logfiles-extensions
   '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
   org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   ))
