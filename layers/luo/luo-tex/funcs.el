(defun luo-tex/config ()
  ;; minted required:
  ;; 1. xelatex -shell-escape
  ;; 2. pip install pygments
  ;; 3. pip install git+https://github.com/hg2c/terminal-pygments#egg=terminal-pygments
  (setq org-latex-listings 'minted)

  (setq
   org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-directory "~/Documents/org/")
  (defvar org-build-directory (expand-file-name  "build" org-directory))
  (defadvice org-export-output-file-name (before org-add-export-dir activate)
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir (expand-file-name (substring extension 1) org-build-directory))
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir t)))))

(defun luo-tex/classes ()
  (setq
   org-latex-classes
   '(
     ("article"
      "
%!TEX TS-program = xelatex
%!TEX encoding = UTF-8 Unicode

\\documentclass[13pt,a4paper]{book}
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
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("huwo"
      "
%!TEX TS-program = xelatex
%!TEX encoding = UTF-8 Unicode

\\documentclass[13pt,titlepage,a4paper]{article}
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

\\usepackage{tikz} % 加水印用
\\usepackage{eso-pic}
\\usepackage{graphicx}
%\\usepackage[pages=some]{background}
\\usepackage{background}
\\usepackage{lipsum}

\\newcommand\\DupImage{%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
     \\includegraphics[width=5cm]{/Users/luotao/Documents/assets/img/logo/huwo-logo}\\hfill%
}

\\newlength{\\drop}

\\backgroundsetup{%
  scale=1,       %% change accordingly
  angle=30,       %% change accordingly
  opacity=.05,    %% change accordingly
  contents={%
     \\begin{minipage}{1.5\\paperheight}
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage\\\\[6ex]
     \\DupImage
     \\end{minipage}%
   }
}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
