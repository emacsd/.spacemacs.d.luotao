(defun org-luo-common/babel ()
  (setq
   org-babel-load-languages
   '(
     (emacs-lisp . t)
     ;; ob-sh was renamed ob-shell in org 8.2
     (shell . t)
     (dot . t)
     (ditaa . t)
     (plantuml . t))

   org-confirm-babel-evaluate nil

   org-plantuml-jar-path
   (expand-file-name "/usr/local/bin/plantuml")
   ))
