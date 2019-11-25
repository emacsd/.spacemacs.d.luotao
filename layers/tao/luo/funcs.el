(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

(defun luo/babel ()
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
