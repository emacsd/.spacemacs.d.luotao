;;; packages.el --- bh-org layer packages file for Spacemacs.

;;; Code:

(defconst bh-org-packages '(bbdb))

(defun bh-org/init-bbdb ())

(spacemacs|use-package-add-hook org
  :post-config
  (require 'org-id)
  (require 'org-habit)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)

  (bh-org/config)

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

  (org-defkey org-mode-map (kbd "C-c C-j") 'org-open-at-point))

;;; packages.el ends here
