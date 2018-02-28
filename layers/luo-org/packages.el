;;; packages.el --- luo-org layer packages file for Spacemacs.

;;; Code:

(defconst luo-org-packages '())

(spacemacs|use-package-add-hook org
  :post-init
  (luo-org/config-defaults)
  (luo-org/config-agenda)
  (luo-org/config-latex)

  :post-config
  (spacemacs//set-monospaced-font "Monaco" "PingFang SC" 12 14)

  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)

  (org-defkey org-mode-map "\C-c\C-j" 'org-open-at-point))

;;; packages.el ends here
