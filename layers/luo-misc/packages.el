;;; packages.el --- luo-misc layer packages file for Spacemacs.

;;; Code:

(defconst luo-misc-packages
  '(
    ))

(spacemacs|use-package-add-hook winum
  :post-config
  (define-key winum-keymap (kbd "M-0") 'delete-windows)
  (define-key winum-keymap (kbd "M-1") 'delete-other-windows)
  (define-key winum-keymap (kbd "M-2") 'split-window-vertically)
  (define-key winum-keymap (kbd "M-3") 'split-window-horizontally)
  )

(spacemacs|use-package-add-hook projectile
  :post-config
  (spacemacs/set-leader-keys
    "ps" 'helm-do-ag-project-root))

;;; packages.el ends here
