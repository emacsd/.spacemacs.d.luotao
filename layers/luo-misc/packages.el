;;; packages.el --- luo-misc layer packages file for Spacemacs.

;;; Code:

(defconst luo-misc-packages
  '(swiper
    key-chord
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
    "ps" 'spacemacs/helm-project-do-ag))

(defun luo-misc/init-swiper ()
  (use-package swiper
    :config
    (progn
      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sb" 'swiper-all)
      (global-set-key "\C-s" 'swiper))))

(defun luo-misc/init-key-chord ()
  (use-package key-chord
    :ensure t
    :config
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jk" 'avy-goto-char)
    (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
    (key-chord-define-global "uu" 'undo-tree-visualize)
    (key-chord-define-global "xx" 'execute-extended-command)
    (key-chord-define-global "yy" 'browse-kill-ring)
    (key-chord-mode +1)
    ))

;;; packages.el ends here
