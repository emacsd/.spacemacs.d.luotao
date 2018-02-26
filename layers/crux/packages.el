;;; packages.el --- crux layer packages file for Spacemacs.

;;; Code:

(defconst crux-packages
  '(
    crux
    ))

(defun crux/init-crux ()
  (use-package crux
    :defer t
    :init
    (let ((map (current-global-map)))
      (define-key map (kbd "C-c o") 'crux-open-with)
      (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
      (define-key map [(shift return)] 'crux-smart-open-line)
      (define-key map (kbd "M-o") 'crux-smart-open-line)
      (define-key map [(control shift return)] 'crux-smart-open-line-above)
      (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
      (define-key map (kbd "C-c f")  'crux-recentf-ido-find-file)
      (define-key map (kbd "C-M-z") 'crux-indent-defun)
      (define-key map (kbd "C-c u") 'crux-view-url)
      (define-key map (kbd "C-c e") 'crux-eval-and-replace)
      (define-key map (kbd "C-c s") 'crux-swap-windows)
      (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
      (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
      (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
      (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
      (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
      (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
      (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
      (define-key map (kbd "C-c I") 'crux-find-user-init-file)
      (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
      (define-key map (kbd "s-r") 'crux-recentf-ido-find-file)
      (define-key map (kbd "s-j") 'crux-top-join-line)
      (define-key map (kbd "s-k") 'crux-kill-whole-line)
      (define-key map (kbd "s-o") 'crux-smart-open-line-above)
      map)
    ))

;;; packages.el ends here
