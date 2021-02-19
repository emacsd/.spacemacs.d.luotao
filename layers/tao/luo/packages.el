;;; packages.el --- luo layer packages file for Spacemacs.

;;; Code:
(defconst luo-packages
  '(
    ;; Just "C-s"
    swiper

    ;; "key chord" means two keys pressed quickly and simultaneously.
    key-chord

    csharp-mode

    ;; A Collection of Ridiculously Useful eXtensions for Emacs.
    ;; crux bundles a few useful interactive commands to enhance
    ;; your overall Emacs experience.
    ;; https://github.com/bbatsov/crux
    crux
    ))

(defun luo/init-swiper ()
  (use-package swiper
    :config
    (progn
      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sb" 'swiper-all))))

(defun luo/init-key-chord ()
  (use-package key-chord
    :config
    (progn
      (key-chord-mode 1)
      (key-chord-define-global "jj" 'avy-goto-word-1)
      (key-chord-define-global "jl" 'avy-goto-line)
      (key-chord-define-global "jk" 'avy-goto-char)
      (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
      (key-chord-define-global "uu" 'undo-tree-visualize)
      (key-chord-define-global "xx" 'execute-extended-command))))

;; remap "M-n" to *-windows
(spacemacs|use-package-add-hook winum
  :post-config
  (define-key winum-keymap (kbd "M-0") 'delete-windows)
  (define-key winum-keymap (kbd "M-1") 'delete-other-windows)
  (define-key winum-keymap (kbd "M-2") 'split-window-vertically)
  (define-key winum-keymap (kbd "M-3") 'split-window-horizontally))

(spacemacs|use-package-add-hook projectile
  :post-init
  (spacemacs/set-leader-keys
    "ps" 'spacemacs/helm-project-do-ag))

(spacemacs|use-package-add-hook google-translate
  :post-init
  (setq google-translate-default-target-language "zh-CN"))

(spacemacs|use-package-add-hook yasnippet
  :post-init
  (when dotspacemacs-directory
    (let* ((dotspacemacs-directory-snippets-dir
            (expand-file-name
             "snippets"
             dotspacemacs-directory)))
      (setq yas-snippet-dirs (list dotspacemacs-directory-snippets-dir)))))

(defun luo/init-csharp-mode ()
  (use-package csharp-mode))

(defun luo/init-multiple-cursors ()
  (use-package multiple-cursors
    :ensure t
    ))

(spacemacs|use-package-add-hook org
  :post-config
  (luo/babel))

(defun luo/init-crux ()
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
