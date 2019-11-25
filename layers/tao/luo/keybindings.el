;;; keybindings.el --- luo layer key bindings file for Spacemacs.

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-z") 'set-mark-command)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(global-set-key (kbd  "C-s") 'swiper)
