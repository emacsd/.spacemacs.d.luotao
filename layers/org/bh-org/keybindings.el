;;; keybindings.el --- bh-org layer key bindings file for Spacemacs.

;; misc
;; (global-set-key (kbd "M-<left>") 'previous-buffer)
;; (global-set-key (kbd "M-<right>") 'next-buffer)

;; org
(global-set-key (kbd "C-c b") 'org-iswitchb)


(global-set-key (kbd "<f5>") 'bh/org-todo)

(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)

(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> r") 'boxquote-region)


(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)

(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)

;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "<f12>") 'org-agenda)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
