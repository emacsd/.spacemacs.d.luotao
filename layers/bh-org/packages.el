;;; packages.el --- bh-org layer packages file for Spacemacs.

;;; Code:

(defconst bh-org-packages '(bbdb))

(defun bh-org/init-bbdb ())

(spacemacs|use-package-add-hook org
  :post-config
  (require 'org-id)
  (require 'org-habit)
  ;; uncheck the boxes automagically when the task is marked done.
  (require 'org-checklist)

  (require 'org-protocol)
  (require 'org-crypt)
  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  (setq org-crypt-key "F0B66B40")
  (setq org-crypt-disable-auto-save nil)


  ;; Use utf-8 as default coding system
  (setq org-export-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))


  (add-to-list 'org-modules 'org-bbdb)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)
  ;; (add-to-list 'org-modules 'org-jsinfo)
  (add-to-list 'org-modules 'org-inlinetask)
  (add-to-list 'org-modules 'org-mew)
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-vm)
  (add-to-list 'org-modules 'org-wl)

  (bh-org/config)

  (org-defkey org-mode-map (kbd "C-c C-j") 'org-open-at-point)

  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))))

;;; packages.el ends here
