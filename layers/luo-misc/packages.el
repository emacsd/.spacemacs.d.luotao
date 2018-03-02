;;; packages.el --- luo-misc layer packages file for Spacemacs.

;;; Code:

(defconst luo-misc-packages
  '(;; Just "C-s"
    swiper

    ;; "key chord" means two keys pressed quickly and simultaneously.
    key-chord

    ;; google it
    helm-google

    ;; https://github.com/domtronn/all-the-icons.el
    all-the-icons
    ;; https://github.com/domtronn/spaceline-all-the-icons.el
    spaceline-all-the-icons))

(defun luo-misc/init-swiper ()
  (use-package swiper
    :config
    (progn
      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sb" 'swiper-all))))

(defun luo-misc/init-key-chord ()
  (use-package key-chord
    :config
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jk" 'avy-goto-char)
    (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
    (key-chord-define-global "uu" 'undo-tree-visualize)
    (key-chord-define-global "xx" 'execute-extended-command)))

(defun luo-misc/init-helm-google ()
  (use-package helm-google
    :after helm
    :init
    (setq
     helm-google-engines
     '((google . "https://google.com/search?ie=UTF-8&oe=UTF-8&q=%s")
       (searx . "https://searx.dk/?engines=google&format=json&q=%s")))
    :config
    (spacemacs||set-helm-key "swg" helm-google)
    (spacemacs||set-helm-key "sws" helm-google-suggest)))

(defun luo-misc/init-all-the-icons ()
  (use-package all-the-icons))

(defun luo-misc/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :init (setq spaceline-all-the-icons-separator-type 'none)
    :config (spaceline-all-the-icons-theme)))

;; hack spacemacs layers

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

;;; packages.el ends here
