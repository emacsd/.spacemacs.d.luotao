;;; packages.el --- luo-tmux layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: 罗涛 <luotao@morgana>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `luo-tmux-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `luo-tmux/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `luo-tmux/pre-init-PACKAGE' and/or
;;   `luo-tmux/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst luo-tmux-packages
  '(
    ;; Interact with tmux from Emacs. https://github.com/syohex/emacs-emamux
    emamux
    (luo-tmux :location built-in)
    ))

(defun luo-tmux/init-emamux ()
  (use-package emamux
    :ensure t))

(defun luo-tmux/init-luo-tmux ()
  (use-package luo-tmux
    :config
    (defun org-babel-execute:shell (body params)
      "Execute a block of Shell commands with Babel.
This function is called by `org-babel-execute-src-block'."
      (emamux:send-command body))))

;;; packages.el ends here
