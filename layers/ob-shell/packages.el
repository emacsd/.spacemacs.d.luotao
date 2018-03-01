;;; packages.el --- ob-shell layer packages file for Spacemacs.
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
;; added to `ob-shell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ob-shell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ob-shell/pre-init-PACKAGE' and/or
;;   `ob-shell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ob-shell-packages
  '(
    ;; Interact with tmux from Emacs. https://github.com/syohex/emacs-emamux
    emamux
    (ob-shell :location built-in)
    ))

(defun ob-shell/init-emamux ()
  (use-package emamux
    :ensure t))

(defun ob-shell/init-ob-shell ()
  (use-package ob-shell
    :config
    (defun org-babel-execute:shell (body params)
      "Execute a block of Shell commands with Babel.
This function is called by `org-babel-execute-src-block'."
      (emamux:send-command body))))

;;; packages.el ends here
