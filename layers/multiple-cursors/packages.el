;;; packages.el --- multiple-cursors layer packages file for Spacemacs.
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
;; added to `multiple-cursors-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `multiple-cursors/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `multiple-cursors/pre-init-PACKAGE' and/or
;;   `multiple-cursors/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst multiple-cursors-packages
  '(
    multiple-cursors
    ))

(defun multiple-cursors/init-multiple-cursors ()
  (use-package multiple-cursors
    :ensure t
    ))

;;; packages.el ends here
