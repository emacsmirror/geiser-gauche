;;; geiser-gauche-setup.el --- Set up Gauche support for Geiser -*- lexical-binding:t -*-

;; Copyright (C) 2020 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>
;; SPDX-License-Identifier: BSD-3-Clause

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Function to set up Gauche support in Geiser. It is in a separate file to
;; avoid loading geiser-gauche in the setup phase.

;;; Code:

(require 'geiser-gauche)

;;;###autoload
(defun geiser-gauche-setup ()
  "Set up Geiser Gauche support.
Add Gauche to the list of active Scheme implementations if the
user didn't customize the list."
  (interactive)
  (eval-after-load 'geiser-impl
    ;; Add Gauche only if the list of active impls is the standard one.
    (when (equalp (cadar (get 'geiser-active-implementations 'standard-value))
		  geiser-active-implementations)
      (add-to-list 'geiser-active-implementations 'gauche))))

(provide 'geiser-gauche-setup)

;;; geiser-gauche-setup.el ends here
