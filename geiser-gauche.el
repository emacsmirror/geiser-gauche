;;; geiser-gauche.el -- Gauche Scheme's implementation of the geiser protocols

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.


;;; Code:

(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'compile)
(require 'info-look)

(eval-when-compile (require 'cl-lib))


;;; Customization:

(defgroup geiser-gauche nil
  "Customization for Geiser's Gauche Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-gauche-binary
    "gosh"
  "Name to use to call the Gauche Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-gauche)

(geiser-custom--defcustom geiser-gauche-extra-command-line-parameters '("-i")
  "Additional parameters to supply to the Gauche binary."
  :type '(repeat string)
  :group 'geiser-gauche)


;;; REPL support:

(defun geiser-gauche--binary ()
  (if (listp geiser-gauche-binary)
      (car geiser-gauche-binary)
    geiser-gauche-binary))

(defun geiser-gauche--parameters ()
  "Return a list with all parameters needed to start Gauche Scheme."
  `(,@geiser-gauche-extra-command-line-parameters
    "-l" ,(expand-file-name "gauche/geiser.scm" geiser-scheme-dir)
    ,@(and (listp geiser-gauche-binary) (cdr geiser-gauche-binary))))

(defconst geiser-gauche--prompt-regexp "> ")


;;; Evaluation support:

(defun geiser-gauche--geiser-procedure (proc &rest args)
  (cl-case proc
    ((eval compile)
     (let ((form (mapconcat 'identity (cdr args) " "))
           (module (cond ((string-equal "'()" (car args))
                          "'()")
                         ((and (car args))
                          (concat "'" (car args)))
                         (t
                          "#f"))))
       (format "(geiser:eval %s '%s)" module form)))
    ((load-file compile-file)
     (format "(geiser:load-file %s)" (car args)))
    ((no-values)
     "(geiser:no-values)")
    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

(defconst geiser-gauche--module-re
  "(define-module +\\(\\w+\\)")

(defun geiser-gauche--get-module (&optional module)
  (cond ((null module)
         (save-excursion
           (geiser-syntax--pop-to-top)
	   (message "%s" )
           (if (or (re-search-backward geiser-gauche--module-re nil t)
                   (looking-at geiser-gauche--module-re)
                   (re-search-forward geiser-gauche--module-re nil t))
               (geiser-gauche--get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-gauche--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-gauche--import-command (module)
  (format "(import %s)" module))

(defun geiser-gauche--exit-command () "(exit)")

;;; REPL startup

(defconst geiser-gauche-minimum-version "0.9.9")

(defun geiser-gauche--version (binary)
  (cadr (read (cadr (process-lines "gosh" "-V")))))

(defun geiser-gauche--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    (geiser-eval--send/wait "(import geiser)(newline)")))

(defun geiser-gauche--display-error (module key msg)
  (and key (message msg) nil))



;;;  Manual look up

(defun gauche--manual-look-up (id mod)
  (let ((info-lookup-other-window-flag
         geiser-gauche-manual-lookup-other-window-p))
    (info-lookup-symbol (symbol-name id) 'geiser-gauche-mode))
  (when geiser-gauche-manual-lookup-other-window-p
    (switch-to-buffer-other-window "*info*"))
  (search-forward (format "%s" id) nil t))


;;; Implementation definition:

(define-geiser-implementation chez
  (binary geiser-gauche--binary)
  (arglist geiser-gauche--parameters)
  (version-command geiser-gauche--version)
  (minimum-version geiser-gauche-minimum-version)
  (repl-startup geiser-gauche--startup)
  (prompt-regexp geiser-gauche--prompt-regexp)
  (debugger-prompt-regexp nil) ;; geiser-gauche--debugger-prompt-regexp
  ;; (enter-debugger geiser-gauche--enter-debugger)
  (marshall-procedure geiser-gauche--geiser-procedure)
  (find-module geiser-gauche--get-module)
  ;; (enter-command geiser-gauche--enter-command)
  (exit-command geiser-gauche--exit-command)
  (import-command geiser-gauche--import-command)
  (find-symbol-begin geiser-gauche--symbol-begin)
  (display-error geiser-gauche--display-error)
  ;; (external-help geiser-gauche--manual-look-up)
  ;; (check-buffer geiser-gauche--guess)
  ;; (keywords geiser-gauche--keywords)
  ;; (case-sensitive geiser-gauche-case-sensitive-p)
  )

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'gauche t)

(provide 'geiser-gauche)
