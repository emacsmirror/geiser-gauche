;;; geiser-gauche.el -- Chez Scheme's implementation of the geiser protocols

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
  "Customization for Geiser's Chez Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-gauche-binary
    "scheme"
  "Name to use to call the Chez Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-gauche)

(geiser-custom--defcustom geiser-gauche-init-file "~/.chez-geiser"
  "Initialization file with user code for the Chez REPL."
  :type 'string
  :group 'geiser-gauche)

(geiser-custom--defcustom geiser-gauche-extra-command-line-parameters '()
  "Additional parameters to supply to the Chez binary."
  :type '(repeat string)
  :group 'geiser-gauche)


;;; REPL support:

(defun geiser-gauche--binary ()
  (if (listp geiser-gauche-binary)
      (car geiser-gauche-binary)
    geiser-gauche-binary))

(defun geiser-gauche--parameters ()
  "Return a list with all parameters needed to start Chez Scheme.
This function uses `geiser-gauche-init-file' if it exists."
  (let ((init-file (and (stringp geiser-gauche-init-file)
                        (expand-file-name geiser-gauche-init-file))))
    `(,@(and init-file (file-readable-p init-file) (list init-file))
      ,(expand-file-name "chez/geiser/geiser.ss" geiser-scheme-dir)
      ,@geiser-gauche-extra-command-line-parameters)))

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

(defun geiser-gauche--get-module (&optional module)
  (cond ((null module)
         :f)
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

(defun geiser-gauche--exit-command () "(exit 0)")
;; 
;; ;;; REPL startup

(defconst geiser-gauche-minimum-version "9.4")

(defun geiser-gauche--version (binary)
  (car (process-lines binary "--version")))

(defun geiser-gauche--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    (geiser-eval--send/wait "(begin (import (geiser)) (write `((result ) (output . \"\"))) (newline))")))

(defun geiser-gauche--display-error (module key msg)
  (and key (message msg) nil))



;;;  Manual look up

(defun guile--manual-look-up (id mod)
  (let ((info-lookup-other-window-flag
         geiser-guile-manual-lookup-other-window-p))
    (info-lookup-symbol (symbol-name id) 'geiser-guile-mode))
  (when geiser-guile-manual-lookup-other-window-p
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

(geiser-impl--add-to-alist 'regexp "\\.ss$" 'chez t)
(geiser-impl--add-to-alist 'regexp "\\.def$" 'chez t)

(provide 'geiser-gauche)
