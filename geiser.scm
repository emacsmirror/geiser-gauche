(define-module geiser
  (use srfi-13)
  (use gauche.interactive)
  (export
   geiser:macroexpand
   geiser:eval
   geiser:newline
   geiser:autodoc
   geiser:load-file
   geiser:no-values
   geiser:completions
   geiser:module-completions
   ;; Missing functions:
   ;; geiser-start-server
   ;; geiser-object-signature
   ;; geiser-symbol-location
   ;; geiser-symbol-documentation
   ;; geiser-find-file
   ;; geiser-add-to-load-path
   ;; geiser-compile-file
   ;; geiser-compile
   ;; geiser-module-path
   ;; geiser-module-location
   ))

(select-module geiser)

(define (geiser:macroexpand form . rest)
  (with-output-to-string
    (cut pprint (macroexpand form))))


(define (geiser:eval module-name form . rest)
  rest
  (let* ((output (open-output-string))
         (module (or (and (symbol? module-name )
			 (find-module module-name))
		    (find-module 'user)))
         (result (with-output-to-port output
                   (lambda ()
                     (eval form module)))))
    (write `((result ,(write-to-string result))
             (output . ,(get-output-string output))))))


(define (geiser:autodoc ids . rest)
  (map (cut ~ <> 'info)
       ids))

(define (geiser:load-file filename)
  (load filename))

(define (geiser:newline)
  (newline))

(define (geiser:no-values)
  (values))

;;; Completions

(define (geiser:completions prefix)
  (delete-duplicates
   (remove
    (^x (or (string=? x "")
	    (string-prefix? "(" x)
	    ;; TODO check whether it is bound in the current module?
	    ;; probably needs changing this into a macro...
	    ;; (not (global-variable-bound? (current-module)
	    ;; 				 (string->symbol x)))
	    ))
    (string-split
     (with-output-to-string
       (cut apropos (string->regexp (string-append "^" prefix))))
     #/\s+/))))

(define (geiser:module-completions prefix)
  (filter
   (cut string-prefix? prefix <>)
   (map (^x (symbol->string (module-name x)))
	(all-modules))))

