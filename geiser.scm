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
   geiser:add-to-load-path
   ;; Missing functions:
   ;; geiser-start-server
   ;; geiser-object-signature
   ;; geiser-symbol-location
   ;; geiser-symbol-documentation
   ;; geiser-find-file
   ;; geiser-compile-file
   ;; geiser-compile
   ;; geiser-module-path
   ;; geiser-module-location
   ))

(select-module geiser)

;; Utility functions

;; Get the list of elements before the dot in a "dotted list" of the form
;; (x_1 x_2 ... x_n . y)
(define (dotted-list-head dl)
  (if (pair? (cdr dl))
      (cons (car dl) (dotted-list-head (cdr dl)))
      (list (car dl))))

;; Return the first leaf of a tree   
(define (get-first-leaf tree)
  (if (pair? tree)
      (get-first-leaf (car tree))
      tree))

;; Return coloned version of symbol
(define (coloned-sym sym)
  (if (string-prefix? ":" (symbol->string sym))
      sym
      (symbol-append ': sym)))

;; Return the id of a module as symbol
(define (module-id module)
  (let ((module-repr (write-to-string module)))
    (string->symbol
     (substring module-repr 9 (- (string-length module-repr) 1)))))


(define (geiser:macroexpand form . rest)
  (with-output-to-string
    (cut pprint (macroexpand form))))

(define (geiser:eval module-name form . rest)
  rest
  (let* ((output (open-output-string))
         (module (or (and (symbol? module-name )
			  (find-module module-name))
		     ;; TODO or should we eval in the currently selected module?
		     (find-module 'user)))
         (result (with-output-to-port output
                   (lambda ()
                     (eval form module)))))
    (write `((result ,(write-to-string result))
             (output . ,(get-output-string output))))))

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
	    (string-prefix? "(" x)))
    (string-split
     (with-output-to-string
       (cut apropos (string->regexp (string-append "^" prefix))))
     #/\s+/))))

(define (geiser:module-completions prefix)
  (filter
   (cut string-prefix? prefix <>)
   (map (^x (symbol->string (module-name x)))
	(all-modules))))


;;; Autodoc

(define (geiser:autodoc ids . rest)
  (map (cut gauche-info <>)
       ids))

(define (gauche-info id)
  (car 
   (sort (filter-map (cut gauche-info-in-module id <>) (all-modules))
	 > (^x (length (car (cadadr x)))))))

(define (gauche-info-in-module id module)
  (if (hash-table-get (module-table module) id #f)
      (let1 obj (global-variable-ref module id)
	    (if (is-a? obj <procedure>)
		(process-info (~ obj 'info) module)
		`(,id ("args" (("required" "...")))
		      ("module" ,(module-id module)))))
      #f))

(define (process-info info module)
  `(,(car info)
    ("args"
     ,((if (list? info)
	   process-normal-arg-info
	   process-dotted-arg-info)
       (cdr info)))
    ("module" ,(module-id module))))

(define (process-normal-arg-info arg-info)
  (let ((required '("required"))
	(optional '("optional"))
	(key '("key"))
	(section :required)
	(arg-no 0))
    (dolist (x arg-info)
	    (if (memq x '(:optional :key :rest))
		(set! section x)
		(begin
		  (inc! arg-no)
		  (case section
		    ((:optional) (push! optional x))
		    ((:key) (push! key
				   (cons (coloned-sym (get-first-leaf x))
					 arg-no)))
		    ((:rest) (push! required "..."))
		    (else (push! required x))))))
    (map (cut reverse <>)
	 (list required optional key))))

(define (process-dotted-arg-info arg-info)
  `(("required" ,@(dotted-list-head arg-info) "...")
    ("optional")
    ("key")))

;; Further

;; TODO We add the load-path at the end. Is this correct?
(define-macro (geiser:add-to-load-path dir)
  `(add-load-path ,dir :after))

