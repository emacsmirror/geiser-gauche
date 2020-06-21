(define-module geiser
  (use srfi-13)
  (use gauche.interactive)
  (export
   geiser:macroexpand
   geiser:eval
   geiser:newline
   geiser:autodoc
   geiser:load-file
   geiser:compile-file
   geiser:no-values
   geiser:completions
   geiser:module-completions
   geiser:add-to-load-path
   geiser:symbol-documentation
   geiser:module-location
   ;; Missing functions:
   ;; geiser:start-server
   ;; geiser:object-signature
   ;; geiser:symbol-location
   ;; geiser:find-file
   ;; geiser:compile
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
  (geiser:eval 'user `(load ,filename)))

(define (geiser:compile-file filename)
  (geiser:load-file  filename))

(define (geiser:newline . rest)
  (newline))

(define (geiser:no-values . rest)
  (values))

;;; Completions

(define (geiser:completions prefix . rest)
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

;; Symbol documentation 

;; Return the signature of SYMBOL in MODULE if there is one, SYMBOL if the
;; symbol is bound without one, #f otherwise.
(define (signature-in-module symbol module)
  (if (hash-table-get (module-table module) symbol #f)
      (let1 obj (global-variable-ref module symbol)
	    (if (is-a? obj <procedure>)
		(~ obj 'info)
		symbol))
      #f))

;; Return a list of (signature module) pairs for all bindings of SYMBOL with
;; signature. If SYMBOL is bound without the signature then the car is SYMBOL.
(define (signatures symbol)
  (let ((signatures-w-modules
	 (map (^x (cons (signature-in-module symbol x)
			(module-id x)))
	      (all-modules))))
    (remove (^x (not (car x)))
	    signatures-w-modules)))

;; Format a signature list for presenting with symbol documentation
(define (format-signatures sigs)
  (map (^x `(,(cdr x) ,(if (pair? (car x))
			   (car x)
			   `(,(car x) "..."))))
       sigs))

(define (geiser:symbol-documentation symbol . rest)
  `(("signature" ,(format-signatures (signatures symbol)))))


;;; Autodoc

(define (geiser:autodoc symbols . rest)
  (map (cut formatted-autodoc <>)
       symbols))

(define (formatted-autodoc symbol)
  (format-autodoc-signature (autodoc-signature symbol)))

(define (format-autodoc-signature as)
  (if (symbol? as)
      (list as)
      (let ((sig (car as))
	    (module (cdr as)))
	(if (symbol? sig)
	    `(,sig ("args" (("required" "...")))
		   ("module" ,module))
	    (signature->autodoc sig module)))))

;; Return a (signature module) pair to be displayed in autodoc for SYMBOL.
;; Return a (SYMBOL module) pair if SYMBOL is bound without signature and 
;; SYMBOL if no binding was found.
(define (autodoc-signature symbol)
  (let1 sigs (signatures symbol)
	(if (not (null? sigs))
	    (or (find (^x ($ not $ symbol? $ car x)) sigs)
		(car sigs))
	    symbol)))

;; Format a signature for Geiser autodoc
(define (signature->autodoc signature module-id)
  
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
  
  `(,(car signature)
    ("args"
     ,((if (list? signature)
	   process-normal-arg-info
	   process-dotted-arg-info)
       (cdr signature)))
    ("module" ,module-id)))


;; Further

(define (geiser:module-location m)
  (and (find-module m)
       (let1 paths (map cdr (library-fold m acons '()))
	     (if (pair? paths)
		 `(("file" . ,(car paths)) ("line") ("column"))
		 ()))))


;; TODO We add the load-path at the end. Is this correct?
(define-macro (geiser:add-to-load-path dir)
  `(add-load-path ,dir :after))
