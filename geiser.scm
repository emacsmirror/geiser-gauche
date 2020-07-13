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
   geiser:module-exports
   ;; Missing functions:
   ;; geiser:start-server
   ;; geiser:object-signature
   ;; geiser:symbol-location
   ;; geiser:find-file
   ;; geiser:compile
   ))

(select-module geiser)

;; Utility functions

;; Return the list of elements before the dot in a "dotted list" of the form
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

;; Add a colon at the beginning to a symbol
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
  ;; (call-with-output-file "/tmp/gauche.log" 
  ;;   (^o (format o "FORM: ~s, REST: ~s" form rest)))
  (let* ((output (open-output-string))
         (module (or (and (symbol? module-name )
			  (find-module module-name))
		     (find-module 'user)))
         (result (with-output-to-port output
                   (lambda ()
                     (eval form module)))))
    (write `((result ,(write-to-string result))
             (output . ,(get-output-string output))))))

(define (geiser:load-file filename . rest)
  (geiser:eval 'user `(load ,filename)))

(define (geiser:compile-file filename . rest)
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

(define (geiser:module-completions prefix . rest)
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

;; Return a list of symbol-infos, i.e., (SIGNATURE-OR-SYMBOL MODULE) pairs for
;; all bindings of SYMBOL. SIGNATURE-OR-SYMBOL is the signature of SYMBOL in
;; MODULE if it can be found, and SYMBOL otherwise.
(define (symbol-infos symbol)
  (let ((signatures-w-modules
	 (map (^x (cons (signature-in-module symbol x)
			(module-id x)))
	      (all-modules))))
    (remove (^x (not (car x)))
	    signatures-w-modules)))

;; Format a symbol-info list for presenting with symbol documentation
(define (format-symbol-infos symbol-infos)
  (map (^x `(,(cdr x) ,(if (pair? (car x))
			   (car x)
			   `(,(car x) "..."))))
       symbol-infos))

(define (geiser:symbol-documentation symbol . rest)
  `(("signature" ,(format-symbol-infos (symbol-infos symbol)))))


;;; Autodoc

(define (geiser:autodoc symbols . rest)
  (map (cut formatted-autodoc <> (car rest))
       symbols))

(define (formatted-autodoc symbol pref-module)
  (format-autodoc-symbol-info
   (autodoc-symbol-info symbol pref-module)))

;; Return a (SIGNATURE-OR-SYMBOL MODULE) pair or SYMBOL itself to be used in the
;; autodoc for SYMBOL. SIGNATURE-OR-SYMBOL is a signature of SYMBOL in MODULE if
;; it can be found, and SYMBOL otherwise. Only SYMBOL and not a pair is returned
;; if no suitable bindings were found. Prefer the binding which is visible from
;; module PREF-MODULE, which should be a symbol.
(define (autodoc-symbol-info symbol pref-module)
  (let1 sis (symbol-infos symbol)
	(if (not (null? sis))
	    (or (find (^x (eq? (global-variable-ref pref-module symbol #f)
			       (global-variable-ref (cdr x) symbol #f)))
		      sis)
		(find (^x ($ not $ symbol? $ car x)) sis)
		(car sis))
	    symbol)))


;; Format an autodoc symbol-info in autodoc format.
(define (format-autodoc-symbol-info asi)

  (define (format-normal-arg-info arg-info)
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

  (if (symbol? asi)
      (list asi)
      (let ((sig (car asi)) (module (cdr asi)))
	(if (symbol? sig)
	    `(,sig ("args" (("required" "...")))
		   ("module" ,module))
	    `(,(car sig)
	      ("args"
	       ,((if (list? sig)
		     format-normal-arg-info
		     process-dotted-arg-info)
		 (cdr sig)))
	      ("module" ,module))))))

;; Module documentation

(define (geiser:module-exports mod-name . rest)
  (let* ((module (find-module mod-name))
	 (symbols (module-exports module))
	 (syms-objs
	  (map (^x (cons x (global-variable-ref module x)))
	       symbols))
	 (procs ()) (macros ()) (vars ()))
    (dolist (sym-obj syms-objs)
	    (let ((obj (cdr sym-obj))
		  (sym (car sym-obj)))
		  (cond
		   ((is-a? obj <procedure>) (push! procs sym))
		   ((or (is-a? obj <macro>)
			(is-a? obj <syntax>)) (push! macros sym))
		   (else (push! vars sym)))))
    (list (cons "procs" (map list procs))
	  (cons "syntax" (map list macros))
	  (cons "vars" (map list vars)))))


;; Further

(define (geiser:module-location m . rest)
  (and (find-module m)
       (let1 paths (map cdr (library-fold m acons '()))
	     (if (pair? paths)
		 `(("file" . ,(car paths)) ("line") ("column"))
		 ()))))


;; TODO We add the load-path at the end. Is this correct?
(define-macro (geiser:add-to-load-path dir)
  `(add-load-path ,dir :after))


