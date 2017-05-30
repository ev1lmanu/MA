#lang racket

(require swindle/misc) ; for cpl

#|

This module introduces the following changes to the class macro:
* it is possible to specify a list of superclasses
* you can definie generic functions with define/generic and specify a
  method combination
* fields can be re-declared in classes that have multiple superclasses or
  where a method combination is involved
* implementing methods can be (re-)declared with define/public in any subclass
  of the class with the generic function

    (class (first second ...)
        ...
        (define/generic (methodname args...) combination)
        ...
    )

The superclass specifier can be either a single class (as in standard Object Racket) or an unquoted, possibly empty list of classes.

The combination can be any function that can be applied to a list of arguments, e.g. +, list, append or (compose reverse list).

Only fields and methods defined with field, init-field or define/public will be inherited when there is more than one superclass or a generic function is definied for the class. Otherwise, Racket will handle inheritance. Method combination is not compatible with override or augment.

-----------------------------------------------------

This module is a prototype for extending the class macro for multiple inheritance without touching the underlying implementation of Racket's object system.

As long as only single inheritance is used there should be no noticable difference between using this module and not using it. When the macro is used with multiple superclasses, the class will inherit fields and methods from all superclasses depending on the precedence of the classes or the specified combination. The result is a normal Racket object. The actual superclass will be object% and the code of all inherited fields and methods will be injected into the class macro by hand. 

The implementation uses a metaobject protocol (MOP) that loosely follows "The Art of of the Metaobject Protocol" (AMOP) by Gregor Kiczales, Jim des Rivières and Daniel G. Bobrow. Instead of defining an own hidden layer, it uses the existing Racket object system as basis though: Whenever a Racket class is created, we'll keep track of it and store some additional info about it in form of a(nother) Racket object that we call meta-object. The meta-object keeps track of everything that we won't be (easily) able to ask the class after creation. The class that defines those meta-objects is consequently called meta-class. 

This module was created by Manuela Beckert as master thesis project. The corresponding German thesis is titled "Untersuchungen zur Integration von CLOS-Konzepten in das Objektsystem von Racket" and will be accessible at the library in the computer science department of the University of Hamburg, Germany. 
|#

; ---------------------- MACRO -----------------------

; provide new version of the class macro
(provide (rename-out [my-class class]))

; Class macro part I
; Enable us to use eval in the definitions window.
; For this purpose we'll create an eval-function that uses the
; namespace of the module where the macro is called and attach
; it to the macro call.
(define-syntax my-class
  (syntax-rules ()
    [(my-class arg . args)
     ; Create a new namespace.
     (let ([ns (make-base-namespace)])
       ; Attach modules of the current namespace.
       ; It might not be nessecary to also do this for list
       ; and function, but let's be safe.
       (namespace-attach-module (current-namespace) 'racket/class ns)
       (namespace-attach-module (current-namespace) 'racket/list ns)
       (namespace-attach-module (current-namespace) 'racket/function ns)
       ; Import identifiert bindings from attached modules.
       ; If the required module is attached, the attached version 
       ; will be used, otherwise a fresh version is imported
       (parameterize ([current-namespace ns])
         (namespace-require 'racket/class)
         (namespace-require 'racket/list)
         (namespace-require 'racket/function))
       ; Create an eval function that uses the namespace
       ; and add it to the syntax of the class call.
       ; Then expand the original class macro.
       (my-eval-class (λ (x) (eval x ns)) arg . args))]))

; Class macro part II
; Instead of only a single superclass, this macro allows
; to also specify a (possibly empty) list of superclasses.
; Thus, there are three cases: That we got
; * a single class
; * an empty list (we'll treat this as object%)
; * a non-empty list
; We'll process all three cases into a list form and
; then call the expand! function which does the actual work.
(define-syntax my-eval-class
  (syntax-rules ()
    [(my-eval-class my-eval () . rest) 
     (expand! my-eval (list object%) 'rest)]
    [(my-eval-class my-eval (super ...) . rest)
     (expand! my-eval (list super ...) 'rest)]
    [(my-eval-class my-eval super . rest)
     (expand! my-eval (list super) 'rest)]))

; The actual expansion happens here
; * creates a class object and returns it
; * classes with a single superclass will be handled solely
;   by Object Racket, we'll just collect some info on them
; * classes with multiple suplerclasses will be put together
;   here; they will have object% as "official" superclass
;   because we'll do all the inheritance work already and
;   Object Racket shall not do it again
; * information that we'll need later about the class object
;   will be stored in a meta object
(define (expand! my-eval supers args)
  ; create meta object
  (let* ([meta (ensure-class supers args)]                    ; meta object
         [obj  (make-classobject my-eval supers args meta)])  ; class object
    ; add the new class to the list of observed classes
    ; since classes have no name, we use the object reference as key
    (add-class obj meta)
    ; return the class object
    obj))

; creates a metaobject.
; supers - the list of superclasses
; args - other arguments passed to the class macro
(define (ensure-class supers args)
  (let* ([direct-supers     (map find-class supers)]  
         [direct-fields     (compute-direct-fields args)]
         [direct-methods    (filter method-definition? args)]
         [generic-functions (filter generic-function-definition? args)]
         [meta (make-object meta-class% direct-supers direct-fields
                 direct-methods generic-functions)])
    (finalize-inheritance meta)
    (ensure-generic-functions meta)
    meta))

;; creates a class object
(define (make-classobject my-eval supers args meta)
  ; remove define/generic forms
  (let ([args (filter (negate generic-function-definition?) args)])
    ; if there's only a single superclass and no generic functions involved
    (if (racket-only? meta)
        ; then we can let racket handle object creation
        (my-eval (append '(class) supers args))
        ; else, put together the superclass and other class options
        ; and use them for the new class
        (my-eval (append '(class)
                         (generate-class-options my-eval meta)
                         ; other args, including the call to
                         ; super-new
                         (filter (negate (curryr is-generic? meta)) args))))))

; Returns whether we can let Racket handle object creation
; or if we need to put together the class ourselves
(define (racket-only? meta)
  (and (= 1 (length (get-field direct-supers meta)))
       (empty? (applicable-generic-functions meta))))

; Generates all class options we'll need to supply to the class
; macro if we put together the class ourselves.
(define (generate-class-options my-eval meta)
  (let* ([inherited-fields  (get-field inherited-fields meta)] 
         [not-redeclared    (send meta not-redeclared-fields)] ; !!
         [direct-methods    (get-field direct-methods meta)]
         [inherited-methods (get-field inherited-methods meta)])
    (append
     ; superclass
     (list (my-eval (append '(class object% (super-new))
                            inherited-fields
                            ; only  methods that aren't
                            ; part of a method combination
                            (filter (negate (curryr is-generic? meta))
                                    inherited-methods))))
     (if (empty? (applicable-generic-functions meta))
         '()
         ; if method combination is involved
         (append ; an inherit-field clause for every inherited field
                 ; that is not redefined in the current class;
                 ; they could possibly be used by the combined methods
                 (map (λ (field) (list 'inherit-field (get-name field)))
                      not-redeclared)
                 ; combination methods
                 (map (curry combine-method meta)
                      (applicable-generic-functions meta)))))))

; --------------------- META CLASSES ----------------------

; since classes have no name, we count them, so we
; can distinguish them for debugging
(define num-of-classes -1)

; class for class meta objects.
; meta objects store additional information for a class object
(define meta-class%
  (class object% (super-new)
    
    ;; init fields
    (init-field direct-supers
                direct-fields
                direct-methods
                generic-functions)

    ; fields
    (field [number (begin 
                     (set! num-of-classes (+ 1 num-of-classes))
                     num-of-classes)]
           [class-precedence-list '()]
           [inherited-fields '()]
           [inherited-methods '()])

    ; inherited fields that were not redeclared !!
    (define/public (not-redeclared-fields)
      (filter (lambda (field) (not (member (get-name field)
                                           (map get-name direct-fields))))
              inherited-fields))

    ; effective fields
    (define/public (effective-fields)
      (append direct-fields inherited-fields))

    ; effective methods
    (define/public (effective-methods)
      (append direct-methods inherited-methods))))

; table that maps each class object to its meta object
(define class-table (make-hasheq))

(define (find-class obj)
  (hash-ref class-table obj))

(define (add-class obj metaclass)
  (hash-set! class-table obj metaclass))

; add object% and its meta object to the table
(add-class object% (make-object meta-class% '() '() '() '()))

; ---------------- COMPUTING FIELDS AND METHODS ----------------

; define what a field, method and generic function definition looks like
(define (field-definition? s)
  (member (car s) '(field init-field)))

(define (method-definition? m)
  (equal? (car m) 'define/public))

(define (generic-function-definition? m)
  (equal? (car m) 'define/generic))

; extract field definitions from args
(define (compute-direct-fields args)
  (let ([fields (filter field-definition? args)])
    (if (empty? fields)
        '()
        (apply append (map split-1 fields)))))

; splits field-declarations with multiple values
; (field [a 1] [b 2]) -> ((field [a 1]) (field [b 2]))
(define (split-1 field)
  (map (curry list (car field)) (cdr field)))

; adds the class precedence list, inherited fields and
; inherited methods to a metaobject
(define (finalize-inheritance meta)
  (let ([cpl (compute-cpl meta)])
    (set-field! class-precedence-list meta cpl)
    (set-field! inherited-fields  meta (compute-inherited-fields (cdr cpl)))
    (set-field! inherited-methods meta (compute-inherited-methods (cdr cpl)))))

; compute the class precedence list
; We use the function compute-std-cpl from swindle.
; It receives the object for which the cpl is to be computed
; and a method that returns the superclasses of a given object
; and gives us the cpl.
; You can find the function near the end of this module.
(define (compute-cpl meta)
  (compute-std-cpl meta (lambda (obj) (get-field direct-supers obj))))

; Computes the list of inherited fields and methods
; (standard method combination).
; Since both are very similar, the main logic is in
; compute-inherited-stuff. Just substitute "stuff"
; for "fields" or "methods" in your head.
(define (compute-inherited-fields cpl)
  (compute-inherited-stuff 'direct-fields cpl))

(define (compute-inherited-methods cpl)
  (compute-inherited-stuff 'direct-methods cpl))

(define (compute-inherited-stuff direct-stuff cpl)
  (let ([stuff '()])
    ; add the fields/methods of all superclasses to the list
    (for*/list ([metas cpl])
      (set! stuff (append stuff 
                          (dynamic-get-field direct-stuff metas))))
    ; only keep the first appearance of every field/method declaration
    (filter-first-occurence stuff)))

; Filters a list of field or method definitions so that
; only the first declaration of each field/method
; is kept:
; '((field [a 1]) (field [a 2]) (field [b 3]) (field [b 0])))
;  -> '((field [a 1]) (field [b 3])
; This is probably very inefficient!!
(define (filter-first-occurence xs)
  (remove-duplicates (map (curryr first-occurence xs) xs)))

; returns the first occurence of a field or method in the list
(define (first-occurence elem xs)
  (car (filter (lambda (x) (equal? (get-name x)
                                   (get-name elem)))
               xs)))

; gives us the name of a field or method
(define (get-name x)
  (if (symbol? (second x)) 
      (second x)    ; field name
      (car (second x)))) ; method name

; ------------------- GENERIC FUNCTIONS -----------------------

; class for generic function objects
(define meta-generic-function%
  (class object% (super-new)
    (init-field name
                params
                meta-class
                combination)        ; quoted operator
    (field [methods (make-hasheq)]) ; (meta, (λ ...)) 
    
    (define/public (number-of-params)
      (length params))
    
    ; adds an implementing method to the generic function
    (define/public (add-method meta method)
      (hash-set! methods meta method))
    
    ; returns a list of all applicable methods
    ; (as quoted lambda functions), sorted by cpl
    (define/public (applicable-methods meta)
      (let* ([cpl (get-field class-precedence-list meta)]
             [applicable
              ; methods that are specialized on classes from cpl
              (map (curry hash-ref methods)
                   (filter (curry hash-has-key? methods) cpl))])
        (if (empty? applicable)
            (list (list 'λ
                        'x
                        (list 'error
                              "No applicable methods found for generic function:"
                              `',name)))
            applicable)))))

; table that maps each generic function name to its object
(define generic-function-table (make-hash))

(define (get-generic name)
  (hash-ref generic-function-table name))

(define (add-generic name meta)
  (hash-set! generic-function-table name meta))

; returns whether x is part of a generic function
(define (is-generic? x meta)
  (and (or (method-definition? x)
           (generic-function-definition? x))
       (hash-has-key? generic-function-table (get-name x))
       (member (get-field meta-class (get-generic (get-name x)))
               (get-field class-precedence-list meta))))

; ---------------- COMPUTING GENERIC FUNCTIONS  -------------------

; Updates the generic function table with method and gf defintions
; in the given meta object
(define (ensure-generic-functions meta)
  ; keep track of new generic functions and replace the definition
  ; in meta with the meta objects
  (set-field! generic-functions
              meta
              (map (curryr make-generic-function meta)
                   (get-field generic-functions meta)))
  ; add  new methods to existing generic functions
  (for ([method (get-field direct-methods meta)])
    ; for each new method, if a generic function for it exists
    (when (is-generic? method meta)
      ; get generic function for it
      (let* ([gf (get-generic (get-name method))]
             [gf-params (send gf number-of-params)]
             [method-params (length (cdr (second method)))])
        ; if the generic function is defined for a superclass
        (when  (member (get-field meta-class gf)
                       (get-field class-precedence-list meta))
          ; and the method has the right amount of parameters
          (if (= gf-params method-params)
              ; add new method to generic function
              (send gf
                    add-method
                    meta
                    (method->λ method))
              ; else, raise an error
              (error (string-append "Parameters do not match. Generic function has "
                                    (number->string gf-params)
                                    " and method has "
                                    (number->string method-params)
                                    ": ")
                     (get-name method))))))))

; Creates a metaobject for gf and adds it to the list
; If it already exists, an error will be signaled.
; generic function definitions have the form:
; '(define/generic (name args...) combination)
;     combination: operator
(define (make-generic-function gf-defintion meta)
  (let ([name (get-name gf-defintion)])
    (if (hash-has-key? generic-function-table name)
        (error "Duplicate definition of generic function" name)
        (let* ([params (cdr (second gf-defintion))]
               [combination (third gf-defintion)]
               [gf (make-object meta-generic-function% name
                     params meta combination)])
          (add-generic name gf)
          gf))))

; Converts a method expression into a quoted lambda function
; '(define/public (foo x y) (+ x y)) -> '(λ (x y) (+ x y))
(define (method->λ method)
  (list 'λ (cdr (second method)) (third method)))

; Returns the names of the applicable generic functions of a class
(define (applicable-generic-functions meta)
  (apply append
         (map (lambda (x) (get-field generic-functions x))
              (get-field class-precedence-list meta))))

; Returns a (quoted) function definition that combines all applicable
; methods.
; The parameters of the resulting lambda function are applied 
; to all applicable methods and on those values the combination function
; is applied.
; Since we're putting together the syntax without evaluating it,
; we have to work with lists and quoted operators.
; For example for the values
; gf:          (foo x y)
; name:        foo
; params:      x y
; combination: *
; functions:   (λ (x y) (+ x y)), (λ (x y) (- x y))
; the result will be:
; '(define/public (foo x y)
;   (apply + (map (curryr apply (list x y)
;                               (list (λ (x y) (+ x y))
;                                     (λ (x y) (- x y)))))))
; wich evaluates to the defintion of a function that computes
; (* (+ x y) (- x y))
(define (combine-method meta gf)
  (let* ([name (get-field name gf)]
         [params (get-field params gf)]
         [combination (get-field combination gf)]
         [functions (send gf applicable-methods meta)])
    (list 'define/public
          (cons name params)
          ; '+  '(3 4 5)  -> '(+ 3 4 5)
          (list 'apply
                combination
                ; '(1 2)  '(f g h) -> '((f 1 2) (g 1 2) (h 1 2))
                (list 'map
                      (list 'curryr
                            'apply
                            (cons 'list params))
                      (cons 'list functions))))))
  
; ---------------- COPY OF CPL FROM SWINDLE ----------------

; A copy of the topological sort implemented in CLOS
; See racket/pkgs/swindle/tiny-clos.rkt
(define (compute-std-cpl c get-direct-supers)
  (top-sort (build-transitive-closure get-direct-supers c)
            (build-constraints get-direct-supers c)
            (std-tie-breaker get-direct-supers)))

(define (top-sort elements constraints tie-breaker)
  (let loop ([elements elements] [constraints constraints] [result '()])
    (if (null? elements)
      result
      (let ([can-go-in-now
             (filter (lambda (x)
                       (every (lambda (constraint)
                                (or (not (eq? (cadr constraint) x))
                                    (memq (car constraint) result)))
                              constraints))
                     elements)])
        (if (null? can-go-in-now)
          (error 'top-sort "contradictory class precedence list")
          (let ([choice (if (null? (cdr can-go-in-now))
                          (car can-go-in-now)
                          (tie-breaker result can-go-in-now))])
            (loop (filter (lambda (x) (not (eq? x choice))) elements)
                  constraints (append result (list choice)))))))))

(define (std-tie-breaker get-supers)
  (lambda (partial-cpl min-elts)
    (let loop ([pcpl (reverse partial-cpl)])
      (let* ([current-elt (car pcpl)]
             [ds-of-ce (get-supers current-elt)]
             [common (filter (lambda (x) (memq x ds-of-ce)) min-elts)])
        (if (null? common)
          (if (null? (cdr pcpl))
            (error 'std-tie-breaker "nothing valid") (loop (cdr pcpl)))
          (car common))))))

(define (build-transitive-closure get-follow-ons x)
  (let track ([result '()] [pending (list x)])
    (if (null? pending)
      result
      (let ([next (car pending)])
        (if (memq next result)
          (track result (cdr pending))
          (track (cons next result)
                 (append (get-follow-ons next) (cdr pending))))))))

(define (build-constraints get-follow-ons x)
  (let loop ([elements (build-transitive-closure get-follow-ons x)]
             [this-one '()]
             [result '()])
    (if (or (null? this-one) (null? (cdr this-one)))
      (if (null? elements)
        result
        (loop (cdr elements)
              (cons (car elements) (get-follow-ons (car elements)))
              result))
      (loop elements
            (cdr this-one)
            (cons (list (car this-one) (cadr this-one)) result)))))

;------------------------ Tests -----------------------------

#|

The left class hirarchy was used for testing inheritance of slots and standard method combination.
For method combination, we'll use a simpler, dimond shaped hirarchy with three generic functions and different method distributions (right)

        0 - object%            0 - object%
      __|__                    |
     / / \ \                  one
    1  2 3  4                /   \ 
   / \/   \/ \            two   three
  8   5   6  /               \   /
       \ /__/                 four
        7
|#

#|
(display "------------ Tests ------------\n")
(display "<test name>:      <expected> / <observed>\n\n")

;                        supers                            fields               methods
(define test1 (my-class ()                  (super-new)   (field [a 1])       (define/public (foo) a)))
(define test2 (my-class ()                  (super-new)   (field [b 2])       (define/public (foo) b)))
(define test3 (my-class ()                  (super-new)   (field [c 3] [d 3]) (define/public (foo) c)))
(define test4 (my-class ()                  (super-new)   (field [a 4])       (define/public (bar) 'bar)))
(define test5 (my-class (test1 test2)       (super-new)))
(define test6 (my-class (test3 test4)       (super-new)))
(define test7 (my-class (test5 test6 test4) (super-new)))
(define test8 (my-class (test1)             (super-new)   (inherit-field a)   (define/public (baz) a)))

(define cpl (get-field class-precedence-list (find-class test7)))

(display "class 7 cpl:      '(7 5 1 2 6 3 4 0) / ")
(map (curry dynamic-get-field 'number) cpl)
(display "class 7 fields:   '((field (a 1)) (field (b 2)) (field (c 3)) (field (d 3))) / ")
(send (find-class test7) effective-fields)
(display "class 7 methods:  '((define/public (foo) a) (define/public (bar) 'bar)) / ")
(send (find-class test7) effective-methods)
(display "class 8 fields:   '((field (a 1))) / ")
(send (find-class test8) effective-fields)
(display "class 8 methods:  '((define/public (baz) a) (define/public (foo) a)) / ")
(send (find-class test8) effective-methods)
(display "object 7 - a:     1 / ")
(get-field a (new test7))
(display "object 7 - foo(): 1 / ")
(send (new test7) foo)
(display "object 8 - a:     1 / ")
(get-field a (new test8))
(display "object 8 - baz(): 1 / ")
(send (new test8) baz)
(display"\n")
(display "(method->λ '(define/public (foo x y) (+ x y))) -> ")
(method->λ '(define/public (foo x y) (+ x y)))
(display"\n")

(define one (my-class () (super-new)
                      (define/generic (foo) list)
                      (define/public (foo) 1)
                      (define/generic (bar x y) list)
                      (define/public (bar x y) (+ x y))
                      (define/generic (number) list)))

(display "one foo:     '(1)   / ")
(send (new one) foo)
(display "one bar:     '(3)   / ")
(send (new one) bar 1 2)

(define two (my-class (one) (super-new)
                      (define/public (bar x y) (* x y))
                      (define/public (number) 'two)))

(display "two foo:     '(1)   / ")
(send (new two) foo)
(display "two bar:     '(2 3) / ")
(send (new two) bar 1 2)

(define three (my-class (one) (super-new)
                        (define/public (number) 'three)))

(define four (my-class (two three) (super-new)
                       (define/public (number) 'four)))

(display "four number: '(four two three) / ")
(send (new four) number)
|#