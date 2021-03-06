#lang racket

(require "cpl.rkt")

#|

This module introduces the following additional class options:
* specifying a list of superclasses
* defining a method combination

    (class (first second ...)
        ...
        (define/generic (methodname args...) combination)
        ...
    )

The superclass specifier can be either a single class (as in standard Object Racket) or an unquoted, possibly empty list of classes.

The combination can be any operator that can be applied to a list of arguments, e.g. +, list, append.

-----------------------------------------------------

This module is a prototype for extending the class macro for multiple inheritance without touching the underlying implementation of Racket's object system.

As long as only single inheritance is used there should be no noticable difference between using this module and not using it. When the macro is used with multiple superclasse, the class will inherit fields and methods from all superclasses depending on the precedence of the classes or the specified combination. The result is a normal Racket object. The actual superclass will be object% and the code of all inherited fields and methods will be injected into the class macro by hand. 

The implementation uses a metaobject protocol (MOP) that loosely follows "The Art of of the Metaobject Protocol" (AMOP) by Gregor Kiczales, Jim des Rivières and Daniel G. Bobrow. Instead of defining an own hidden layer, it uses the existing Racket object system as basis though: Whenever a Racket class is created, we'll keep track of it and store some additional info about it in form of a(nother) Racket object that we call meta-object. The meta-object keeps track of everything that we won't be (easily) able to ask the class after creation. The class that defines those meta-objects is consequently called meta-class. 

This module was created by Manuela Beckert as master thesis project. The corresponding (German) thesis is titled "Untersuchungen zur Integration von CLOS-Konzepten in das Objektsystem von Racket" and will be accessible at the library in the computer science department of the University of Hamburg, Germany. 
|#

; ---------------------- MACRO -----------------------

; enable us to use eval in the definitions window
(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (my-eval x) (eval x ns))

; provide new version of the class macro
(provide (rename-out [my-class class]))

; redefine class macro.
; Instead of only a single superclass, this macro allows
; to also specify a (possibly empty) list of superclasses.
; Thus, there are three cases: That we got
; * a single class
; * an empty list (we'll treat this as object%)
; * a non-empty list
; TODO: class*, class/derived ?
(define-syntax my-class
  (syntax-rules ()
    [(my-class () . rest)
     (expand! #f (list object%) 'rest)]
    [(my-class (super ...) . rest)
     (expand! #f (list super ...) 'rest)]
    [(my-class super . rest)
     (expand! #t (list super) 'rest)]))

; The entry point where everything begins!!
; * creates a class object and returns it
; * classes with a single superclass will be handled solely
;   by Object Racket, we'll just collect some info on them
; * classes with multiple suplerclasses will be put together
;   here; they will have object% as "official" superclass
;   because we'll do all the inheritance work already and
;   Object Racket shall not do it again
; * information that we'll need later about the class object
;   will be stored in a meta object
(define (expand! do-not-touch? supers args)
  ; create meta object
  (let ([meta (make-metaobject supers args)])
    (update-generic-functions meta)
    ; create actual class object
    (let ([obj (make-classobject do-not-touch? supers args meta)])
      ; add the new class to the list of observed classes
      (add-class obj meta)
      ; return the class object
      obj)))

; creates a metaobject.
; supers - the list of superclasses
; args - other arguments passed to the class macro
(define (make-metaobject supers args)
  (let* ([direct-supers     (map find-class supers)]  
         [direct-slots      (compute-direct-slots args)]
         [direct-methods    (filter method? args)]
         [generic-functions (filter defgeneric? args)]
         [meta (make-object meta-class% direct-supers direct-slots
                 direct-methods generic-functions)])
    (finalize-inheritance meta)
    meta))

;; creates a class object
(define (make-classobject do-not-touch? supers args meta)
  ; remove define/generic forms
  (let ([args (filter (negate defgeneric?) args)])
    (if do-not-touch?
        ; if we're not allowed to touch it, 
        ; let racket handle object creation
        (my-eval (append '(class) supers args))
        ; else, put together the superclass
        (let ([superclass (my-eval (append '(class object% (super-new))
                                          (get-field inherited-slots meta)
                                          ; only add methods that are not part of a
                                          ; method combination
                                          (filter (negate is-generic?)
                                                  (get-field inherited-methods meta))))])
          ; and use it for the new class
          (my-eval (append '(class)
                           (list superclass)
                           ; add combination methods
                           (map (curry combine-method meta)
                                (remove-duplicates
                                 (map get-name
                                      (filter is-generic?
                                              (append (get-field direct-methods meta)
                                                      (get-field inherited-methods meta))))))
                           ; other args
                           (filter (negate is-generic?) args)))))))

; --------------------- CLASSES ----------------------

; since classes have no name, we count them, so we
; can distinguish them for debugging
(define num-of-classes -1)

; class for class meta objects.
; meta objects store additional information for a class object
(define meta-class%
  (class object% (super-new)
    
    ;; init fields
    (init-field direct-supers
                direct-slots
                direct-methods
                generic-functions)

    ; fields
    (field [number (begin 
                     (set! num-of-classes (+ 1 num-of-classes))
                     num-of-classes)]
           [class-precedence-list '()]
           [inherited-slots '()]
           [inherited-methods '()])    

    ; effective slots (for testing)
    (define/public (effective-slots)
      (append direct-slots inherited-slots))

    ; effective methods (for testing)
    (define/public (effective-methods)
      (append direct-methods inherited-methods))))

(define (finalize-inheritance meta)
  (let ([cpl (compute-cpl meta)])
    (set-field! class-precedence-list meta cpl)
    (set-field! inherited-slots       meta (compute-inherited-slots (cdr cpl)))
    (set-field! inherited-methods     meta (compute-inherited-methods (cdr cpl)))))

; table that maps each class object to its meta object
(define class-table (make-hasheq))

(define (find-class obj)
  (hash-ref class-table obj))

(define (add-class obj metaclass)
  (hash-set! class-table obj metaclass))

; add object% and its meta object to the table
(define meta-object%
  (make-object meta-class% '() '() '() '()))

(add-class object% meta-object%)

; compute the class precedence list
; * TODO: this is way simpler than the topological sort in AMOP or  
;   the CLOS implementation and I need to figure out whether that's
;   a good thing or not
;(define (compute-cpl classes [result '()])
;  ; remove object% from the list since we'll add it once at the end
;  (let ([classes (remove* (list meta-object%) classes)])
;    (if (empty? classes)
;        ; once we saw all classes, add object% and return the list
;        (reverse (cons meta-object% result))
;        ; add current class to result list and
;        ; its superclasses to the list of classes to be processed
;        (let* ([elem (car classes)]
;               [elem-supers (get-field direct-supers elem)])
;          (compute-cpl (remove-duplicates (append (cdr classes)
;                                                  elem-supers))
;                       (cons elem result))))))
(define (compute-cpl meta)
  (compute-std-cpl meta (lambda (obj) (get-field direct-supers obj))))

;; slots and methods

; define what a slot/method definition looks like
; TODO: consider whether it actually makes a difference to
;       directly return the value of the member function (a 
;       list contianing the field name instead of #t)
(define (slot? s)
  (if (member (car s) '(field init-field))
      #t
      #f))

(define (method? m)
  ;(string-prefix? (symbol->string (car m)) "define/"))
  ; -> problem: we only keep instances of the class with
  ;    highest precence
  (equal? (car m) 'define/public))

(define (defgeneric? m)
  (equal? (car m) 'define/generic))

; extract slot definitions from args
(define (compute-direct-slots args)
  (let ([slots (filter slot? args)])
    (if (empty? slots)
        '()
        (apply append (map split-1 slots)))))

; splits field-declarations with multiple values
; (field [a 1] [b 2]) -> ((field [a 1]) (field [b 2])
(define (split-1 slot)
  (map (curry list (car slot)) (cdr slot)))

; computes the list of inherited slots and methods.
; since both are very similar, the main logic is in
; compute-super-stuff. Just substitute "stuff"
; for "slots" or "methods" in your head.
(define (compute-inherited-slots cpl)
  (compute-inherited-stuff 'direct-slots cpl))

(define (compute-inherited-methods cpl)
  (compute-inherited-stuff 'direct-methods cpl))

(define (compute-inherited-stuff direct-stuff-field cpl)
  (let ([stuff '()])
    ; add the slots/methods of all superclasses to the list
    (for*/list ([metas cpl])
      (set! stuff (append stuff 
                          (dynamic-get-field direct-stuff-field metas))))
    ; only keep the first appearance of every slot/method
    ; declaration
    (filter-first-occurence stuff)))

; Filters a list of slot or method definitions so that
; only the first declaration of each slot/method
; is kept:
; '((field (a 1)) (field (a 2)) (field b) (field (b 0)))
;  -> '((field (a 1)) (field b))
; TODO: This is probably very inefficient!!
(define (filter-first-occurence xs)
  (remove-duplicates (map (curryr first-occurence xs) xs)))

(define (first-occurence elem xs)
  (car (filter (lambda (x) (equal? (get-symbol x)
                                   (get-symbol elem)))
               xs)))

(define (get-symbol x)
  (if (symbol? (cadr x))
      (cadr x)
      (caadr x)))


; ----------------- GENERIC FUNCTIONS -----------------------

; class for generic function objects
(define generic-function%
  (class object% (super-new)
    (init-field name)
    (init-field params)
    (init-field meta-class)
    (init-field combination)        ; quoted operator
    (field [methods (make-hasheq)]) ; (meta, (λ ...)) 

    (define/public (number-of-params)
      (length params))
    
    (define/public (add-method meta method)
      (hash-set! methods meta method))

    ; returns a list of all applicable function
    ; (as quoted lambda functions), sorted by cpl
    (define/public (get-applicable-methods meta)
      (let* ([cpl (get-field class-precedence-list meta)]
             [applicable  (map (curry hash-ref methods)
             (filter (curry hash-has-key? methods) cpl))])
        (if (empty? applicable)
            (error "no applicable methods found for generic function" name)
            applicable)))))

; table that maps each generic function name to its object
(define generic-function-table (make-hash))

(define (get-generic name)
  (hash-ref generic-function-table name))

(define (add-generic name meta)
  (hash-set! generic-function-table name meta))

(define (is-generic? x)
  (and (or (method? x)
           (defgeneric? x))
       (hash-has-key? generic-function-table (get-name x))))

(define (get-name method)
  (car (second method)))

; Updates generic functions
(define (update-generic-functions meta)
  ; keep track of new generic functions
  (map (curryr make-generic-function meta)
       (get-field generic-functions meta))
  ; add  new methods to existing generic functions
  (for ([method (get-field direct-methods meta)])
    ; for each new method, if a generic function for it exists
    (when (is-generic? method)
      ; add new method to generic function
      (send (get-generic (get-name method))
            add-method
            meta
            (method->λ method)))))

; Creates the generic function object and adds it to the list
; If it already exists, an error will be signaled
; gf: '(define/generic (name args...) combination)
; combination: quoted operator
; meta-class: class metaobject
; 
(define (make-generic-function gf meta)
  (let ([name (car (second gf))]
        [params (cdr (second gf))]
        [combination (third gf)]) 
    (if (hash-has-key? generic-function-table name)
        (error "duplicate definition of generic function" name)
        (hash-set! generic-function-table 
                   name
                   (make-object generic-function% name
                     params meta combination)))))

; Converts a method expression into a quoted lambda function
; '(define/public (foo x y) (+ x y)) -> '(λ (x y) (+ x y))
(define (method->λ method)
  (let ([m (list 'λ (cdr (second method)) (third method))])
;    (display m)
;    (display "\n")
    m))

; Returns a (quoted) function that combines all applicable methods.
; The parameters (params) of the resulting lambda function are
; first applied to all applicable methods. On the resulting values
; we'll apply the combination function. 
(define (combine-method meta name)
  (let* ([gf (get-generic name)]
         [params (get-field params gf)]
         [combination (get-field combination gf)]
         [functions (send gf get-applicable-methods meta)]
         [f (list 'define/public
                  (cons name params)
                  ; '+  '(3 4 5)  -> '(+ 3 4 5)
                  (list 'apply
                        combination
                        ; '(f g h)  '(1 2) -> '((f 1 2) (g 1 2) (h 1 2))
                        (list 'map
                              (list 'curryr
                                    'apply
                                    (cons 'list params))
                              (cons 'list functions))))])
    f))
  

;------------------------ Tests -----------------------------

#|      0 - object%
      __|__ 
     / / \ \
    1  2 3  4
   / \/   \/ \
  8   5   6  /
       \ /__/
        7
|#

(display "------------ Tests ------------\n")
(display "<test name>:      <expected> / <observed>\n\n")

;                        supers                            slots               methods
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
(display "class 7 slots:    '((field (a 1)) (field (b 2)) (field (c 3)) (field (d 3))) / ")
(send (find-class test7) effective-slots)
(display "class 7 methods:  '((define/public (foo) a) (define/public (bar) 'bar)) / ")
(send (find-class test7) effective-methods)
(display "class 8 slots:    '((field (a 1))) / ")
(send (find-class test8) effective-slots)
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


(define thing (my-class () (super-new)
                        (field [x 42])
                        (define/generic (attack) list)))

(define element (class thing (super-new)
                  (inherit-field x)
                  (init-field [attr 'water])
                  (define/public (attack) attr)))

(define animal (my-class thing (super-new)
                 (init-field [size 'small])
                 (define/public (attack) size)))

;(define pokemon (my-class (element animal) (super-new)
;                          (field [form 'ball])
;                          (define/public (attack) form)
;                          (inherit-field attr size)))