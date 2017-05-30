#lang racket
#|
This module is a prototype for extending the class macro for multiple inheritance without touching the underlying implementation of Racket's object system.

As long as only single inheritance is used there should be no noticable difference between using this module and not using it. When the macro is used with multiple superclasse, the class will inherit fields and methods from all superclasses depending on the precedence of the classes or the specified combination. The result is a normal Racket object. The actual superclass will be object% and the code of all inherited fields and methods will be injected into the class macro by hand. 

The implementation uses a very simple metaobject protocol (MOP) that loosely follows "The Art of of the Metaobject Protocol" (AMOP) by Gregor Kiczales, Jim des Rivières and Daniel G. Bobrow. Instead of defining an own hidden layer, it uses the existing Racket object system as basis though: Whenever a Racket class is created, we'll keep track of it and store some additional info about it in form of a(nother) Racket object that we call meta-object. The meta-object keeps track of everything that we won't be (easily) able to ask the class after creation. The class that defines those meta-objects is consequently called meta-class. 

This module was created by Manuela Beckert as master thesis project. The corresponding (German) thesis is titled "Untersuchungen zur Integration von CLOS-Konzepten in das Objektsystem von Racket" and will be accessible at the library in the computer science department of the University of Hamburg, Germany. 
|#

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
     (expand! #f '(object%) 'rest)]
    [(my-class (super . supers) . rest)
     (expand! #f '(super . supers) 'rest)]
    [(my-class super . rest)
     (expand! #t (super) 'rest)]))

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
  (let*
       ; create meta object
      ([meta (make-metaobject supers args)]
       ; create actual class object
       [obj (make-object do-not-touch? supers args meta)])
    ; complete class precedence list
    ; TODO: needed?
    (send meta add-self-to-cpl obj)
    ; keep track of subclasses
    (add-subclass obj (get-field direct-supers meta))
    ; add the new class to the list of observed classes
    (add-class obj meta)
    ; return the class object
    obj))

; since classes have no name, we count them, so we
; can distinguish them for debugging
(define num-of-classes -1)

(define (number-of obj)
  (get-field number (find-class obj)))

; class for meta objects.
; meta objects store additional information for a class object
(define meta-class%
  (class object% (super-new)
    
    ;; fields
    ;; getters: (get-field  field obj)
    ;; setters: (set-field! field obj value)
    (field [number (begin 
                     (set! num-of-classes (+ 1 num-of-classes))
                     num-of-classes)])
    (init-field direct-supers)         ; direct: own slots and
    (init-field class-precedence-list) ; methods, etc.
    (init-field direct-slots)          ; super: inherited slots,
    (init-field super-slots)           ; methods, etc.
    (init-field direct-methods)
    (init-field super-methods)
    (init-field direct-subclasses) 
    
    ; adds the class object to its CPL
    ; (this is only called once after object creation)
    (define/public (add-self-to-cpl self)
      (set! class-precedence-list
            (cons self class-precedence-list)))
    ; adds a direct subclass to the list
    (define/public (add-direct-subclass subclass)
      (set! direct-subclasses
            (cons subclass direct-subclasses)))

    ; effective slots
    (define/public (effective-slots)
      (append direct-slots super-slots))

    ; effective methods
    (define/public (effective-methods)
      (append direct-methods super-methods))))

; table that maps each class object to its meta object
(define class-table (make-hasheq))

(define (find-class obj)
  (hash-ref class-table obj))

(define (add-class obj metaclass)
  (hash-set! class-table obj metaclass))

; add object% and its meta object to the table
(define meta-object%
  (new meta-class%
       [direct-supers '()]
       [direct-slots '()]
       [class-precedence-list (list object%)]
       [super-slots '()]   ; object% has no fields or methods
       [direct-subclasses '()]
       [direct-methods '()] 
       [super-methods '()]))

(add-class object% meta-object%)

; creates a metaobject.
; supers - the list of superclasses
; args - other arguments passed to the class macro
(define (make-metaobject supers args)
  (let* ([direct-supers (map my-eval supers)]
         [cpl           (compute-cpl direct-supers)] 
         [direct-slots  (compute-direct-slots args)]
         [super-slots   (compute-super-slots cpl)]
         [direct-methods (filter method? args)]
         [super-methods  (compute-super-methods cpl direct-methods)])
    (new meta-class% 
         [direct-supers direct-supers]
         [direct-slots direct-slots]
         [class-precedence-list cpl]
         [super-slots super-slots]
         [direct-methods direct-methods]
         [super-methods super-methods]
         [direct-subclasses '()])))

; compute the class precedence list
; * the class object itself will be created later, so it
;   cannot be added yet in this method
; * TODO: this is way simpler than the topological sort in AMOP or  
;   the CLOS implementation and I need to figure out whether that's
;   a good thing or not
(define (compute-cpl classes [result '()])
  ; remove object% from the list since we'll add it once at the end
  (let ([classes (filter (negate (curry eq? object%)) classes)])
    (if (empty? classes)
        ; once we saw all classes, add object% and return the list
        (reverse (cons object% result))
        ; add current class to result list and
        ; its superclasses to the list of classes to be processed
        (let* ([elem (car classes)]
               [elem-supers (get-field direct-supers (find-class elem))])
          (compute-cpl (remove-duplicates (append (cdr classes)
                                                  elem-supers))
                       (cons elem result))))))

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
  ; -> Problem: Behalten nur Instanz mit höchster Präzedenz
  (equal? (car m) 'define/public))

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

; computes the list of effective slots and methods.
; since both are very similar, the main logic is in
; compute-super-stuff. Just substitute "stuff"
; for "slots" or "methods" in your head.
(define (compute-super-slots cpl)
  (compute-super-stuff 'direct-slots cpl))

(define (compute-super-methods cpl direct-methods)
  (let ([direct-symbols (map get-symbol direct-methods)])
    ; remove methods that are re-declared with define/public
    ; in the current class
    ; Why we need to do this: We are only keeping track of methods
    ; declared with define/public. This means that a subclass cannot
    ; declare them again as public, but must use override - but then
    ; we won't track this overridden method. Even if we did change
    ; the method? function to include redefinitions, we then would
    ; not only need to track the definition of the redefinition, but
    ; the method will depend on the definition of its super method
    ; and then things will get ugly...
    ; So to allow methods to be redeclared and accessible for method
    ; combination we'll allow redeclaration with define/public
    (filter (lambda (x) (not (member (get-symbol x) direct-symbols)))
            (compute-super-stuff 'direct-methods cpl))))

(define (compute-super-stuff direct-stuff-field cpl)
  (let ([stuff '()])
    ; add the slots/methods of all superclasses to the list
    (for*/list ([metas (map find-class cpl)])
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

;; object
(define (make-object do-not-touch? supers args meta)
  (if do-not-touch?
                ; if we're nt allowed to touch it, 
                ; let racket handle object creation
                (my-eval (append '(class) supers args))
                ; else, put together the superclass
                (let ([superclass
                       (my-eval (append '(class object% (super-new))
                   (get-field super-slots meta)
                   (get-field super-methods meta)))])
                  ; and use it for the new class
                  (my-eval (append '(class)
                                   (list superclass)
                                   args)))))

;; subclasses

; adds the class obj as subclass to all specified supers
(define (add-subclass obj supers)
  (for*/list ([metas (map find-class supers)])
    (dynamic-send metas 'add-direct-subclass obj)))

;----------------------------- Tests -----------------------------

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

;                        supers                            slots               methods
(define test1 (my-class ()                  (super-new)   (field [a 1])       (define/public (foo) a)))
(define test2 (my-class ()                  (super-new)   (field [b 2])       (define/public (foo) b)))
(define test3 (my-class ()                  (super-new)   (field [c 3] [d 3]) (define/public (foo) c)))
(define test4 (my-class ()                  (super-new)   (field [a 4])       (define/public (bar) 'no)))
(define test5 (my-class (test1 test2)       (super-new)))
(define test6 (my-class (test3 test4)       (super-new)                       (define/public (bar) 'yes)))
(define test7 (my-class (test5 test6 test4) (super-new)))
(define test8 (my-class (test1)             (super-new)   (inherit-field a)   (define/public   (baz) a) (define/public (foo) 'yes)))
(define cpl (get-field class-precedence-list (find-class test7)))

(display "object% subclasses: ")
(map number-of (get-field direct-subclasses (find-class object%)))
(display "class 1 subclasses: ")
(map number-of (get-field direct-subclasses (find-class test1)))
(display "class 7 cpl:        ")
(map number-of cpl)
(display "class 7 slots:      ")
(send (find-class test7) effective-slots)
(display "class 7 methods:    ")
(send (find-class test7) effective-methods)
(display "class 8 slots:      ")
(send (find-class test8) effective-slots)
(display "class 8 methods      ")
(send (find-class test8) effective-methods)
(display "object 7 - a:       ")
(get-field a (new test7))
(display "object 7 - foo():   ")
(send (new test7) foo)
(display "object 8 - a:       ")
(get-field a (new test8))
(display "object 8 - baz():   ")
(send (new test8) baz)
