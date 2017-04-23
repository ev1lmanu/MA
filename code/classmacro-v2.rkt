#lang racket
#|
This module is a prototype for extending the class macro for multiple inheritance.

The purpose of this module is use in teaching, so it focuses on supporting existing class options what will be relevant for the lecture or training excercises instead of covering all possible syntax constructs and use cases.

TODO: Describe functionality that has been added

It uses a very simple metaobject protocol (MOP) that loosely follows "The Art of of the Metaobject Protocol" (AMOP) by Gregor Kiczales, Jim des Rivières and Daniel G. Bobrow. Instead of defining an own hidden layer, it uses the existing Racket object system as basis though:

Whenever a Racket class is created, we'll keep track of it and store some additional info about it. We'll want to know for example which superclasses the class has or which fields and methods it defines, so we can later use them to decide which of them will be (multiple) inherited.
All that info is stored in form of a(nother) Racket object that we call meta-object. The meta-object keeps track of everything that we won't be (easily) able to ask the class later, after it has been created. The class that defines those meta-objects is consequently called meta-class. The mapping of a class object to its meta-object is stored in a hashmap.

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
     (expand! '(object%) 'rest)]
    [(my-class (super . supers) . rest)
     (expand! '(super . supers) 'rest)]
    [(my-class super . rest)
     (expand! '(super) 'rest)]))

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
(define (expand! supers args)
  (let*
       ; create meta object
      ([meta (make-metaobject supers args)]
       [slots (get-field effective-slots meta)]
       [methods (get-field effective-methods meta)]
       ; create actual class object
       [obj (if (= 1 (length supers))
                ; if there is only one superclass, let
                ; racket handle object creation
                (my-eval (append '(class) supers args))
                ; else, put together the object ourselves
                (my-eval
                 (append '(class object%)
                         slots
                         methods
                         ; add anything else that was passed in args
                         ; (including the call to super-new)
                         ; remove* removes all elements in the first
                         ; list from the second list
                         (remove* (append slots methods) args))))])
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
    (init-field direct-supers)         ; direct: only what was 
    (init-field class-precedence-list) ;  explicitly defined in the
    (init-field direct-slots)          ;  class
    (init-field effective-slots)       ; effective: everything
    (init-field direct-methods)        ;  that's accessible in the
    (init-field effective-methods)     ;  class, including inherited
    (init-field direct-subclasses)     ;  fields, methods, etc.
    
    ; adds the class object to its CPL
    ; (this is only called once after object creation)
    (define/public (add-self-to-cpl self)
      (set! class-precedence-list
            (cons self class-precedence-list)))
    ; adds a direct subclass to the list
    (define/public (add-direct-subclass subclass)
      (set! direct-subclasses
            (cons subclass direct-subclasses)))))

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
       [effective-slots '()]   ; object% has no fields or methods
       [direct-subclasses '()]
       [direct-methods '()] 
       [effective-methods '()]))

(add-class object% meta-object%)

; creates a metaobject.
; supers - the list of superclasses
; args - other arguments passed to the class macro
(define (make-metaobject supers args)
  (let* ([direct-supers (map my-eval supers)]
         [cpl           (compute-cpl direct-supers)] 
         [direct-slots  (compute-direct-slots args)]
         [effective-slots (compute-effective-slots cpl direct-slots)]
         [direct-methods (filter method? args)]
         [effective-methods (compute-effective-methods cpl direct-methods)])
    (new meta-class% 
         [direct-supers direct-supers]
         [direct-slots direct-slots]
         [class-precedence-list cpl]
         [effective-slots effective-slots]
         [direct-methods direct-methods]
         [effective-methods effective-methods]
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
  ;(string-prefix? (symbol->string (car m)) "define/")
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
; compute-effective-stuff. Just substitute "stuff"
; for "slots" or "methods" in your head.
(define (compute-effective-slots cpl direct-slots)
  (compute-effective-stuff direct-slots 'direct-slots cpl))

(define (compute-effective-methods cpl direct-methods)
  (compute-effective-stuff direct-methods 'direct-methods cpl))

(define (compute-effective-stuff direct-stuff direct-stuff-field cpl)
  (let ([stuff direct-stuff])
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
(define test2 (my-class ()                  (super-new)   (field [b 2])       (define/public (foo) 'no)))
(define test3 (my-class ()                  (super-new)   (field [c 3] [d 3]) (define/public (foo) 'nono)))
(define test4 (my-class ()                  (super-new)   (field [a 4])       (define/public (bar) 'no)))
(define test5 (my-class (test1 test2)       (super-new)))
(display "whaaaat")
(define test6 (my-class (test3 test4)       (super-new)))
(define test7 (my-class (test5 test6 test4) (super-new)                       (define/public (bar) 'yes)))
(define test8 (my-class (test1)             (super-new)   (inherit-field a)   (define/public (baz) a)))
(define cpl (get-field class-precedence-list (find-class test7)))

(display "object% subclasses:  ")
(map number-of (get-field direct-subclasses (find-class object%)))
(display "class 1 subclasses:  ")
(map number-of (get-field direct-subclasses (find-class test1)))
(display "class 7 cpl:         ")
(map number-of cpl)
(display "class 7 slots:       ")
(get-field effective-slots (find-class test7))
(display "class 7 methods:     ")
(get-field effective-methods (find-class test7))
(display "object of 7 - a:     ")
(get-field a (new test7))
(display "object of 7 - foo(): ")
(send (make-object test7) foo)
(display "class 8 slots:       ")
(get-field effective-slots (find-class test8))
(display "object of 8 - a:     ")
(get-field a (new test8))
(display "object of 8 - baz(): ")
(send (new test8) baz)
