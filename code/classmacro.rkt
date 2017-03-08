#lang racket
;; This module extends the class macro for multiple inheritance.
;; It uses a very simple metaobject protocol (MOP) that follows "The Art of of the Metaobject Protocol" (AMOP) by Gregor Kiczales, Jim des Rivières and Daniel G. Bobrow. Instead of defining an own hidden layer, it uses the existing Racket object system as basis though.

;; This module was created by Manuela Beckert as master thesis project. The corresponding (German) thesis is titled "Untersuchungen zur Integration von CLOS-Konzepten in das Objektsystem von Racket" and accessible at the library in the computer science department of the University of Hamburg, Germany.

(provide (rename-out [my-class class]))

; redefine class macro
(define-syntax my-class
  (syntax-rules ()
    [(my-class () . rest)
     (expand! '(object%) 'rest)]
    [(my-class (super . supers) . rest)
     (expand! '(super . supers) 'rest)]
    [(my-class super . rest)
     (expand! '(super) 'rest)]))

; enable us to use eval in the definitions window
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (my-eval x)
  (eval x ns))

; the entry point where everything begins!!
; * creates a class object and returns it
; * all classes will simply have object% as "official"
;   superclass, since we'll provide all inheritance behavior
;   ourselves and do not want Object Racket to interfere with that
; * creates a meta object for the class that stores all additional
;   information we need and adds it to the table of metaobjects
(define (expand! supers args)
  (let* ([meta (make-metaobject supers args)]
         [obj (my-eval
                (append
                 '(class object% (super-new))
                (send meta get-direct-slots)
                (send meta get-direct-methods)))])
    (send meta add-self-to-cpl obj)
    (add-class obj meta)
    obj))

; since classes have no name, we count them, so we
; can distinguish them for debugging later
(define num-of-classes -1)

; class for meta objects
; meta objects store additional information for a class object
(define meta-class
  (class object% (super-new)
    
    ;; fields
    (field [number (begin 
                     (set! num-of-classes (+ 1 num-of-classes))
                     num-of-classes)])
    (init-field direct-supers)
    (init-field direct-slots)    ; fields (not inherited)
    (init-field class-precedence-list)
    (init-field effective-slots) ; fields (including inherited)
    (init-field direct-subclasses)
    (init-field direct-methods)

    ;; getters
    (define/public (get-number) number) 
    (define/public (get-direct-supers) direct-supers)
    (define/public (get-direct-slots)  direct-slots)
    (define/public (get-class-precedence-list) class-precedence-list)
    (define/public (get-effective-slots) effective-slots)
    (define/public (get-direct-subclasses) direct-subclasses)
    (define/public (get-direct-methods) direct-methods)

    ;; setters
    ; adds the class object to its CPL
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
  (new meta-class
       [direct-supers '()]
       [direct-slots '()]
       [class-precedence-list (list object%)]
       [effective-slots '()]
       [direct-subclasses '()]
       [direct-methods '()]))

(add-class object% meta-object%)

; creates a metaobject, given the list of superclasses and
; other arguments
(define (make-metaobject supers args)
  (let* ([direct-supers (compute-direct-supers supers)]
         [direct-slots  (compute-direct-slots args)]
         [cpl           (compute-cpl direct-supers)] 
         [effective-slots (compute-effective-slots cpl direct-slots)]
         [direct-methods (compute-direct-methods args)])
    
    (new meta-class 
         [direct-supers direct-supers]
         [direct-slots direct-slots]
         [class-precedence-list cpl]
         [effective-slots effective-slots]
         [direct-subclasses '()]
         [direct-methods direct-methods])))

; compute the super class objects from the list of superclass names
(define (compute-direct-supers supers)
  (map (curryr eval ns) supers))

; extracts fields from the list of arguments
(define (compute-direct-slots args)
  (filter (lambda (arg) (or (equal? (car arg) 'field)
                            (equal? (car arg) 'init-field)))
          args))

; compute the class precedence list
; * the class object itself will be created later, so it
;   cannot be added yet in this method
; * this is way simpler than the topological sort in the CLOS
;   implementation and I need to figure out whether that's a
;   good thing or not
(define (compute-cpl classes [result '()])
  ; remove object% from the list, since we'll add it once at the end
  (let ([classes (filter (negate (curry eq? object%)) classes)])
    (if (empty? classes)
        ; once we saw all classes, add object and return the list
        (reverse (cons object% result))
        ; add current class to result list and
        ; its superclasses to the list of classes to be processed
        (let* ([elem (car classes)]
               [elem-supers (send (find-class elem)
                                  get-direct-supers)])
          (compute-cpl (remove-duplicates
                        (append (cdr classes)
                                elem-supers))
                       (cons elem result))))))

(define (compute-effective-slots cpl direct-slots)
  direct-slots) ; TODO

(define (add-subclass obj direct-supers)
  (void)) ; TODO

(define (compute-direct-methods args)
  '()) ; TODO

;----------------------------- Tests -----------------------------
#|
(define test1 (my-class () (super-new)))
(define test2 (my-class () (super-new)))
(define test3 (my-class () (super-new)))
(define test4 (my-class () (super-new)))
(define test5 (my-class (test1 test2) (super-new)))
(define test6 (my-class (test3 test4) (super-new)))
(define test7 (my-class (test5 test6) (super-new)))

(define cpl (send (find-class test7) get-class-precedence-list))

(define meta (map find-class cpl))
(send (first meta) get-number)  ;7
(send (second meta) get-number) ;5
(send (third meta) get-number)  ;6
(send (fourth meta) get-number) ;1
(send (fifth meta) get-number)  ;2
(send (sixth meta) get-number)  ;3
(send (seventh meta) get-number);4
(send (eighth meta) get-number) ;object%
(send (ninth meta) get-number)  ;error
|#