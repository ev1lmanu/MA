#lang racket

; ------------ Klassen und Objekte ---------------
(define Thing (class object% (super-new)))

; (new Thing)

(define Element (class Thing (super-new)
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))))

(define elem (new Element))
; (get-field attr elem)
; (set-field! elem attr 'wind)
; (send elem hot?)

(define Animal (class Thing (super-new)
                 (init-field [gender 'male]
                             [size 'small])))

(define Pet (class Animal (super-new)
              (init-field [name 'unknown])))

(define harry (new Pet [name 'harry] [size 'normal]))
; (get-field name harry)
; (get-field gender harry)
; (get-field size harry)


; --------------- Mixins ------------------

(define (generate-subclass superclass)
  (class superclass (super-new)))

(define (Element-mixin %)
  (class % (super-new)
    (init-field [attr 'water])
;    (define/public (attack) attr) ; !
    (define/override (attack) (list (super attack) attr))
    ))

(define (Animal-mixin %)
  (class % (super-new)
    (init-field [gender 'male]
                [size 'small])
    (define/public (attack) size)))

(define Element2 (Element-mixin
                  (class object% (super-new)
                    (define/public (attack) null))))
; (get-field attr (new Element2))

(define Animal2 (Animal-mixin Thing))
; (get-field size (new Animal [size 'huge] [gender 'female]))

(define Pokemon (Element-mixin (Animal-mixin Thing)))
; (define Pokemon (Animal-mixin (Element-mixin Thing))) ; !

; (get-field size (new Pokemon))
; (get-field attr (new Pokemon))
; (get-field gender (new Pokemon))
; (send (new Pokemon) attack)
(define p (new Pokemon [size 'large]
                       [gender 'female]
                       [attr 'fire]))
; (get-field size p)
; (get-field gender p)
; (get-field attr p)
; (send p attack)

; --------------- Traits ------------------

(require racket/trait)

(define Element-trait
  (trait (field [attr 'water])
         (define/public (attack) attr)))

(define Animal-trait
  (trait (field [gender 'male]
                [size 'small])
         (define/public (attack) size)))

(define Pokemon-trait
  ; combine the following traits
  (trait-sum
   ; create a trait with an additional method element-attack
   ; that is a duplicate of attack with trait-alias
   ; and remove the original attack method with trait-exclude
   (trait-exclude (trait-alias Element-trait  
                              attack         
                              element-attack)
                 attack)
   ; same for animal
   (trait-exclude (trait-alias Animal-trait  
                              attack         
                              animal-attack)
                 attack)
   ; combine the two new attack methods
   (trait (inherit element-attack animal-attack)
          (define/public (attack)
            (list (animal-attack) (element-attack))))))

(define Pokemon-mixin (trait->mixin Pokemon-trait))

(define Pokemon2 (Pokemon-mixin
                  (class object% (super-new)
                    (init-rest args)
                    (when (not (empty? args))
                      (display args))
                    )))

(define p2 (new Pokemon2))
; (send p2 attack)
(define p3 (make-object Pokemon2 'large 'female 'fire))

; ------------- Ergänzungsmethoden ----------------
