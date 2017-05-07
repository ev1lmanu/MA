#lang racket

; ------------ Klassen und Objekte ---------------
(define Thing (class object% (super-new)
                (init-field [i-am-a " Thing"])
                (define/public (what-am-i?) 
                  (string-append "I am a" i-am-a "!"))))

; (new Thing)
; (send (new Thing what-am-i?))

(define Element (class Thing (super-new [i-am-a "n Element"])
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))))

(define elem (new Element))
; (send elem what-am-i?)
; (get-field attr elem)
; (set-field! elem attr 'wind)
; (send elem hot?)

(define Animal (class Thing (super-new [i-am-a "n Animal"])
                 (init-field [gender 'male]
                             [size 'small])))

; (send (new Animal) what-am-i?)


; ------------------ Mixins ---------------------

(define (generate-subclass superclass)
  (class superclass (super-new)))

(define (Element-mixin %)
  (class % (super-new) ; ! (super-new [i-am-a "n Element"])
    (init-field [attr 'water])
;    (define/public (attack) attr) ; !
    (define/override (attack) (list (super attack) attr))
    ))

(define (Animal-mixin %)
  (class % (super-new) ; ! (super-new [i-am-a "n Animal"])
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

; ------------------ Traits ---------------------

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
                  (class Thing (super-new [i-am-a " Pokemon"])
                    (init-rest args))))

(define p2 (new Pokemon2))
; (send p2 what-am-i?)
; (send p2 attack)
(define p3 (make-object Pokemon2 'large 'female 'fire))
; (send p3 attack)

; ------------ Ergänzungsmethoden ---------------

(define TheNumber (class object% (super-new)
                     (define/public (number)
                       (display "The number is ")
                       23)))

(define Sub (class TheNumber (super-new)
              (define/override (number)
                (display "Actually ")
                (+ (super number) 19))))

; (send (new TheNumber) number)
; (send (new Sub) number)

(define TheNumber2 (class object% (super-new)
                  (define/pubment (number)
                    (inner (void) number)
                    (display "The number is ")
                    23)))

(define Sub2 (class TheNumber2 (super-new)
                    (define/augment (number)
                      (display "Believe it! "))))

; (send (new TheNumber2) number)
; (send (new Sub2) number)

(define TheNumber3 (class object% (super-new)
                     (define/pubment (number)
                       (display "The number is ")
                       (display (inner 23 number)))))

(define Sub3 (class TheNumber3 (super-new)
              (define/augment (number)
                42)))

; (send (new TheNumber3) number)
; (send (new Sub3) number)
