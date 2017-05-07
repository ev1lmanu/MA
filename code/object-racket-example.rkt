#lang racket

; ------------ Klassen und Objekte ---------------
(define Thing (class object% (super-new)
                (init-field [name "a Thing"])
                (define/public (who-are-you?) 
                  (string-append "I am " name "!"))))

; (new Thing)
; (send (new Thing who-are-you?))
; (send (make-object Thing "Bob") who-are-you?)

(define Element (class Thing
                  (init [name "an Element"])
                  (super-new [name name])
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))))

(define elem (new Element))
; (send elem who-are-you?)
; (get-field attr elem)
; (set-field! elem attr 'wind)
; (send elem hot?)

(define Animal (class Thing
                 (init [name "an Animal"])
                 (super-new [name name])
                 (init-field [gender 'male]
                             [size 'small])))

; (send (new Animal) who-are-you?)


; ------------------ Mixins ---------------------

(define (generate-subclass superclass)
  (class superclass (super-new)))

(define (Element-mixin %)
  (class % ;(super-new)
    (init [name "an Element"])
    (super-new [name name])
    (init-field [attr 'water])
    ; ! (define/public (attack) attr)
    (define/override (attack) (list (super attack) attr))
    ))

(define (Animal-mixin %)
  (class % ; (super-new)
    (init [name "an Animal"])
    (super-new [name name])
    (init-field [gender 'male]
                [size 'small])
    (define/public (attack) size)))

(define Element2 (Element-mixin
                  (class Thing (super-new)
                    (define/public (attack) null))))
; (get-field attr (new Element2))
; (send (new Element2) who-are-you?)
; (send (new Element2 [name "Fire"]) who-are-you?)

(define Animal2 (Animal-mixin Thing))
; (get-field size (new Animal [size 'huge] [gender 'female]))
; (send (new Animal2) who-are-you?)
; (send (new Animal2 [name "Cat"]) who-are-you?)

(define Pokemon (class (Element-mixin (Animal-mixin Thing))
                  (init [name "a Pokemon"])
                  (super-new [name name])))
; (define Pokemon (Animal-mixin (Element-mixin Thing))) ; !

; (get-field size (new Pokemon))
; (get-field attr (new Pokemon))
; (get-field gender (new Pokemon))
; (send (new Pokemon) who-are-you?)
; (send (new Pokemon [name "Charmander"]) who-are-you?)
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

(define Pokemon2 (class (Pokemon-mixin Thing)
                   (init [this-name "a Pokemon"]
                         [this-size 'small]
                         [this-gender 'male]
                         [this-attr 'water])
                   (super-new [name this-name])
                   (inherit-field size gender attr)
                   (set! size this-size)
                   (set! gender this-gender)
                   (set! attr this-attr)))

(define p2 (new Pokemon2))
(send p2 who-are-you?)
(send p2 attack)

(define p3 (make-object Pokemon2 "Charmander" 'large 'female 'fire))
(send p3 who-are-you?)
(send p3 attack)

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
