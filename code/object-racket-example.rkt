#lang racket

; ------------ Classes and Objects ---------------
;; A basic class Thing with a field and a public method
(define Thing (class object% (super-new)
                (init-field [name "a Thing"])
                (define/public (who-are-you?) 
                  (string-append "I am " name "!"))))

;; Object creation with new (label based), make-object (position based)
;; or instantiate (label and position based):
;> (new Thing)
;; Field access with get-field and set-field!, Method access with send.
;> (send (new Thing who-are-you?))
;> (send (make-object Thing "Bob") who-are-you?)

;; A class Element that inherits from Thing
(define Element (class Thing (super-new)
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))))

;; We can access both inherited and new behavior
(define elem (new Element))
;> (send elem who-are-you?)
;> (get-field attr elem)
;> (set-field! elem attr 'wind)
;> (send elem hot?)

;; A class Animal that also inherits from Thing
;; We can override the inherited field to supply a more appropriate
;; default behavior:
(define Animal (class Thing
                 (init [name "an Animal"]) ;; update default value
                 (super-new [name name])
                 (init-field [gender 'male]
                             [size 'small])))

;> (send (new Animal) who-are-you?)

; ------------------ Mixins ---------------------
;; The basic idea for Mixin classes
(define (generate-subclass superclass)
  (class superclass (super-new)))

;; An Element Mixin that provides everythin Element-related
(define (Element-Mixin %)
  (class % (super-new)
    (init-field [attr 'water])
    ;; Before: (define/public (attack) attr)
    (define/override (attack) (list (super attack) attr))
    ))

(define (Animal-Mixin %)
  (class % ; (super-new)
    (init [name "an Animal"])  ;; this also works for Mixins :)
    (super-new [name name])
    (init-field [gender 'male]
                [size 'small])
    (define/public (attack) size)))

;; We can still define Element and Animal
(define Element2 (Element-Mixin
                  ;; since Element-Mixin uses override, we have to
                  ;; provide a method to be overriden here.
                  (class Thing (super-new)
                    (define/public (attack) null))))
;> (get-field attr (new Element2))
;> (send (new Element2) who-are-you?)
;> (send (new Element2 [name "Fire"]) who-are-you?)

(define Animal2 (Animal-Mixin Thing))
;> (get-field size (new Animal [size 'huge] [gender 'female]))
;> (send (new Animal2) who-are-you?)
;> (send (new Animal2 [name "Cat"]) who-are-you?)

;; And additionally Pokemon
(define Pokemon (class (Element-Mixin (Animal-Mixin Thing))
                  (init [name "a Pokemon"])
                  (super-new [name name])))
;; It won't work the other way around though:
; (define Pokemon (Animal-Mixin (Element-Mixin Thing))) ; !

;; We can access all fields and methods, including attack
;> (get-field size (new Pokemon))
;> (get-field attr (new Pokemon))
;> (get-field gender (new Pokemon))
;> (send (new Pokemon) who-are-you?)
;> (send (new Pokemon [name "Charmander"]) who-are-you?)
;> (send (new Pokemon) attack)
(define p (new Pokemon [size 'large]
                       [gender 'female]
                       [attr 'fire]))
;> (get-field size p)
;> (get-field gender p)
;> (get-field attr p)
;> (send p attack)

; ------------------ Traits ---------------------

(require racket/trait)

;; Traits look a bit simpler than Mixins.
;; However, they don't allow initialization arguments.
(define Element-Trait
  (trait (field [attr 'water]) ;; we need to use field instead of init-field
         (define/public (attack) attr)))

(define Animal-Trait
  (trait (field [gender 'male]
                [size 'small])
         (define/public (attack) size)))

;; Traits can be combined rather easily with trait-sum, trait-alias,
;; and trait-exclude
(define Pokemon-Trait
  ; combine the following Traits
  (trait-sum
   ; create a Trait with an additional method element-attack
   ; that is a duplicate of attack with trait-alias,
   ; then remove the original attack method with trait-exclude
   (trait-exclude (trait-alias Element-Trait  
                              attack         
                              element-attack)
                 attack)
   ; same for Animal
   (trait-exclude (trait-alias Animal-Trait  
                              attack         
                              animal-attack)
                 attack)
   ; combine the two new attack methods
   (trait (inherit element-attack animal-attack)
          (define/public (attack)
            (list (animal-attack) (element-attack))))))

;; We cannot add Traits directly to a class, but we can convert them to Mixins
(define Pokemon-Mixin (trait->mixin Pokemon-Trait))

;; Remember that we have no initialization arguments so far?
;; To restore them, we have to get a little creative by
;; not using the restulting Mixin directly, but a subclass of it:
(define Pokemon2 (class (Pokemon-Mixin Thing)
                   ;; add initialization arguments by hand
                   (init [this-name "a Pokemon"]
                         [this-size 'small]
                         [this-gender 'male]
                         [this-attr 'water])
                   ;; name already is an init-field, we can just
                   ;; pass it to the super call
                   (super-new [name this-name])
                   ;; to have access to the others, we first need
                   ;; to inherit them
                   (inherit-field size gender attr)
                   ;; and then we can set them to the desired value
                   (set! size this-size)
                   (set! gender this-gender)
                   (set! attr this-attr)))

;; We can make Pokemon now! \o/
(define p2 (new Pokemon2))
;> (send p2 who-are-you?)
;> (send p2 attack)

(define p3 (make-object Pokemon2 "Charmander" 'large 'female 'fire))
;> (send p3 who-are-you?)
;> (send p3 attack)

; ------------ Auxiliary Methods ---------------

;; Example for override_
(define TheNumber (class object% (super-new)
                     (define/public (number)
                       (display "The number is ")
                       23)))

(define Sub (class TheNumber (super-new)
              (define/override (number)
                (display "Actually ")
                (+ (super number) 19))))  ;; call superclass method

;> (send (new TheNumber) number)
;> (send (new Sub) number)

;; Example for augment
(define TheNumber2 (class object% (super-new)
                  (define/pubment (number)
                    (inner (void) number)  ;; call subclass method
                    (display "The number is ")
                    23)))

(define Sub2 (class TheNumber2 (super-new)
                    (define/augment (number)
                      (display "Believe it! "))))

;> (send (new TheNumber2) number)
;> (send (new Sub2) number)

;; Example for augment that uses the return value
(define TheNumber3 (class object% (super-new)
                     (define/pubment (number)
                       (display "The number is ")
                       ;; 23 is the default value
                       ;; if there is no subclass
                       (display (inner 23 number)))))

(define Sub3 (class TheNumber3 (super-new)
              (define/augment (number)
                42)))

;> (send (new TheNumber3) number)
;> (send (new Sub3) number)
