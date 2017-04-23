#lang racket

; ------------ Klassen und Objekte ---------------
(define thing (class object% (super-new)))

; (new thing)

(define element (class thing (super-new)
                  (init-field [attr 'water])
                  (define/public (get-attr) attr)
                  (define/public (set-attr value) (set! attr value))))

(define elem (new element))
; (send elem get-attr)
; (send elem set-attr 'wind)

(define elem2 (new element [attr 'fire]))
; (send elem2 get-attr)

(define animal (class thing (super-new)
                 (init-field [gender 'male]
                             [size 'small])
                 (define/public (get-gender) gender)
                 (define/public (get-size) size)))
; (new animal)
; (send (new animal) get-gender)
; (send (new animal) get-size)
; (send (new animal [gender 'female] [size 'huge]) get-size)
; (send (new animal [gender 'female] [size 'huge]) get-gender)

(define pet (class animal (super-new)
              (init-field [name 'unknown])
              (define/public (get-name) name)))

(define harry (new pet [name 'harry] [size 'normal]))
; (send harry get-name)
; (send harry get-gender)
; (send harry get-size)


; --------------- Mixins ------------------

(define (generate-subclass superclass)
  (class superclass (super-new)))

(define (element-mixin %)
  (class % (super-new)
    (init-field [attr 'water])
    (define/public (get-attr) attr)
;    (define/public (attack) attr) ; !
    (define/override (attack) (list (super attack) attr))
    ))

(define (animal-mixin %)
  (class % (super-new)
    (init-field [gender 'male]
                [size 'small])
    (define/public (get-gender) gender)
    (define/public (get-size) size)
    (define/public (attack) size)))

(define element2 (element-mixin
                  (class object% (super-new)
                    (define/public (attack) null))))
; (send (new element2) get-attr)

(define animal2 (animal-mixin thing))
; (send (new animal2 [size 'huge] [gender 'female]) get-size)

(define pokemon (element-mixin (animal-mixin thing)))
; (define pokemon (animal-mixin (element-mixin thing))) ; !

; (send (new pokemon) get-size)
; (send (new pokemon) get-attr)
; (send (new pokemon) attack)
(define p (new pokemon [size 'large]
                       [gender 'female]
                       [attr 'fire]))
; (send p get-size)
; (send p get-gender)
; (send p get-attr)
; (send p attack)

; --------------- Traits ------------------

(require racket/trait)

(define element-trait
  (trait (field [attr 'water])
         (define/public (get-attr) attr)
         (define/public (attack) attr)))

(define animal-trait
  (trait (field [gender 'male]
                [size 'small])
         (define/public (get-gender) gender)
         (define/public (get-size) size)
         (define/public (attack) size)
         ))

(define pokemon-trait
  ; combine the following traits
  (trait-sum
   ; create a trait with an additional method element-attack
   ; that is a duplicate of attack with trait-alias
   ; and remove the original attack method with trait-exclude
   (trait-exclude (trait-alias element-trait  
                              attack         
                              element-attack)
                 attack)
   ; same for animal
   (trait-exclude (trait-alias animal-trait  
                              attack         
                              animal-attack)
                 attack)
   ; combine the two new attack methods
   (trait (inherit element-attack animal-attack)
          (define/public (attack)
            (list (animal-attack) (element-attack))))))

(define pokemon-mixin (trait->mixin pokemon-trait))

(define pokemon2 (pokemon-mixin
                  (class object% (super-new)
                    (init-rest args)
                    (when (not (empty? args))
                      (display args))
                    )))

(define p2 (new pokemon2))
; (send p2 attack)
; (define p3 (new pokemon2 [size 'large] [gender 'female] [attr 'fire]))
(define p4 (make-object pokemon2 'large 'female 'fire))
               