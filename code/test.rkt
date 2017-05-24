#lang racket
(require "multiple-inheritance.rkt")

(define Thing (class object% (super-new)
                (init-field [name "a Thing"])
                (define/public (who-are-you?)
                  (string-append "I am " name "!"))
                ; generic function
                (define/generic (attack)
                  (compose reverse list))
                ; implementing method
                (define/public (attack) 'a)))

;> (send (new Thing) who-are-you?)
;> (send (new Thing [name "Bob"]) who-are-you?)

(define Element (class Thing 
                  (init [name "an Element"])
                  (super-new [name name])
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))
                  (define/override (who-are-you?)
                    (string-append (super who-are-you?)
                                   (if (hot?) " And I am hot!" "")))
                  ; implementing method
                  (define/public (attack) attr)))

;> (send (new Element) who-are-you?)
;> (send (new Element [attr 'fire]) who-are-you?)
;> (send (new Element) attack)

(define Animal (class Thing
                 (init [name "an Animal"])
                 (super-new [name name])
                 (init-field [size 'small])
                 ; implementing method
                 (define/public (attack) size)))

;> (send (new Animal) who-are-you?)
;> (send (new Animal [name "Bob"]) who-are-you?)
;> (send (new Animal) attack)

; multiple inheritance by supplying an (unquoted) super class list
(define Pokemon (class (Element Animal)
                  (init [name "a Pokemon"])
                  (super-new [name name])
                  ; implementing method
                  (define/public (attack) 'ball)
                  ; inherit fields that are needed for combination
                  (inherit-field attr size)))

; Beispiel für die Methodenkombination
(send (new Pokemon) who-are-you?)
(send (new Pokemon) hot?)
(get-field size (new Pokemon [name "Bob"] [attr 'fire] [size 'big]))
(send (new Pokemon [attr 'fire] [size 'big]) who-are-you?)
(send (new Pokemon) attack)
(send (new Pokemon [attr 'fire] [size 'big]) attack)