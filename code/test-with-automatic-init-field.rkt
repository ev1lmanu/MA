#lang racket
(require "classmacro.rkt")

(define Thing (class () (super-new)
                (init-field [name "a Thing"])
                (define/public (who-are-you?)
                  (string-append "I am " name "!"))
                ; Definition einer generische Funktion
                (define/generic (attack)
                  ; Es kann eine beliebige Funktion angegeben werden,
                  ; die sich auf eine Liste apply-en lässt
                  (compose reverse list))
                (define/generic (x) list)))

(define Element (class Thing (super-new)
                  (when (equal? name "a Thing")
                        (set! name "an Element"))
                  (init-field [attr 'water])
                  (define/public (hot?) (equal? attr 'fire))
                  (define/public (attack) attr)))

(define Animal (class Thing (super-new)
                 (when (equal? name "a Thing")
                   (set! name "an Animal"))
                 (init-field [size 'small])
                 (define/public (attack) size)))

; Mehrfachvererbung durch Angabe einer (ungequoteten) Liste von Superklassen
(define Pokemon (class (Element Animal) (super-new)
                  (when (equal? name "a Thing")
                    (set! name "a Pokemon"))
                  (define/public (attack) 'ball)))

; Beispiel für die Methodenkombination
(send (new Pokemon) who-are-you?)
(send (new Pokemon) hot?)
(send (new Pokemon [attr 'fire] [size 'big]) hot?)
(send (new Pokemon) attack)
(send (new Pokemon [attr 'fire] [size 'big]) attack)