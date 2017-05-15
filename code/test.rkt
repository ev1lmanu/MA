#lang racket
(require "classmacro.rkt")

(define thing (class () (super-new)
                        ; Definition einer generische Funktion
                        (define/generic (attack)
                          ; Es kann eine beliebige Funktion angegeben werden,
                          ; die sich auf eine Liste apply-en lässt
                          (compose reverse list))))

(define element (class thing (super-new)
                  (init-field [attr 'water])
                  (define/public (attack) attr)))

(define animal (class thing (super-new)
                 (init-field [size 'small])
                 (define/public (attack) size)))

; Mehrfachvererbung durch Angabe einer (ungequoteten) Liste von Superklassen
(define pokemon (class (element animal) (super-new)
                          (define/public (attack) 'ball)
                          ; Das inherit-field ist für die Methodenkombination
                          ; notwendig; ich will noch dafür sorgen, dass es
                          ; automagisch geerbt wird.
                          (inherit-field attr size)))

; Beispiel für die Methodenkombination
(send (new pokemon) attack)
(send (new pokemon [attr 'fire] [size 'big]) attack)