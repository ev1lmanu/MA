#lang racket
(require "classmacro.rkt")


(define thing (class () (super-new)
                        (field [x 42])
                        (define/generic (attack) list)))

(define element (class thing (super-new)
                  (inherit-field x)
                  (init-field [attr 'water])
                  (define/public (attack) attr)))

(define animal (class thing (super-new)
                 (init-field [size 'small])
                 (define/public (attack) size)))

(define pokemon (class (element animal) (super-new)
                          (field [form 'ball])
                          (define/public (attack) form)
                          (inherit-field attr size)))