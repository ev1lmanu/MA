#lang swindle

(defclass thing ())

(defclass element (thing)
  (attr
    :accessor attr
    :initvalue 'water
    :initarg :attr)
  :printer #t)

; (print (make element))
; (print (make element :attr 'fire))
(define elem (make element))
(set! (attr elem) 'wind)
; (print elem)

(defclass animal (thing)
  (gender :initvalue 'male)
  (size  :initvalue 'small)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

(print (make-animal))
(printf "~n")
(print (make-animal 'female 'medium))
(printf "~n")

(defclass pokemon (animal element)
  (index :initvalue 0
         :type <number>)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

(define p1 (make-pokemon))
(print p1)
(printf "~n")
(define p2 (make-pokemon 'fire 'female 'large 42))
(print p2)
(printf "~n")

(defgeneric attack ((t thing))
  :combination generic-list-combination)

(defmethod attack ((e element))
  (attr e))

(defmethod attack ((a animal))
  (size a))

(print (attack p1))
(printf "~n")
(print (attack p2))
