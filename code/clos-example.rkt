#lang swindle

(defclass thing ())

(defclass element (thing)
  (attr
    :accessor attr
    :initvalue 'water
    :initarg :attr)
  (color
    :accessor color
    :initvalue 'blue)
  :printer #t
  :automaker #t)

; (make element)
; (make element :attr 'fire)
; (make-element 'fire)
(define elem (make element))
(set! (attr elem) 'wind)

(defclass animal (thing)
  (gender :initvalue 'male)
  (size  :initvalue 'small)
  (color :initvalue 'brown)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

; (make-animal)
; (make-animal 'female)
; (make-animal 'female 'normal 'foo)

(defclass pokemon (animal element)
  (index :initvalue 0
         :type <number>)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

(define p1 (make-pokemon))
(define p2 (make-pokemon 'fire 'red 'female 'large 42))

(defgeneric attack ((t thing))
  :combination generic-list-combination)

(defmethod attack ((e element))
  (attr e))

(defmethod attack ((a animal))
  (size a))

; (attack p1)
; (attack p2)

(defclass trainer ())

(defmethod daily-routine ((t trainer))
  (println "He caught some pokemon."))
(defmethod daily-routine :before ((t trainer))
  (println "He walked out."))
(defmethod daily-routine :after ((t trainer))
  (println "He walked back home."))

(defclass earlybird (trainer))

(defmethod daily-routine ((e earlybird))
  (println "He found two bird pokemon in the morning."))
(defmethod daily-routine :before ((e earlybird))
  (println "The sun just started rising."))
(defmethod daily-routine :after ((e earlybird))
  (println "There was still time before dinner."))

; (daily-routine (make earlybird))