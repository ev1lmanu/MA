#lang swindle

(defclass Thing ())

(defclass Element (Thing)
  (attr
    :accessor attr
    :initvalue 'water
    :initarg :attr)
  (color
    :accessor color
    :initvalue 'blue)
  :printer #t
  :automaker #t)

; (make Element)
; (make Element :attr 'fire)
; (make-Element 'fire)
(define elem (make Element))
(set! (attr elem) 'wind)

(defclass Animal (Thing)
  (gender :initvalue 'male)
  (size  :initvalue 'small)
  (color :initvalue 'brown)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

; (make-Animal)
; (make-Animal 'female)
; (make-Animal 'female 'normal 'foo)

(defclass Pokemon (Animal Element)
  (index :initvalue 0
         :type <number>)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

(define p1 (make-Pokemon))
(define p2 (make-Pokemon 'fire 'red 'female 'large 42))

(defgeneric attack ((t Thing))
  :combination generic-list-combination)

(defmethod attack ((e Element))
  (attr e))

(defmethod attack ((a Animal))
  (size a))

; (attack p1)
; (attack p2)

(defclass Trainer ())

(defmethod daily-routine ((t Trainer))
  (println "He caught some Pokemon."))
(defmethod daily-routine :before ((t Trainer))
  (println "He walked out."))
(defmethod daily-routine :after ((t Trainer))
  (println "He walked back home."))

(defclass Earlybird (Trainer))

(defmethod daily-routine ((e Earlybird))
  (println "He found two bird Pokemon in the morning."))
(defmethod daily-routine :before ((e Earlybird))
  (println "The sun just started rising."))
(defmethod daily-routine :after ((e Earlybird))
  (println "There was still time before dinner."))

; (daily-routine (make Earlybird))