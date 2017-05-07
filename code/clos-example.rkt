#lang swindle

(defclass Thing ()
  (name 
    :accessor name
    :initvalue "a Thing")
  :printer #t)

; (make Thing)

(defgeneric (who-are-you? (t Thing)))
(defmethod (who-are-you? (t Thing))
  (string-append "I am " (name t) "!"))

; (who-are-you? (make Thing))

(defclass Element (Thing)
  (name :initvalue "an Element")  ; first without
  (attr
    :accessor attr
    :initvalue 'water
    :initarg :attr)
  :printer #t
  :automaker #t)

(defmethod (hot? (e Element))
  (equal? (attr e) 'fire))

; (make Element)
; (make Element :attr 'fire)
; (make-Element "Calcifer" 'fire)
(define elem (make Element))
(set! (attr elem) 'wind)
; elem
; (who-are-you? elem)

(defclass Animal (Thing)
  (name :initvalue "an Animal")
  (gender :initvalue 'male)
  (size  :initvalue 'small)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

; (make-Animal)
; (make-Animal 'female)
; (make-Animal "Kittycat" 'female 'small)

(defclass Pokemon (Animal Element)
  (name :initvalue "a Pokemon") ; first without
  (index :initvalue 0
         :type <number>)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

(define p1 (make-Pokemon))
(define p2 (make-Pokemon "Charmander" 'fire 'female 'large 4))
; p1
; p2
; (hot? p1)
; (hot? p2)
; (who-are-you? p1)

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
  (display "He caught some Pokemon."))
(defmethod daily-routine :before ((t Trainer))
  (display "He walked out."))
(defmethod daily-routine :after ((t Trainer))
  (display "He walked back home."))

(defclass Earlybird (Trainer))

(defmethod daily-routine ((e Earlybird))
  (display "He found two bird Pokemon in the morning."))
(defmethod daily-routine :before ((e Earlybird))
  (display "The sun just started rising."))
(defmethod daily-routine :after ((e Earlybird))
  (display "There was still time before dinner."))

; (daily-routine (make Earlybird))