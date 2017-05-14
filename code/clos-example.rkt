#lang swindle

;; A basic class Thing with a slot
(defclass Thing ()
  (name :accessor name        ; how to access this slot for reading/writing
        :initvalue "a Thing"  ; default value
        :initarg :name)       ; how to access this slot for intialization
  :printer #t                 ; pretty printing of objects of this class
  :automaker #t)                 ; enable position-based slot initialization with make-

;; Create an instance with make (label-based slot intialization)
;> (make Thing)
;> (make Thing :name "Bob")
;> (make-Thing "Bob")
(define thing (make Thing))
(set! (name thing) "Bob")
;> (name thing)

;; A method defined on instances of Thing
(defmethod (who-are-you? (t Thing))
  (string-append "I am " (name t) "!"))

;; Let's call the method
;> (who-are-you? (make Thing))

;; An Element class that inherits from Thing
;; If we only use by-position initialization,
;; with automaker, we can omit the :initarg option
(defclass Element (Thing)
  (name :initvalue "an Element")  ; we can overwrite the inherited slot
  (attr :initvalue 'water)
  :autoaccessors :slot            ; automatically create accessors
  :printer #t          
  :automaker #t)

;; method specialized on Element
(defmethod (hot? (e Element))
  (equal? (attr e) 'fire))

(define elem (make Element))
;> (who-are-you? elem)

;; An Animal class that also inherits from Thing
(defclass Animal (Thing)
  (name :initvalue "an Animal")      
  (size  :initvalue 'small)
  :autoaccessors :slot
  :automaker #t
  :printer #t)

;> (make-Animal)
;> (make-Animal 'female)
;> (make-Animal "Kittycat" 'female 'small)

;; A pokemon class that inherits from both Element and Animal
(defclass Pokemon (Animal Element)
  ; (name :initvalue "a Pokemon") ; first without
  :automaker #t
  :printer #t)

;; Pokemon inherits everything!
(define p1 (make-Pokemon))
;> p1
;> (hot? p1)
;> (who-are-you? p1)

;; Curious: Less inherited slots from less specific classes 
;; come first as by-position argument
(define p2 (make-Pokemon "Charmander" 'fire 'large 4))
;> p2
;> (hot? p2)
;> (who-are-you? p2)

;; A generic function for objects of type Thing and its method combination
(defgeneric attack ((t Thing))
  :combination generic-list-combination)

;; Implementations of the generic function for Element and Animal
(defmethod attack ((e Element))
  (attr e))

(defmethod attack ((a Animal))
  (size a))

;; See how the combination works
;> (attack p1)
;> (attack p2)

;; Before, After and Around methods
(defclass Trainer ())

;; For each primary method, we can also define a Before, After and
;; Around method (and either can be present or not)
;; Before and After methods have a set evaluation order:
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

;; See the order of evaluation: 
;> (daily-routine (make Earlybird))

;; Around methods are like an override with a (optional) super call:
;; It is the only method that can prevent the execution of Before and
;; After methods of its superclass:
(defclass Lazybum (Trainer))

(defmethod daily-routine :around ((l Lazybum))
  (println "He slept through the whole day."))                                      

;> (daily-routine (make Lazybum))

;; Or we can invoke superclass methods with call-next-method. While
;; Before and After methods can only be used for side effects, an
;; Around method can access the primary method's result, use it and
;; return something which may or may not be based on the primary
;; method's value:
(defclass Nightowl (Trainer))

(defmethod daily-routine :around ((n Nightowl))
  (println "He slept through the whole day.")
  (list (call-next-method) 42))              ; let's return something

;> (daily-routine (make Nightowl))