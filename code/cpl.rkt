#lang racket
; A copy of the topological sort implemented in CLOS

(require swindle/misc)

(provide compute-std-cpl)

(define (compute-std-cpl c get-direct-supers)
  (top-sort (build-transitive-closure get-direct-supers c)
            (build-constraints get-direct-supers c)
            (std-tie-breaker get-direct-supers)))

(define (top-sort elements constraints tie-breaker)
  (let loop ([elements elements] [constraints constraints] [result '()])
    (if (null? elements)
      result
      (let ([can-go-in-now
             (filter (lambda (x)
                       (every (lambda (constraint)
                                (or (not (eq? (cadr constraint) x))
                                    (memq (car constraint) result)))
                              constraints))
                     elements)])
        (if (null? can-go-in-now)
          (error 'top-sort "invalid constraints")
          (let ([choice (if (null? (cdr can-go-in-now))
                          (car can-go-in-now)
                          (tie-breaker result can-go-in-now))])
            (loop (filter (lambda (x) (not (eq? x choice))) elements)
                  constraints (append result (list choice)))))))))

(define (std-tie-breaker get-supers)
  (lambda (partial-cpl min-elts)
    (let loop ([pcpl (reverse partial-cpl)])
      (let* ([current-elt (car pcpl)]
             [ds-of-ce (get-supers current-elt)]
             [common (filter (lambda (x) (memq x ds-of-ce)) min-elts)])
        (if (null? common)
          (if (null? (cdr pcpl))
            (error 'std-tie-breaker "nothing valid") (loop (cdr pcpl)))
          (car common))))))

(define (build-transitive-closure get-follow-ons x)
  (let track ([result '()] [pending (list x)])
    (if (null? pending)
      result
      (let ([next (car pending)])
        (if (memq next result)
          (track result (cdr pending))
          (track (cons next result)
                 (append (get-follow-ons next) (cdr pending))))))))

(define (build-constraints get-follow-ons x)
  (let loop ([elements (build-transitive-closure get-follow-ons x)]
             [this-one '()]
             [result '()])
    (if (or (null? this-one) (null? (cdr this-one)))
      (if (null? elements)
        result
        (loop (cdr elements)
              (cons (car elements) (get-follow-ons (car elements)))
              result))
      (loop elements
            (cdr this-one)
            (cons (list (car this-one) (cadr this-one)) result)))))