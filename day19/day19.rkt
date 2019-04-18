#lang racket

(define (build-circle n)
  (define (build-list)
    (define (build-list i)
      (if (> i n)
          null
          (mcons i (build-list (add1 i)))))
    (build-list 1))
  (define (make-circle l)
    (define (make-circle it)
      (if (null? (mcdr it))
          (set-mcdr! it l)
          (make-circle (mcdr it))))
    (make-circle l))
  (let ([l (build-list)])
    (make-circle l)
    l))

(define (find-mid l)
  (define (find-mid a b)
    (if (or (= (mcar (mcdr b)) 1) (= (mcar (mcdr (mcdr b))) 1))
        a
        (find-mid (mcdr a) (mcdr (mcdr b)))))
  (find-mid l l))

(define (solve cur target len advance-target)
  (define (solve cur target len)
    (if (= len 1)
        (mcar cur)
        (begin
          (set-mcar! target (mcar (mcdr target)))
          (set-mcdr! target (mcdr (mcdr target)))
          (solve (mcdr cur) (advance-target target len) (sub1 len)))))
  (solve cur target len))

(define input (string->number (command-line #:args (input) input)))

(let ([circle (build-circle input)])
  (printf "part 1: ~a\n" (solve circle (mcdr circle) input (lambda (target len) (mcdr target)))))
(let ([circle (build-circle input)])
  (printf "part 2: ~a\n" (solve circle
                                (find-mid circle)
                                input
                                (lambda (target len) (if (odd? len) (mcdr target) target)))))
