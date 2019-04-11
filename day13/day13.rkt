#lang racket

(define input 1362)

(define (open-space? pos)
  (let ([x (car pos)]
        [y (cdr pos)])
    (even? (count (lambda (c) (char=? c #\1))
                  (string->list (number->string (+ (* x x) (* 3 x) (* 2 x y) y (* y y) input) 2))))))

(define (adjacent x y)
  (define (pos-valid? x y) (and (>= x 0) (>= y 0)))
  (for/list ([dx '(-1 0 1 0)]
              [dy '(0 1 0 -1)]
              #:when (pos-valid? (+ x dx) (+ y dy)))
    (cons (+ x dx) (+ y dy))))

(struct position (x y steps))

(define (shortest-path-length x y break-cond ret)
  (define (shortest-path-length positions seen)
    (when (null? positions) (error "not found"))
    (let* ([pos   (car positions)]
           [x     (position-x pos)]
           [y     (position-y pos)]
           [steps (position-steps pos)])
      (if (break-cond pos)
          (ret pos seen)
          (let* ([adj (filter (lambda (p) (and (not (set-member? seen p)) (open-space? p))) (adjacent x y))]
                 [adj-pos-steps (map (lambda (p) (position (car p) (cdr p) (add1 steps))) adj)])
            (shortest-path-length (append (cdr positions) adj-pos-steps) (set-union seen (list->set adj)))))))
  (shortest-path-length (list (position x y 0)) (set (cons x y))))

(shortest-path-length 1 1
                      (lambda (pos) (and (= (position-x pos) 31) (= (position-y pos) 39)))
                      (lambda (pos seen) (position-steps pos)))

(shortest-path-length 1 1
                      (lambda (pos) (= (position-steps pos) 50))
                      (lambda (pos seen) (set-count seen)))
