#lang racket

(define (rotate-row screen y n)
  (let ([row (list-ref screen y)])
    (list-set screen
              y
              (append (take-right row n) (take row (- (length row) n))))))

(define (rotate-column screen x n)
  (let ([column (map (lambda row (list-ref (car row) x)) screen)])
    (map (lambda (row v) (list-set row x v))
         screen
         (car (rotate-row (list column) 0 n)))))

(define (rect screen A B)
  (append (map (lambda row (append (make-list A #t) (drop (car row) A)))
               (take screen B))
          (drop screen B)))

(define (print-screen screen)
  (for-each (lambda row
              (for-each (lambda v (display (if (car v) #\# #\space))) (car row))(newline))
            screen))

(define (read-operations)
  (let ([op (read-line)])
    (if (eof-object? op) null (cons op (read-operations)))))

(define (apply-operations screen operations)
  (define (apply-op screen op)
    (let* ([ab (regexp-match* #rx"[0-9]+" op)]
           [a (string->number (first ab))]
           [b (string->number (second ab))])
      (cond [(string-prefix? op "rect") (rect screen a b)]
            [(string-prefix? op "rotate row") (rotate-row screen a b)]
            [(string-prefix? op "rotate column") (rotate-column screen a b)])))
  (if (null? operations)
      screen
      (apply-operations (apply-op screen (car operations)) (cdr operations))))

(define (new-screen rows columns) (make-list rows (make-list columns #f)))

(let ([screen (apply-operations (new-screen 6 50) (read-operations))])
  (printf "part 1: ~a\n" (count (lambda v (car v)) (flatten screen)))
  (displayln "part 2:")
  (print-screen screen))
