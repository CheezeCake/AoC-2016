#lang racket

(struct range (low high) #:transparent)

(define (add-range l r)
  (cond [(null? l) (cons r null)]
        [(< (range-high r) (sub1 (range-low (car l)))) (cons r l)]
        [(> (range-low r) (add1 (range-high (car l)))) (cons (car l) (add-range (cdr l) r))]
        [else (add-range (cdr l)
                         (range (min (range-low (car l)) (range-low r))
                                (max (range-high (car l)) (range-high r))))]))

(define (read-ranges)
  (define (read-ranges l)
    (let ([line (read-line)])
      (if (eof-object? line)
          l
          (let* ([r (string-split line "-")]
                 [low (string->number (first r))]
                 [high (string->number (second r))])
            (read-ranges (add-range l (range low high)))))))
  (read-ranges '()))

(let ([ranges (read-ranges)])
  (printf "part 1: ~a\n" (if (zero? (range-low (first ranges)))
                             (add1 (range-high (first ranges)))
                             0))
  (printf "part 2: ~a\n" (- 4294967296
                            (for/sum ([r ranges])
                              (add1 (- (range-high r) (range-low r)))))))
