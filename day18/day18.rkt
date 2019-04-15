#lang racket

(define (safe? c) (char=? c #\.))

(define (trap? left center right)
  (define (trap? c) (not (safe? c)))
  (or (and (trap? left) (trap? center) (safe? right))
      (and (safe? left) (trap? center) (trap? right))
      (and (trap? left) (safe? center) (safe? right))
      (and (safe? left) (safe? center) (trap? right))))

(define (safe-tiles first-row n)
  (define (safe-cnt row) (count safe? row))
  (define (safe-tiles prev-row cur-row)
    (if (> cur-row n)
        0
        (let ([row (for/list ([left (append (list #\.) prev-row)]
                              [center prev-row]
                              [right (append (drop prev-row 1) '(#\.))])
                     (if (trap? left center right) #\^ #\.))])
          (+ (safe-cnt prev-row) (safe-tiles row (add1 cur-row))))))
  (safe-tiles first-row 1))

(let ([first-row (string->list (read-line))])
  (printf "part 1: ~a\n" (safe-tiles first-row 40))
  (printf "part 1: ~a\n" (safe-tiles first-row 400000)))
