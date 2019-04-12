#lang racket

(struct disc (number positions position-at-zero))

(define (solve discs)
  (define (solve time)
    (if (for/and ([disc discs])
          (= (remainder (+ (disc-position-at-zero disc) time (disc-number disc))
                        (disc-positions disc))
              0))
        time
        (solve (add1 time))))
  (solve 0))

(define (read-discs)
  (let ([line (read-line)])
    (if (eof-object? line)
        null
        (cons (apply disc
                     (map string->number
                          (car (regexp-match* #rx"Disc #([0-9]+) has ([0-9]+).*position ([0-9]+)" line #:match-select cdr))))
              (read-discs)))))

(let ([discs (read-discs)])
  (printf "part 1: ~a\n" (solve discs))
  (printf "part 2: ~a\n" (solve (append discs (list (disc 7 11 0))))))
