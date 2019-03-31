#lang racket

(require openssl/md5)

(define input "reyedfim")

(define (password door-id)
  (define (password n curlen)
    (if (= curlen 8)
        ""
        (let ([hash (md5 (open-input-string (string-append door-id (number->string n))))])
          (if (string-prefix? hash "00000")
              (string-append (substring hash 5 6) (password (+ n 1) (+ curlen 1)))
              (password (+ n 1) curlen)))))
  (password 0 0))

(define (positional-password door-id)
  (define (password n pwd)
    (if (= (hash-count pwd) 8)
        pwd
        (let* ([hash (md5 (open-input-string (string-append door-id (number->string n))))]
               [pos (string->number (substring hash 5 6))]
               [char (string-ref hash 6)])
          (password (+ n 1) (if (and (string-prefix? hash "00000")
                                     (not (boolean? pos)) (>= pos 0) (< pos 8)
                                     (not (hash-has-key? pwd pos)))
                                (hash-set pwd pos char)
                                pwd)))))
  (let ([pwd (password 0 (hash))])
    (list->string (map (lambda k (hash-ref pwd (car k))) (range 8)))))

(printf "part 1: ~a\n" (password input))
(printf "part 2: ~a\n" (positional-password input))
