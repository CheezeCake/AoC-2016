#lang racket

; (define (generate-data a len)
;   (define (generate-data a)
;     (if (>= (string-length a) len)
;         a
;         (generate-data (string-append a
;                                       "0"
;                                       (list->string (map (lambda (c) (if (char=? c #\0) #\1 #\0))
;                                                          (reverse (string->list a))))))))
;   (generate-data a))

(define (generate-data s len)
  (define a (make-string (* 2 len)))
  (define (generate-data cur-len)
    (if (>= cur-len len)
        (substring a 0 cur-len)
        (begin
          (string-set! a cur-len #\0)
          (for ([c (in-string a)]
                [i (in-range (* 2 cur-len) cur-len -1)])
            (string-set! a i (if (char=? c #\0) #\1 #\0)))
          (generate-data (add1 (* 2 cur-len))))))
  (string-copy! a 0 s)
  (generate-data (string-length s)))

(define (checksum state len)
  (define data (substring (generate-data state len) 0 len))
  (define (checksum cur-len)
    (define next-len (/ cur-len 2))
    (for/list ([i (in-range 0 cur-len 2)]
               [j (in-range next-len)])
      (let ([x (string-ref data i)]
            [y (string-ref data (add1 i))])
        (string-set! data j (if (char=? x y) #\1 #\0))))
    (if (odd? next-len)
        (substring data 0 next-len)
        (checksum next-len)))
  (checksum len))

(let ([input (command-line #:args (input) input)])
  (printf "part 1: ~a\n" (checksum input 272))
  (printf "part 2: ~a\n" (checksum input 35651584)))
