#lang racket

(require openssl/md5)

(define (check-tuple s i n)
  (let ([c (string-ref s i)])
    (andmap (lambda (off) (char=? (string-ref s (- i off)) c)) (range 1 n))))

(define (tuple s n)
  (define (tuple i)
    (cond [(= i (string-length s)) #f]
          [(and (>= i (sub1 n)) (check-tuple s i n)) (string-ref s i)]
          [else (tuple (add1 i))]))
  (tuple 0))

(define (triplet s) (tuple s 3))
(define (quintuple s) (tuple s 5))

(define (generate-keys salt n hashing)
  (define (generate-keys index possible-key-indices key-indices)
    (define (add-key-index key-indices c index)
      (hash-update key-indices c (lambda (l) (append l (list index))) '()))
    (define (remove-key-indices key-indices c indices)
      (hash-update key-indices c (lambda (l) (remove* indices l)) '()))
    (if (>= (length key-indices) n)
        key-indices
        (let* ([hash (hashing (string-append salt (number->string index)))]
               [trips (triplet hash)]
               [quints (quintuple hash)]
               [new-key-indices (filter (lambda (key-index) (<= (- index key-index) 1000))
                                        (hash-ref possible-key-indices quints '()))])
          (generate-keys (add1 index)
                         (remove-key-indices (if trips
                                                 (add-key-index possible-key-indices trips index)
                                                 possible-key-indices)
                                             quints
                                             new-key-indices)
                         (append key-indices new-key-indices)))))
  (generate-keys 0 (hash) '()))

(define (hashing n)
  (define (hashing s n)
    (if (<= n 0)
        s
        (hashing (md5 (open-input-string s)) (sub1 n))))
  (lambda (s) (hashing s n)))

(define input "cuanljph")

(printf "part 1: ~a\n" (list-ref (generate-keys input 64 (hashing 1)) 63))
(printf "part 2: ~a\n" (list-ref (generate-keys input 64 (hashing 2017)) 63))
