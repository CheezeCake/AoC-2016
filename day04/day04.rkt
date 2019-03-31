#lang racket

(struct room (name id checksum))

(define (name-checksum name)
  (define (count-chars chars h)
    (if (null? chars)
        h
        (count-chars (cdr chars)
                     (let ([c (car chars)])
                       (if (hash-has-key? h c)
                           (hash-update h c (lambda v (+ (car v) 1)))
                           (hash-set h c 1))))))
  (define (name->list name) (filter char-alphabetic? (string->list name)))
  (list->string (take (map (lambda kv (caar kv))
                           (sort (hash->list (count-chars (name->list name) (hash)))
                                 (lambda (kv1 kv2) (if (= (cdr kv1) (cdr kv2))
                                                       (char<? (car kv1) (car kv2))
                                                       (> (cdr kv1) (cdr kv2))))))
                      5)))

(define (room-real? room)
  (equal? (name-checksum (room-name room)) (room-checksum room)))

(define (read-rooms)
  (let ([line (read-line)])
    (if (eof-object? line)
        null
        (let ([name (car (regexp-match #rx"^[^0-9]+" line))]
               [id  (car (regexp-match* #rx"[0-9]+" line))]
               [checksum  (string-trim (car (regexp-match* #rx"[a-z]+\\]" line)) "]")])
          (cons (room (string-trim name "-") (string->number id) checksum) (read-rooms))))))

(define (real-room-id-sum rooms)
  (if (null? rooms)
      0
      (+ (if (room-real? (car rooms)) (room-id (car rooms)) 0)
         (real-room-id-sum (cdr rooms)))))

(define (caesar-cipher s n)
  (define a-int (char->integer #\a))
  (define (alpha->int c) (- (char->integer c) a-int))
  (define (int->alpha c) (integer->char (+ c a-int)))
  (define (shift-char c)
    (if (char-alphabetic? c)
        (int->alpha (remainder (+ (alpha->int c) n) 26))
        #\space))
  (define (caesar-cipher l)
    (if (null? l)
        null
        (cons (shift-char (car l)) (caesar-cipher (cdr l)))))
  (list->string (caesar-cipher (string->list s))))

(define (get-room-id rooms name)
  (if (null? rooms)
      null
      (let ([room (car rooms)])
        (if (equal? (caesar-cipher (room-name room) (room-id room)) name)
            (room-id room)
            (get-room-id (cdr rooms) name)))))

(let ([rooms (read-rooms)])
  (printf "part 1: ~a\n" (real-room-id-sum rooms))
  (printf "part 2: ~a\n" (get-room-id rooms "northpole object storage")))
