#lang racket

(define (read-messages)
  (let ([message (read-line)])
    (if (eof-object? message) null (cons message (read-messages)))))

(define (count-at messages pos)
  (define (count messages cnt)
    (if (null? messages)
        cnt
        (count (cdr messages)
               (let ([c (string-ref (car messages) pos)])
                 (if (hash-has-key? cnt c)
                     (hash-update cnt c (lambda v (+ (car v) 1)))
                     (hash-set cnt c 1))))))
  (count messages (hash)))

(define (get-key cnt cmp init)
  (define (get-key kv)
    (if (null? kv)
        (cons #\nul init)
        (let ([x (get-key (cdr kv))])
          (if (cmp (cdar kv) (cdr x)) (car kv) x))))
  (get-key (hash->list cnt)))

(let* ([messages (read-messages)]
       [distributions (map (lambda pos (count-at messages (car pos)))
                           (range (string-length (car messages))))])
  (define (max-key cnt) (get-key cnt > 0))
  (define (min-key cnt) (get-key cnt < +inf.0))
  (define (keys->string keys) (list->string (map car keys)))
  (printf "part 1: ~a\n" (keys->string (map max-key distributions)))
  (printf "part 2: ~a\n" (keys->string (map min-key distributions))))
