#lang racket

(define (ABBA? s)
  (let ([a (string-ref s 0)]
        [b (string-ref s 1)]
        [B (string-ref s 2)]
        [A (string-ref s 3)])
    (and (char=? a A) (char=? b B) (not (char=? a b)))))

(define (TLS-support? ip)
  (define (TLS-support? i in-brackets abba-ouside-brackets)
    (if (= i (- (string-length ip) 3))
        abba-ouside-brackets
        (cond [(char=? (string-ref ip i) #\[) (TLS-support? (+ i 1) #t abba-ouside-brackets)]
              [(char=? (string-ref ip i) #\]) (TLS-support? (+ i 1) #f abba-ouside-brackets)]
              [(ABBA? (substring ip i (+ i 4))) (if in-brackets #f (TLS-support? (+ i 1) #f #t))]
              [else (TLS-support? (+ i 1) in-brackets abba-ouside-brackets)])))
  (TLS-support? 0 #f #f))

(define (ABA? s)
  (let ([a (string-ref s 0)]
        [b (string-ref s 1)]
        [A (string-ref s 2)])
    (and (char=? a A) (not (char=? a b)))))

(define (ABA->BAB aba)
  (let ([b (substring aba 1 2)])
    (string-append b (substring aba 0 1) b)))

(define (corresponding-BAB? ABAs BABs)
  (define (corresponding-BAB? aba)
    (cond [(null? aba) #f]
          [(set-member? BABs (ABA->BAB (car aba))) #t]
          [else (corresponding-BAB? (cdr aba))]))
  (corresponding-BAB? (set->list ABAs)))

(define (SSL-support? ip)
  (define (SSL-support? i in-brackets ABAs BABs)
    (if (= i (- (string-length ip) 2))
        (and (corresponding-BAB? ABAs BABs))
        (let ([c (string-ref ip i)]
              [ss (substring ip i (+ i 3))])
          (cond [(char=? c #\[) (SSL-support? (+ i 1) #t ABAs BABs)]
                [(char=? c #\]) (SSL-support? (+ i 1) #f ABAs BABs)]
                [(ABA? ss) (if in-brackets
                               (SSL-support? (+ i 1) #t ABAs (set-add BABs ss))
                               (SSL-support? (+ i 1) #f (set-add ABAs ss) BABs))]
                [else (SSL-support? (+ i 1) in-brackets ABAs BABs)]))))
  (SSL-support? 0 #f (set) (set)))

(define (read-ips)
  (let ([ip (read-line)])
    (if (eof-object? ip) null (cons ip (read-ips)))))

(let ([ips (read-ips)])
  (printf "part 1: ~a\n" (count TLS-support? ips))
  (printf "part 2: ~a\n" (count SSL-support? ips)))
