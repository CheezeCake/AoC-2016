#lang racket

(define (apply-marker marker in)
  (let* ([vals (regexp-match* #rx"[0-9]+" marker)]
         [n-chars (string->number (first vals))]
         [n (string->number (second vals))]
         [chars (map (lambda v (read-char in)) (range n-chars))])
    (append-map (lambda l chars) (range n))))

(define (handle-marker in)
  (define (handle-marker marker)
    (let ([c (read-char in)])
      (if (char=? c #\))
          (apply-marker (list->string marker) in)
          (handle-marker (append marker (list c))))))
  (handle-marker '()))

(define (decompressed-length in v2)
  (define (decompressed-length in)
    (let* ([c (read-char in)])
      (cond [(eof-object? c) 0]
            [(char-whitespace? c) (decompressed-length)]
            [(char=? c #\() (+ (if v2
                                   (decompressed-length (open-input-string
                                                          (list->string (handle-marker in))))
                                   (length (handle-marker in)))
                               (decompressed-length in))]
            [else (+ 1 (decompressed-length in))])))
  (decompressed-length in))

(define (read-data)
  (let ([line (read-line)])
    (if (eof-object? line) "" (string-append line (read-data)))))

(let ([data (read-data)])
  (printf "part 1: ~a\n" (decompressed-length (open-input-string data) #f))
  (printf "part 2: ~a\n" (decompressed-length (open-input-string data) #t)))
