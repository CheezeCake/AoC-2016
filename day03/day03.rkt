#lang racket

(define (read-lengths)
  (let ([line (read-line)])
    (if (eof-object? line)
        '()
        (append (map string->number (filter non-empty-string? (string-split line " ")))
                (read-lengths)))))

(define (triangle-valid? t)
  (let ([a (first t)]
        [b (second t)]
        [c (third t)])
    (and (> (+ a b) c) (> (+ a c) b) (> (+ b c) a))))

(define (possible-triangles lengths make-3-triangles)
  (if (null? lengths)
      0
      (let ([triangles (make-3-triangles lengths)])
        (+ (if (triangle-valid? (first triangles)) 1 0)
           (if (triangle-valid? (second triangles)) 1 0)
           (if (triangle-valid? (third triangles)) 1 0)
           (possible-triangles (drop lengths 9) make-3-triangles)))))

(let ([lengths (read-lengths)])
  (define (make-3-triangles-lines lengths)
    (list (take lengths 3) (take (drop lengths 3) 3) (take (drop lengths 6) 3)))
  (define (make-3-triangles-columns lengths)
    (list (list (first lengths)   (fourth lengths)  (seventh lengths))
          (list (second lengths)  (fifth lengths)   (eighth lengths))
          (list (third lengths)   (sixth lengths)   (ninth lengths))))
  (printf "part 1: ~a\n" (possible-triangles lengths make-3-triangles-lines))
  (printf "part 2: ~a\n" (possible-triangles lengths make-3-triangles-columns)))
