#lang racket

(require openssl/md5)

(define directions (list (cons "U" (cons 0 -1))
                         (cons "D" (cons 0 1))
                         (cons "L" (cons -1 0))
                         (cons "R" (cons 1 0))))

(define (done? pos) (and (= (car pos) 3) (= (cdr pos) 3)))

(define (within-bounds? pos)
  (define (valid? x) (and (>= x 0) (< x 4)))
  (and (valid? (car pos)) (valid? (cdr pos))))

(define (next-pos pos d) (cons (+ (car pos) (car d)) (+ (cdr pos) (cdr d))))

(define (open? c) (>= (char->integer c) (char->integer #\b)))

(define (find-path input cmp init-length)
  (define (find-path pos path)
    (if (done? pos)
        (cons (string-length path) path)
        (let ([md5-hash (md5 (open-input-string (string-append input path)))])
          (for/fold ([best-path (cons init-length "")])
                    ([p (for/list ([d directions]
                                   [c md5-hash]
                                   #:when (and (open? c) (within-bounds? (next-pos pos (cdr d)))))
                          (find-path (next-pos pos (cdr d)) (string-append path (car d))))])
                    (if (cmp (car p) (car best-path)) p best-path)))))
  (find-path (cons 0 0) ""))

(let ([input (command-line #:args (input) input)])
  (printf "part 1: ~a\n" (cdr (find-path input < +inf.0)))
  (printf "part 2: ~a\n" (car (find-path input > 0))))
