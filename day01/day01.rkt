#lang racket

(define north (cons 0 1))
(define east  (cons 1 0))
(define south (cons 0 -1))
(define west  (cons -1 0))

(define (direction-next dir c)
  (let ([left (equal? c #\L)])
    (match dir
      [(== north) (if left west east)]
      [(== east)  (if left north south)]
      [(== south) (if left east west)]
      [(== west)  (if left south north)])))

(define (position-next pos dir)
  (cons (+ (car pos) (car dir))
        (+ (cdr pos) (cdr dir))))

(define (build-path pos cur-dir instr)
  (define (walk pos dir n)
    (if (= n 0)
        (list pos)
        (cons pos (walk (position-next pos dir) dir (- n 1)))))
  (if (null? instr)
      (list pos)
      (let* ([next-dir (direction-next cur-dir (string-ref (car instr) 0))]
             [n (string->number (substring (car instr) 1))]
             [path (walk pos next-dir n)])
        (append (drop-right path 1) (build-path (last path) next-dir (cdr instr))))))

(define (find-duplicate path)
  (define (find-duplicate path st)
    (if (null? path)
        null
        (let ([loc (car path)])
          (if (set-member? st loc)
              (car path)
              (find-duplicate (cdr path) (set-add st loc))))))
  (find-duplicate path (set)))

(define (manhattan-distance a b)
  (define (abs-diff a b) (abs (- a b)))
  (+ (abs-diff (car a) (car b))
     (abs-diff (cdr a) (cdr b))))

(let* ([start (cons 0 0)]
       [path (build-path start north (string-split (read-line) ", "))])
  (printf "part 1: ~a\n" (manhattan-distance start (last path)))
  (printf "part 2: ~a\n" (manhattan-distance start (find-duplicate path))))
