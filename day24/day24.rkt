#lang racket

(define (read-grid)
  (define (read-grid)
    (let ([line (read-line)])
      (if (eof-object? line)
          null
          (cons (list->vector (string->list line)) (read-grid)))))
  (list->vector (read-grid)))

(define (grid-at grid pos)
  (vector-ref (vector-ref grid (cdr pos)) (car pos)))

(define (char-isdigit? c) (char<=? #\0 c #\9))

(define (grid-digits grid)
  (for*/fold ([digits (hash)])
             ([y (in-range (vector-length grid))]
              [x (in-range (vector-length (vector-ref grid y)))]
              #:when (char-isdigit? (grid-at grid (cons x y))))
    (let ([pos (cons x y)])
      (hash-set digits (- (char->integer (grid-at grid pos)) (char->integer #\0)) pos))))

(define (valid? pos grid)
  (define (valid? v limit) (and (>= 0) (< v limit)))
  (and (valid? (cdr pos) (vector-length grid))
       (valid? (car pos) (vector-length (vector-ref grid 0)))))

(define (wall? grid pos) (char=? (grid-at grid pos) #\#))

(define (shortest-path-len grid start goal)
  (define (shortest-path-len queue visited)
    (define pos (caar queue))
    (define x (car pos))
    (define y (cdr pos))
    (define n (cdar queue))
    (if (equal? pos goal)
        n
        (let ([adjs (for/list ([adj (list (cons x (add1 y))
                                          (cons x (sub1 y))
                                          (cons (add1 x) y)
                                          (cons (sub1 x) y))]
                               #:when (valid? adj grid)
                               #:unless (or (wall? grid adj)
                                            (set-member? visited adj)))
                      adj)])
          (shortest-path-len (append (cdr queue) (map (lambda (adj) (cons adj (add1 n))) adjs))
                             (set-union visited (list->set adjs))))))
  (shortest-path-len (list (cons start 0)) (set start)))

(define (distances grid digits)
  (for*/fold ([distances (hash)])
             ([a (hash-keys digits)]
              [b (hash-keys digits)])
    (let ([dist (shortest-path-len grid (hash-ref digits a) (hash-ref digits b))])
      (hash-set (hash-set distances (cons a b) dist)
                (cons b a)
                dist))))

(define (min-steps digits distances suffix)
  (apply min
         (for/list ([p (permutations (range 1 (hash-count digits)))])
           (for/sum ([a (append '(0) p)]
                     [b (append p suffix)])
             (hash-ref distances (cons a b))))))

(let* ([grid (read-grid)]
       [digits (grid-digits grid)]
       [dist (distances grid digits)])
  (printf "part 1: ~a\n" (min-steps digits dist '()))
  (printf "part 2: ~a\n" (min-steps digits dist '(0))))
