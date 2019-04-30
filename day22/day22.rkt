#lang racket

(struct node (size used) #:transparent)

(define (node-avail node) (- (node-size node) (node-used node)))

(define (read-nodes)
  (define (read-nodes grid)
    (define line (read-line))
    (if (eof-object? line)
        grid
        (let ([vals (map string->number (regexp-match* #rx"([0-9]+)" line #:match-select cadr))])
          (read-nodes (hash-set grid (cons (first vals) (second vals)) (node (third vals)
                                                                             (fourth vals)))))))
  (read-line)(read-line) ; ignore the first two lines
  (read-nodes (hash)))

(define (grid-dimensions grid)
  (define (dimension-size grid f) (apply max (map f (hash-keys grid))))
  (cons (dimension-size grid car) (dimension-size grid cdr)))

(define (viable-pairs nodes)
  (for/sum ([(p node) nodes]
            #:when (positive? (node-used node)))
    (let ([n (count (lambda (kv) (>= (node-avail (cdr kv)) (node-used node))) (hash->list nodes))])
      (if (>= (node-avail node) (node-used node)) (sub1 n) n))))

(define (closest-node start grid grid-dim target?)
  (define (closest-node queue visited)
    (define (valid? p)
      (define (valid? p f) (let ([v (f p)]) (and (>= v 0) (<= v (f grid-dim)))))
      (and (valid? p car) (valid? p cdr)))
    (define (viable? p1 p2)
      (>= (node-size (hash-ref grid p1))
          (node-used (hash-ref grid p2))))
    (define cur (car queue))
    (define cur-x (car cur))
    (define cur-y (cdr cur))
    (if (target? cur)
        cur
        (let-values ([(queue visited) (for/fold ([queue (cdr queue)]
                                                 [visited visited])
                                                ([adj (list (cons cur-x (add1 cur-y))
                                                            (cons cur-x (sub1 cur-y))
                                                            (cons (add1 cur-x) cur-y)
                                                            (cons (sub1 cur-x) cur-y))]
                                                #:when (and (valid? adj) (viable? cur adj))
                                                #:unless (set-member? visited adj))
                                        (values (append queue (list adj)) (set-add visited adj)))])
          (closest-node queue visited))))
  (closest-node (list start) (set start)))

(define (min-steps grid)
  (define (manhattan-distance a b)
    (+ (abs (- (car a) (car b)))
       (abs (- (cdr a) (cdr b)))))
  (define grid-dim (grid-dimensions grid))
  (define hole (car (first (filter (lambda (kv) (zero? (node-used (cdr kv)))) (hash->list grid)))))
  (define top (closest-node hole grid grid-dim (lambda (p) (zero? (cdr p)))))
  (+ (manhattan-distance hole top)
     (- (car grid-dim) (car top))
     (* 5 (sub1 (car grid-dim)))))

(let ([grid (read-nodes)])
  (printf "part 1: ~a\n" (viable-pairs grid))
  (printf "part 2: ~a\n" (min-steps grid)))
