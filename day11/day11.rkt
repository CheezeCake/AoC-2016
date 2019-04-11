#lang racket

(struct microchip (type) #:transparent)

(struct generator (type) #:transparent)

(define (get-type v)
  (if (microchip? v) (microchip-type v) (generator-type v)))

(define (cnt floors)
  (for/list ([floor floors])
    (foldl (lambda (v h) (hash-update h (if (microchip? v) "m" "g") add1 0))
          (hash)
          floor)))

(struct facility-state (floors cur-floor) ;#:transparent)
        #:methods
        gen:equal+hash
        [(define (equal-proc a b equal?-recur)
           ; compare a and b
           (equal?-recur (cons (cnt (facility-state-floors a)) (facility-state-cur-floor a))
                         (cons (cnt (facility-state-floors b)) (facility-state-cur-floor b))))
         (define (hash-proc a hash-recur)
           ; compute primary hash code of a
           (hash-recur (cons (cnt (facility-state-floors a)) (facility-state-cur-floor a))))
         (define (hash2-proc a hash2-recur)
           ; compute secondary hash code of a
           (hash2-recur (cons (cnt (facility-state-floors a)) (facility-state-cur-floor a))))])

(define (done? states)
  (define (done? floors)
    (and (empty? (list-ref floors 0))
         (empty? (list-ref floors 1))
         (empty? (list-ref floors 2))))
  (for/or ([state states])
    (done? (facility-state-floors state))))

(define (radiation? floors)
  (define (radiation? floor)
    (define (cnt floor)
      (foldl (lambda (v h) (hash-update h (get-type v) (if (microchip? v) sub1 add1) 0))
             (hash)
             floor))
    (let ([cnt (hash-values (cnt floor))])
      (and (findf positive? cnt) (findf negative? cnt))))
  (for/or ([floor floors])
    (radiation? floor)))

(define (next-floors floors current-floor lift-contents next-floor)
  (list-set (list-set floors
                      current-floor
                      (remove* lift-contents (list-ref floors current-floor)))
            next-floor
            (append (list-ref floors next-floor) lift-contents)))

(define (next-states states)
  (define (next-states state)
    (let ([floors (facility-state-floors state)]
          [cur-floor (facility-state-cur-floor state)])
        (for*/list ([lift-contents (combinations (list-ref floors cur-floor))]
                    #:when (and (not (empty? lift-contents)) (<= (length lift-contents) 2))
                    [next-floor (list (add1 cur-floor) (sub1 cur-floor))]
                    #:when (and (>= next-floor 0) (<= next-floor 3)))
          (facility-state (next-floors floors cur-floor lift-contents next-floor) next-floor))))
  (for/fold ([s (set)])
            ([state states])
    (set-union s (list->set (filter (lambda s (not (radiation? (facility-state-floors (car s)))))
                                    (next-states state))))))

(define (solve floors)
  (define (solve states states-seen)
    (if (done? states)
        0
        (let ([states-seen (set-union states-seen states)])
          (+ 1 (solve (set-subtract (next-states states) states-seen) states-seen)))))
  (solve (set (facility-state floors 0)) (set)))

(define input (list (list (generator "thulium")
                          (microchip "thulium")
                          (generator "plutonium")
                          (generator "strontium"))
                    (list (microchip "plutonium")
                          (microchip "strontium"))
                    (list (generator "promethium")
                          (microchip "promethium")
                          (generator "ruthenium")
                          (microchip "ruthenium"))
                    (list)))

(define input2 (list (list (generator "thulium")
                           (microchip "thulium")
                           (generator "plutonium")
                           (generator "strontium")
                           (generator "elerium")
                           (microchip "elerium")
                           (generator "dilithium")
                           (microchip "dilithium"))
                     (list (microchip "plutonium")
                           (microchip "strontium"))
                     (list (generator "promethium")
                           (microchip "promethium")
                           (generator "ruthenium")
                           (microchip "ruthenium"))
                     (list)))

(printf "part 1: ~a\n" (solve input))
(printf "part 2: ~a\n" (solve input2))
