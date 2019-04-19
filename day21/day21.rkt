#lang racket

(define (exec instructions s)
  (for/fold ([s s])
            ([instr instructions])
    (define (rotate-right x)
      (let ([x (remainder x (string-length s))])
        (string-append (substring s (- (string-length s) x) (string-length s))
                       (substring s 0 (- (string-length s) x)))))
    (define (rotate-left x)
      (let ([x (remainder x (string-length s))])
        (string-append (substring s x (string-length s))
                       (substring s 0 x))))
    (define (swap-position x y)
      (define (set s i v)
        (string-append (substring s 0 i) v (substring s (add1 i))))
      (let ([X (substring s x (add1 x))]
            [Y (substring s y (add1 y))])
        (set (set s x Y) y X)))
    (define (swap-letter x y)
      (string-replace (string-replace (string-replace s x "_") y x) "_" y))
    (define (find-letter x)
      (define (find-letter i)
        (if (char=? (string-ref s i) x)
            i
            (find-letter (add1 i))))
      (find-letter 0))
    (define (rotate-based-steps idx) (add1 (if (>= idx 4) (add1 idx) idx)))
    (define (rotate-based x)
      (let ([idx (find-letter x)])
        (rotate-right (rotate-based-steps idx))))
    (define (rotate-based-reverse x)
      (define (find-old-idx new-idx)
        (define (find-old-idx idx)
          (if (= idx (string-length s))
              (error "cannot reverse rotation")
              (if (= (remainder (+ idx (rotate-based-steps idx)) (string-length s)) new-idx)
                  idx
                  (find-old-idx (add1 idx)))))
        (find-old-idx 0))
      (let* ([idx (find-letter x)]
             [old-idx (find-old-idx idx)])
        (rotate-left (rotate-based-steps old-idx))))
    (define (rotate dir x)
      (match dir
        ["right" (rotate-right x)]
        ["left" (rotate-left x)]
        [_ (error (format "unknown direction: ~a" dir))]))
    (define (reverse-positions x y)
      (string-append (substring s 0 x)
                     (list->string (reverse (string->list (substring s x (add1 y)))))
                     (if (= (add1 y) (string-length s)) "" (substring s (add1 y)))))
    (define (move x y)
      (let* ([X (substring s x (add1 x))]
             [rem (string-append (substring s 0 x) (substring s (add1 x)))])
        (string-append (substring rem 0 y) X (substring rem y))))
    (match (instruction-op instr)
      ["swap-position" (swap-position (string->number (instruction-x instr))
                                      (string->number (instruction-y instr)))]
      ["swap-letter" (swap-letter (instruction-x instr) (instruction-y instr))]
      ["rotate-based" (rotate-based (string-ref (instruction-x instr) 0))]
      ["rotate-based-reverse" (rotate-based-reverse (string-ref (instruction-x instr) 0))]
      ["rotate" (rotate (instruction-x instr)
                        (string->number (instruction-y instr)))]
      ["reverse" (reverse-positions (string->number (instruction-x instr))
                                    (string->number (instruction-y instr)))]
      ["move" (move (string->number (instruction-x instr))
                    (string->number (instruction-y instr)))]
      [_ (error (format "unknown instruction op: ~a" (instruction-op instr)))])))

(struct instruction (op x y) #:transparent)

(define (reverse-instructions instructions)
  (for/list ([instr (reverse instructions)])
    (match (instruction-op instr)
      ["rotate" (instruction "rotate" (if (string=? (instruction-x instr) "left") "right" "left") (instruction-y instr))]
      ["rotate-based" (instruction "rotate-based-reverse" (instruction-x instr) 0)]
      ["move" (instruction "move" (instruction-y instr) (instruction-x instr))]
      [_ instr])))

(define (read-instructions)
  (let ([line (read-line)])
    (if (eof-object? line)
        null
        (cons
          (match line
            [(regexp #rx"swap position") (apply instruction "swap-position" (regexp-match* #rx"[0-9]+" line))]
            [(regexp #rx"^swap letter") (apply instruction "swap-letter" (car (regexp-match* #rx"swap letter (.) with letter (.)" line #:match-select cdr)))]
            [(regexp #rx"^rotate based") (instruction "rotate-based" (car (regexp-match* #rx"rotate based on position of letter (.)" line #:match-select cadr)) 0)]
            [(regexp #rx"^rotate")
             (let ([match (regexp-match* #rx"rotate ([a-z]+) ([0-9]) step" line #:match-select cdr)])
               (instruction "rotate" (caar match) (cadar match)))]
            [(regexp #rx"^reverse") (apply instruction "reverse" (regexp-match* #rx"[0-9]+" line))]
            [(regexp #rx"^move") (apply instruction "move" (regexp-match* #rx"[0-9]+" line))]
            [_ (error (format "unknown command: ~a" line))])
          (read-instructions)))))

(let ([instructions (read-instructions)])
  (printf "part 1: ~a\n" (exec instructions "abcdefgh"))
  (printf "part 2: ~a\n" (exec (reverse-instructions instructions) "fbgdceah")))
