#lang racket

(define (button-next keypad button instr)
  (define directions (hash
                       #\L (cons -1 0)
                       #\U (cons 0 -1)
                       #\R (cons 1 0)
                       #\D (cons 0 1)))
  (let* ([dir (hash-ref directions instr)]
         [next (cons (+ (car button) (car dir))
                     (+ (cdr button) (cdr dir)))])
    (if (hash-has-key? keypad next) next button)))

(define (process-instr-line line keypad button)
  (if (null? line)
      button
      (process-instr-line (cdr line) keypad (button-next keypad button (car line)))))

(define (find-combination instructions keypad button)
  (if (null? instructions)
      ""
      (let ([final-button (process-instr-line (car instructions) keypad button)])
        (string-append (hash-ref keypad final-button)
                       (find-combination (cdr instructions) keypad final-button)))))

(define (read-instructions)
  (let ([line (read-line)])
    (if (eof-object? line) null (cons (string->list line) (read-instructions)))))

(let ([instructions (read-instructions)]
      [keypad1 (hash (cons 0 0) "1" (cons 1 0) "2" (cons 2 0) "3"
                     (cons 0 1) "4" (cons 1 1) "5" (cons 2 1) "6"
                     (cons 0 2) "7" (cons 1 2) "8" (cons 2 2) "9")]

      [keypad2 (hash                               (cons 2 0) "1"
                                    (cons 1 1) "2" (cons 2 1) "3" (cons 3 1) "4"
                     (cons 0 2) "5" (cons 1 2) "6" (cons 2 2) "7" (cons 3 2) "8" (cons 4 2) "9"
                                    (cons 1 3) "A" (cons 2 3) "B" (cons 3 3) "C"
                                                   (cons 2 4) "D")])
  (printf "part 1: ~a\n" (find-combination instructions keypad1 (cons 1 1)))
  (printf "part 2: ~a\n" (find-combination instructions keypad2 (cons 0 2))))
