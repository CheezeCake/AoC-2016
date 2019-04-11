#lang racket

(struct instruction (opcode op1 op2) #:transparent)

(define (exec-instr cpu instr)
  (define (reg-value-or-imm x cpu)
    (let ([val (string->number x)])
      (if val val (hash-ref cpu x))))
  (define (cpy cpu)
    (hash-set cpu
              (instruction-op2 instr)
              (reg-value-or-imm (instruction-op1 instr) cpu)))
  (define (jnz cpu)
    (let ([x (reg-value-or-imm (instruction-op1 instr) cpu)])
      (if (= x 0)
          cpu
          (hash-update cpu "pc" (lambda pc (+ (- (car pc) 1)
                                              (string->number (instruction-op2 instr))))))))
  (match (instruction-opcode instr)
    ["cpy" (cpy cpu)]
    ["inc" (hash-update cpu (instruction-op1 instr) add1)]
    ["dec" (hash-update cpu (instruction-op1 instr) sub1)]
    ["jnz" (jnz cpu)]
    [_     (error "invalid instruction")]))

(define (exec cpu prog)
  (define (exec cpu)
    (define (cpu-inc-pc cpu) (hash-update cpu "pc" add1))
    (let ([pc (hash-ref cpu "pc")])
      (if (and (>= pc 0) (< pc (length prog)))
          (exec (cpu-inc-pc (exec-instr cpu (list-ref prog pc))))
          cpu)))
  (exec cpu))

(define (read-instructions)
  (let ([line (read-line)])
    (if (eof-object? line)
        null
        (cons (match (string-split line)
                [(list opcode x y)  (instruction opcode x y)]
                [(list opcode x)    (instruction opcode x 0)]
                [_                  (error "malformed instruction")])
              (read-instructions)))))

(define cpu (hash "a" 0
                  "b" 0
                  "c" 0
                  "d" 0
                  "pc" 0))

(let ([instructions (read-instructions)])
  (printf "part 1: ~a\n" (hash-ref (exec cpu instructions) "a"))
  (printf "part 2: ~a\n" (hash-ref (exec (hash-set cpu "c" 1) instructions) "a")))
