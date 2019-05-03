#lang racket

(struct instruction (opcode op1 op2) #:transparent)

(define (exec-instr cpu instr)
  (define toggled? (set-member? (hash-ref cpu "tgls") (hash-ref cpu "pc")))
  (define (reg-value-or-imm x cpu)
    (let ([val (string->number x)])
      (if val val (hash-ref cpu x))))
  (define (cpy cpu)
    (if (string? (instruction-op2 instr))
        (hash-set cpu
                  (instruction-op2 instr)
                  (reg-value-or-imm (instruction-op1 instr) cpu))
        cpu))
  (define (jnz cpu)
    (let ([x (reg-value-or-imm (instruction-op1 instr) cpu)])
      (if (= x 0)
          cpu
          (hash-update cpu "pc" (lambda pc (+ (sub1 (car pc))
                                              (reg-value-or-imm (instruction-op2 instr) cpu)))))))
  (define (inc cpu)
    (hash-update cpu (instruction-op1 instr) add1))
  (define (dec cpu)
    (hash-update cpu (instruction-op1 instr) sub1))
  (define (tgl cpu)
    (hash-set cpu
              "tgls"
              (let ([toggles (hash-ref cpu "tgls")]
                    [i (+ (hash-ref cpu "pc")
                          (reg-value-or-imm (instruction-op1 instr) cpu))])
                (if (set-member? toggles i)
                    (set-remove toggles i)
                    (set-add toggles i)))))
  (define (out cpu)
    (hash-set cpu "transmited" (reg-value-or-imm (instruction-op1 instr) cpu)))
  (match (instruction-opcode instr)
    ["cpy" (if toggled? (jnz cpu) (cpy cpu))]
    ["inc" (if toggled? (dec cpu) (inc cpu))]
    ["dec" (if toggled? (inc cpu) (dec cpu))]
    ["jnz" (if toggled? (cpy cpu) (jnz cpu))]
    ["tgl" (if toggled? (inc cpu) (tgl cpu))]
    ["out" (out cpu)]
    [_     (error "invalid instruction")]))

(define (exec cpu prog)
  (define (exec cpu n)
    (define (cpu-inc-pc cpu) (hash-update cpu "pc" add1))
    (define pc (hash-ref cpu "pc"))
    (if (and (>= pc 0) (< pc (length prog)))
        (let* ([instr (list-ref prog pc)]
               [cpu (cpu-inc-pc (exec-instr cpu instr))])
          (if (string=? (instruction-opcode instr) "out")
              (if (= (hash-ref cpu "transmited") (hash-ref cpu "expected"))
                  (if (= n 20)
                      #t
                      (exec (hash-set cpu "expected" (remainder (add1 (hash-ref cpu "expected")) 2))
                            (add1 n)))
                  #f)
              (exec cpu n)))
        cpu))
  (exec cpu 0))

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
                  "pc" 0
                  "tgls" (set)
                  "transmited" 0
                  "expected" 0))

(define (min-value prog)
  (define (min-value v)
    (if (exec (hash-set cpu "a" v) prog)
        v
        (min-value (add1 v))))
  (min-value 0))

(let ([instructions (read-instructions)])
  (printf "part 1: ~a\n" (min-value instructions)))
