#lang racket

(struct factory (comparison-log bots outputs))

(define (make-factory) (factory (hash) (hash) (hash)))

(define (factory-set-comparison-log f comparison-log)
  (factory comparison-log (factory-bots f) (factory-outputs f)))

(define (factory-set-bots f bots)
  (factory (factory-comparison-log f) bots (factory-outputs f)))

(define (factory-set-outputs f outputs)
  (factory (factory-comparison-log f) (factory-bots f) outputs))

(struct value-instruction (value bot))

(define (make-value-instruction instruction)
  (let* ([vals  (regexp-match* #rx"[0-9]+" instruction)]
         [value (string->number (first vals))]
         [bot   (string->number (second vals))])
    (value-instruction value bot)))

(define (add-value-to-bot bots bot value)
  (hash-update bots bot (lambda l (cons value (car l))) null))

(define (apply-value-instruction instr factory)
  (factory-set-bots factory
                    (add-value-to-bot (factory-bots factory)
                                      (value-instruction-bot instr)
                                      (value-instruction-value instr))))

(struct bot-instruction (bot low-dst low-dst-id high-dst high-dst-id))

(define (make-bot-instruction instruction)
  (let ([vals (car
                (regexp-match* #rx"bot ([0-9]+) gives low to (.+) ([0-9]+) and high to (.+) ([0-9]+)"
                               instruction
                               #:match-select cdr))])
    (bot-instruction (string->number (first vals))
                     (second vals)
                     (string->number (third vals))
                     (fourth vals)
                     (string->number (fifth vals)))))

(define (apply-bot-instruction instr factory)
  (define (update-comparison-log factory instr)
    (let ([bot (bot-instruction-bot instr)])
      (hash-set (factory-comparison-log factory)
                (hash-ref (factory-bots factory) bot)
                bot)))
  (define (add-value factory dst dst-id value)
    (if (string=? dst "bot")
        (factory-set-bots factory (add-value-to-bot (factory-bots factory) dst-id value))
        (factory-set-outputs factory (hash-set (factory-outputs factory) dst-id value))))
  (define (set-bot-values factory bot vals)
    (factory-set-bots factory (hash-set (factory-bots factory) bot vals)))
  (define (update-bots factory instr)
    (let ([bots (factory-bots factory)]
          [bot (bot-instruction-bot instr)])
      (match-let ([(list low high) (sort (hash-ref bots bot) <)])
        (set-bot-values (add-value (add-value factory
                                              (bot-instruction-low-dst instr)
                                              (bot-instruction-low-dst-id instr)
                                              low)
                                   (bot-instruction-high-dst instr)
                                   (bot-instruction-high-dst-id instr)
                                   high)
                        bot
                        '()))))
  (factory-set-comparison-log (update-bots factory instr)
                              (update-comparison-log factory instr)))

(define (read-instructions)
  (define (read-instruction instruction)
    (cond [(string-prefix? instruction "value") (make-value-instruction instruction)]
          [(string-prefix? instruction "bot") (make-bot-instruction instruction)]
          [else (error "malformed instruction: " instruction)]))
  (define (instruction-dep instr)
    (if (value-instruction? instr) -1 (bot-instruction-bot instr)))
  (define (add-instruction h instr)
    (hash-update h (instruction-dep instr) (lambda l (cons instr (car l))) null))
  (define (read-instructions h)
    (let ([line (read-line)])
      (if (eof-object? line)
          h
          (read-instructions (add-instruction h (read-instruction line))))))
  (read-instructions (hash)))

(define (apply-instructions instructions)
  (define (apply-instructions instructions id factory)
    (define (apply-instruction-list instr-list factory)
      (define (find-next instructions bots)
        (define (find-next keys bots)
          (if (null? keys)
              #f
              (if (= (length (hash-ref bots (car keys) '())) 2)
                  (car keys)
                  (find-next (cdr keys) bots))))
        (find-next (hash-keys instructions) bots))
      (define (apply-instruction instr factory)
        (if (value-instruction? instr)
            (apply-value-instruction instr factory)
            (apply-bot-instruction instr factory)))
      (if (null? instr-list)
          (let* ([instructions (hash-remove instructions id)]
                 [next-id (find-next instructions (factory-bots factory))])
            (if next-id
                (apply-instructions instructions next-id factory)
                factory))
          (apply-instruction-list (cdr instr-list)
                                  (apply-instruction (car instr-list) factory))))
    (apply-instruction-list (hash-ref instructions id) factory))
  (apply-instructions instructions -1 (make-factory)))

(let* ([factory (apply-instructions (read-instructions))]
       [comparison-log (factory-comparison-log factory)]
       [outputs (factory-outputs factory)]
       [chip1 61]
       [chip2 17])
  (printf "part 1: ~a\n" (hash-ref comparison-log
                                   (list chip1 chip2)
                                   (hash-ref comparison-log (list chip2 chip1) #f)))
  (printf "part 2: ~a\n" (* (hash-ref outputs 0)
                            (hash-ref outputs 1)
                            (hash-ref outputs 2))))
