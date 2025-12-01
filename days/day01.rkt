#lang racket

(provide part1
         part2)

(define process-file (lambda (filename) (file->lines filename)))

(define get-direction-and-mag
  (lambda (rotation)
    (let* ([regoutput (regexp-match #rx"([LR])([0-9]+)" rotation)]
           [direction (second regoutput)]
           [mag (string->number (third regoutput))])
      (values direction mag))))

(define turn-dial
  (lambda (current-state direction mag)
    (cond
      [(equal? direction "L") (modulo (- current-state mag) 100)]
      [(equal? direction "R") (modulo (+ current-state mag) 100)]
      [else (error "Incorrect specification!")])))

(define wraparound-zero-check
  (lambda (direction current-state new-state)
    (if (or (= current-state 0)
            (= new-state 0)
            (and (equal? direction "L") (< new-state current-state))
            (and (equal? direction "R") (> new-state current-state)))
        0
        1)))

(define part1
  (lambda (filename)
    (let loop ([lines (process-file filename)]
               [current-state 50]
               [password 0])
      (cond
        [(empty? lines) password]
        [else
         (let*-values ([(direction mag) (get-direction-and-mag (car lines))]
                       [(new-state) (turn-dial current-state direction mag)])
           (loop (cdr lines)
                 new-state
                 (if (= new-state 0)
                     (+ password 1)
                     password)))]))))

(define part2
  (lambda (filename)
    (let loop ([lines (process-file filename)]
               [current-state 50]
               [password 0])
      (cond
        [(empty? lines) password]
        [else
         (let*-values ([(direction raw-mag) (get-direction-and-mag (car lines))]
                       [(extra-zeroes mag) (quotient/remainder raw-mag 100)]
                       [(new-state) (turn-dial current-state direction mag)]
                       [(wraparound-zero) (wraparound-zero-check direction
                                                                 current-state
                                                                 new-state)])
           (loop (cdr lines)
                 new-state
                 (+ password
                    wraparound-zero
                    extra-zeroes
                    (if (= new-state 0) 1 0))))]))))
