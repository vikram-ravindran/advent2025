#lang racket

(provide part1
         part2)

(define convert-bankstring-to-banklist
  (lambda (str) (map (compose (lambda (x) (- x 48)) char->integer) (string->list str))))

(define process-file (lambda (filename) (map convert-bankstring-to-banklist (file->lines filename))))

(define remove-first-n-values
  (lambda (lst n)
    (cond
      [(= n 0) lst]
      [else (remove-first-n-values (cdr lst) (- n 1))])))

(define max-joltage-and-index-of-nth-digit
  (lambda (lst n)
    (for/fold ([maxjolt 0]
               [joltidx 0])
              ([currentjolt lst]
               [i (range 0 (- (length lst) (- n 1)))])
      (if (> currentjolt maxjolt)
          (values currentjolt i)
          (values maxjolt joltidx)))))

(define max-joltage-from-n-batteries
  (lambda (lst n)
    (cond
      [(= n 0) 0]
      [else
       (let-values ([(maxjolt joltidx) (max-joltage-and-index-of-nth-digit lst n)])
         (+ (* maxjolt (expt 10 (- n 1)))
            (max-joltage-from-n-batteries (remove-first-n-values lst (+ joltidx 1)) (- n 1))))])))

(define part1
  (lambda (filename)
    (apply + (map (lambda (lst) (max-joltage-from-n-batteries lst 2)) (process-file filename)))))

(define part2
  (lambda (filename)
    (apply + (map (lambda (lst) (max-joltage-from-n-batteries lst 12)) (process-file filename)))))
