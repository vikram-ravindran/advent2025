#lang racket

(define cclarg (current-command-line-arguments))

(define datafile
  (if (= (vector-length cclarg) 2)
      (vector-ref cclarg 1)
      ;else
      (string-append "data/" (vector-ref cclarg 0) ".txt")))

(define codefile (string-append "days/" (vector-ref cclarg 0) ".rkt"))

(displayln (format "Result of part 1: ~a"
                   ((dynamic-require codefile 'part1) datafile)))
(displayln (format "Result of part 2: ~a"
                   ((dynamic-require codefile 'part2) datafile)))
