#lang racket

(define cclarg (current-command-line-arguments))

(define datafile
    (if (= (vector-length cclarg) 2)
        (vector-ref cclarg 1) 
        ;else
        (string-append "data/" (vector-ref cclarg 0) ".txt")))

(define codefile (string-append "days/" (vector-ref cclarg 0) ".rkt"))

((dynamic-require codefile 'part1) datafile)
((dynamic-require codefile 'part2) datafile)


