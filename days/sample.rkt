#lang racket

(provide part1 part2)

(define process-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
        (let loop ((rec (read-line port)))
          (cond 
            ((eof-object? rec) eof)
            (else
              (displayln rec)
              (loop (read-line port)))))))))


(define part1
  (lambda (filename)
    (displayln (string-append "PART ONE"))
    (process-file filename)))

(define part2
  (lambda (filename)
    (displayln (string-append "PART TWO"))
    (process-file filename)))
