#lang racket

(provide part1
         part2)

(define process-file (lambda (filename) (file->lines filename)))

(define find-all-instances-of-char-in-string
  (lambda (str char-to-find)
    (reverse (for/fold ([indices '()])
                       ([current-char (string->list str)]
                        [i (in-naturals)])
               (if (equal? current-char char-to-find)
                   (values (cons i indices))
                   ;else:
                   (values indices))))))

(define split-tachyon-beams
  (lambda (tachyon-columns splitter-columns)
    (for/fold ([number-of-splits 0]
               [new-tachyon-columns '()])
              ([tachyon-column tachyon-columns])
      (cond
        [(member tachyon-column splitter-columns)
         (values (+ number-of-splits 1)
                 (remove-duplicates (append new-tachyon-columns
                                            `(,(- tachyon-column 1) ,(+ tachyon-column 1)))))]
        [else
         (values number-of-splits
                 (remove-duplicates (append new-tachyon-columns `(,tachyon-column))))]))))

(define count-total-number-of-tachyon-splits
  (lambda (lines)
    (let-values
        ([(total-splits _)
          (for/fold ([total-number-of-splits 0]
                     [current-tachyon-columns (find-all-instances-of-char-in-string (car lines) #\S)])
                    ([current-line (cdr lines)])
            (let ([current-splitter-columns (find-all-instances-of-char-in-string current-line #\^)])
              (let-values ([(number-of-splits new-tachyon-columns)
                            (split-tachyon-beams current-tachyon-columns current-splitter-columns)])
                (values (+ total-number-of-splits number-of-splits) new-tachyon-columns))))])
      total-splits)))

(define count-number-of-tachyon-worlds
  (lambda (tachyon-column lines)
    (define count-number-of-tachyon-worlds-helper
      (let ([memoization-hash (make-hash)])
        (lambda (tachyon-column lines)
          (cond
            [(empty? lines) 1]
            [(hash-has-key? memoization-hash (list tachyon-column (car lines)))
             (hash-ref memoization-hash (list tachyon-column (car lines)))]
            [else
             (let* ([current-line (car lines)]
                    [splitter-columns (find-all-instances-of-char-in-string current-line #\^)])
               (cond
                 [(member tachyon-column splitter-columns)
                  (let ([number-of-worlds
                         (+ (count-number-of-tachyon-worlds-helper (- tachyon-column 1) (cdr lines))
                            (count-number-of-tachyon-worlds-helper (+ tachyon-column 1)
                                                                   (cdr lines)))])
                    (hash-set! memoization-hash (list tachyon-column (car lines)) number-of-worlds)
                    number-of-worlds)]
                 [else (count-number-of-tachyon-worlds-helper tachyon-column (cdr lines))]))]))))
    (count-number-of-tachyon-worlds-helper tachyon-column lines)))

(define count-total-number-of-tachyon-worlds
  (lambda (lines)
    (let ([tachyon-column (car (find-all-instances-of-char-in-string (car lines) #\S))])
      (count-number-of-tachyon-worlds tachyon-column (cdr lines)))))

(define part1 (lambda (filename) (count-total-number-of-tachyon-splits (process-file filename))))

(define part2 (lambda (filename) (count-total-number-of-tachyon-worlds (process-file filename))))
