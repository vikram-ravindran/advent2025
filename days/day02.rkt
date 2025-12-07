#lang racket

(provide part1
         part2)

(define process-file
  (lambda (filename)
    (map (lambda (x) (map string->number (string-split x "-")))
         (string-split (car (file->lines filename)) ","))))

(define invalid-doubled-id?
  (lambda (id)
    (let* ([string-id (number->string id)]
           [string-id-length (string-length string-id)])
      (cond
        [(odd? string-id-length) #f]
        [(equal? (substring string-id 0 (/ string-id-length 2))
                 (substring string-id (/ string-id-length 2) string-id-length))
         #t]
        [else #f]))))

(define find-invalid-doubled-ids-in-rangepair
  (lambda (rangepair)
    (for/list ([i (range (first rangepair) (+ 1 (second rangepair)))])
      (if (invalid-doubled-id? i)
          i
          '()))))

(define invalid-ned-id?
  (lambda (id n)
    (let* ([string-id (number->string id)]
           [string-id-length (string-length string-id)])
      (and (= (modulo string-id-length n) 0)
           (for*/and ([i (range 0 n)]
                      [j (range i string-id-length n)])
             (equal? (string-ref string-id i) (string-ref string-id j)))))))

(define invalid-id?
  (lambda (id)
    (let* ([string-id (number->string id)]
           [string-id-length (string-length string-id)])
      (for/or ([i (range 1 (+ (floor (/ string-id-length 2)) 1))])
        (invalid-ned-id? id i)))))

(define find-invalid-ids-in-rangepair
  (lambda (rangepair)
    (for/list ([i (range (first rangepair) (+ 1 (second rangepair)))])
      (if (invalid-id? i)
          i
          '()))))

(define part1
  (lambda (filename)
    (let ([rangepairs (process-file filename)])
      (apply + (flatten (map find-invalid-doubled-ids-in-rangepair rangepairs))))))

(define part2
  (lambda (filename)
    (let ([rangepairs (process-file filename)])
      (apply + (flatten (map find-invalid-ids-in-rangepair rangepairs))))))
