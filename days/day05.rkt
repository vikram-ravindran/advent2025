#lang racket

(provide part1
         part2)

(define convert-filelines-to-intervals-and-ids
  (lambda (filelines)
    (for/fold ([intervals '()]
               [ids '()])
              ([line filelines])
      (cond
        [(string-contains? line "-")
         (values (cons (map string->number (string-split line "-")) intervals) ids)]
        [(not (equal? line "")) (values intervals (cons (string->number line) ids))]
        [else (values intervals ids)]))))

(define check-if-ingredient-is-fresh
  (lambda (intervals id)
    (for/or ([interval intervals])
      (<= (first interval) id (second interval)))))

(define process-file
  (lambda (filename) (convert-filelines-to-intervals-and-ids (file->lines filename))))

(define combine-intervals-into-smallest-set
  (lambda (intervals)
    (let ([sorted-intervals (sort intervals (lambda (x y) (< (car x) (car y))))])
      (for/fold ([combined-intervals '()]) ([current-interval sorted-intervals])
        (let ([previous-interval (if (empty? combined-intervals)
                                     '()
                                     (first combined-intervals))])
          (cond
            [(empty? previous-interval) (list current-interval)]
            [(<= (first current-interval) (second previous-interval))
             (cons (list (first previous-interval)
                         (max (second current-interval) (second previous-interval)))
                   (rest combined-intervals))]
            [else (cons current-interval combined-intervals)]))))))

(define count-ids-spanned-by-intervals
  (lambda (intervals)
    (apply + (map (lambda (interval) (+ (- (second interval) (first interval)) 1)) intervals))))

(define part1
  (lambda (filename)
    (let*-values ([(intervals ids) (process-file filename)]
                  [(check) (curry check-if-ingredient-is-fresh intervals)])
      (length (filter check ids)))))

(define part2
  (lambda (filename)
    (let-values ([(intervals ids) (process-file filename)])
      (count-ids-spanned-by-intervals (combine-intervals-into-smallest-set intervals)))))
