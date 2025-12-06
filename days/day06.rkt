#lang racket

(provide part1
         part2)

(define ns (make-base-namespace))

(define process-file-part1
  (lambda (filename)
    (let ([stringlines (map string-split (reverse (file->lines filename)))])
      (let ([operators (map (lambda (opstr) (string->symbol opstr)) (car stringlines))]
            [operands (map (lambda (operandline)
                             (map (lambda (operand) (string->number operand)) operandline))
                           (cdr stringlines))])
        (cons operators operands)))))

(define parallel-car (lambda (lsts) (map car lsts)))

(define parallel-cdr (lambda (lsts) (map cdr lsts)))

(define parallel-empty?
  (lambda (lsts)
    (for/and ([lst lsts])
      (empty? lst))))

(define parallel-spaces?
  (lambda (lsts)
    (for/and ([lst lsts])
      (char=? (car lst) #\space))))

(define pivot-string-numbers
  (lambda (string-numbers)
    (cond
      [(parallel-empty? string-numbers) string-numbers]
      [(parallel-spaces? string-numbers)
       (cons -1 (pivot-string-numbers (parallel-cdr string-numbers)))]
      [else
       (cons (string->number (string-trim (list->string (parallel-car string-numbers))))
             (pivot-string-numbers (parallel-cdr string-numbers)))])))

(define convert-pivoted-numbers-to-lists
  (lambda (pivoted-numbers)
    (for/fold ([numbers '()]
               [numberslist '()])
              ([pivoted-number pivoted-numbers])
      (cond
        [(or (empty? pivoted-number) (= pivoted-number -1)) (values '() (cons numbers numberslist))]
        [else (values (cons pivoted-number numbers) numberslist)]))))

(define process-file-part2
  (lambda (filename)
    (let* ([reverse-stringlines (reverse (file->lines filename))])
      (let-values ([(operators) (map (lambda (opstr) (string->symbol opstr))
                                     (string-split (car reverse-stringlines)))]
                   [(_ raw-operands) (convert-pivoted-numbers-to-lists
                                      (pivot-string-numbers
                                       (map string->list (reverse (cdr reverse-stringlines)))))])
        (cons (reverse operators) (filter (lambda (x) (not (empty? x))) raw-operands))))))

(define compute-column-totals-part1
  (lambda (lines)
    (let ([operators (first lines)]
          [initial-totals (second lines)]
          [remaining-number-lines (cddr lines)])
      (for/fold ([totals initial-totals]) ([number-line remaining-number-lines])
        (values (map (lambda (exp) (eval exp ns)) (map list operators totals number-line)))))))

(define compute-column-totals-part2
  (lambda (columns)
    (cond
      [(empty? (car columns)) '()]
      [else
       (cons (apply (eval (caar columns) ns) (cadr columns))
             (compute-column-totals-part2 (cons (cdar columns) (cddr columns))))])))

(define part1
  (lambda (filename) (apply + (compute-column-totals-part1 (process-file-part1 filename)))))

(define part2
  (lambda (filename) (apply + (compute-column-totals-part2 (process-file-part2 filename)))))
