#lang racket

(provide part1
         part2)

(require racket/set)
(require glpk)

(struct machine (diagram schematics joltages) #:transparent)

(define convert-diagram-to-list
  (lambda (diagram)
    (reverse (for/fold ([diagram-list '()])
                       ([diagram-char (string->list diagram)]
                        [i (in-naturals)])
               (cond
                 [(char=? diagram-char #\#) (cons i diagram-list)]
                 [else diagram-list])))))

(define convert-schematics-to-list
  (lambda (schematics-string)
    (map (lambda (schematic-string)
           (let ([schematic-contents (second (regexp-match #rx"\\((.*)\\)" schematic-string))])
             (map string->number (string-split schematic-contents ","))))
         (string-split schematics-string " "))))

(define convert-joltage-requirement-to-list
  (lambda (joltage-requirement-string)
    (let ([joltage-contents (second (regexp-match #rx"\\{(.*)\\}" joltage-requirement-string))])
      (map string->number (string-split joltage-contents ",")))))

(define combine-two-schematics
  (lambda (s1 s2) (set->list (set-symmetric-difference (list->set s1) (list->set s2)))))

(define combine-schematics-list
  (lambda (slist)
    (sort (for/fold ([combined-schematics '()]) ([s slist])
            (combine-two-schematics combined-schematics s))
          <)))

(define combine-two-joltages
  (lambda (j1 j2) (set->list (set-symmetric-difference (list->set j1) (list->set j2)))))

(define combine-joltages-list
  (lambda (jlist)
    (let* ([max-counter (apply max (flatten jlist))]
           [combined-joltages (make-vector (+ max-counter 1) 0)])
      (for ([joltages jlist])
        (for ([joltage joltages])
          (vector-set! combined-joltages joltage (+ (vector-ref combined-joltages joltage) 1))))
      (vector->list combined-joltages))))

(define construct-machine-from-string
  (lambda (str)
    (let* ([regoutput (regexp-match #rx"\\[([.#]+)\\] (.*) (\\{.*\\})" str)]
           [diagram (second regoutput)]
           [schematics (third regoutput)]
           [joltage-requirement (fourth regoutput)])
      (machine (convert-diagram-to-list diagram)
               (convert-schematics-to-list schematics)
               (convert-joltage-requirement-to-list joltage-requirement)))))

(define combinations-with-replacement
  (lambda (elements k)
    (cond
      [(= k 0) '(())]
      [(empty? elements) '()]
      [else
       (append (combinations-with-replacement (cdr elements) k)
               (map (lambda (element) (cons (car elements) element))
                    (combinations-with-replacement elements (- k 1))))])))

(define find-minimum-presses-for-diagram
  (lambda (m)
    (let ([diagram (machine-diagram m)]
          [schematics (machine-schematics m)])
      (for*/or ([k (in-naturals)]
                [press-combo (combinations schematics k)])
        (cond
          [(equal? diagram (combine-schematics-list press-combo)) k]
          [else #f])))))

; This is way too slow! Use integer solver instead (see below)
(define find-minimum-presses-for-joltage
  (lambda (m)
    (let ([joltage (machine-joltages m)]
          [schematics (machine-schematics m)])
      (for*/or ([k (range (apply min joltage) 1000)]
                [press-combo (combinations-with-replacement schematics k)])
        (cond
          [(equal? joltage (combine-joltages-list press-combo)) k]
          [else #f])))))

(define assemble-objective-function-from-schematics-list
  (lambda (schematics-list)
    (cons 0
          (for/list ([i (range 0 (length schematics-list))])
            (list 1 (string->symbol (format "j~a" i)))))))

(define assemble-equations-from-schematics-and-joltage-list
  (lambda (schematics-list joltage-list)
    (for/list ([j (range 0 (length joltage-list))])
      (cons (string->symbol (format "r~a" j))
            (filter (lambda (x) (not (empty? x)))
                    (for/list ([schematic schematics-list]
                               [r (range 0 (length schematics-list))])
                      (cond
                        [(member j schematic) (list 1 (string->symbol (format "j~a" r)))]
                        [else '()])))))))

(define assemble-constraints-from-schematics-and-joltage-list
  (lambda (schematics-list joltage-list)
    (append (for/list ([joltage joltage-list]
                       [j (range 0 (length joltage-list))])
              (list (string->symbol (format "r~a" j)) joltage joltage))
            (for/list ([s (range 0 (length schematics-list))])
              (list (string->symbol (format "j~a" s)) 0 'posinf)))))

(define assemble-integer-list-from-schematics-list
  (lambda (schematics-list)
    (for/list ([s (range 0 (length schematics-list))])
      (string->symbol (format "j~a" s)))))

(define find-minimum-presses-for-joltage-using-glpk
  (lambda (m)
    (let ([jlist (machine-joltages m)]
          [slist (machine-schematics m)])
      (inexact->exact
       (caaddr (mip-solve (assemble-objective-function-from-schematics-list slist)
                          'min
                          (assemble-equations-from-schematics-and-joltage-list slist jlist)
                          (assemble-constraints-from-schematics-and-joltage-list slist jlist)
                          (assemble-integer-list-from-schematics-list slist)))))))

(define process-file
  (lambda (filename)
    (let ([lines (file->lines filename)]) (map construct-machine-from-string lines))))

(define part1
  (lambda (filename) (apply + (map find-minimum-presses-for-diagram (process-file filename)))))

(define part2
  (lambda (filename)
    (apply + (map find-minimum-presses-for-joltage-using-glpk (process-file filename)))))
