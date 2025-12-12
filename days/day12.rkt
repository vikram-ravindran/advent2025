#lang racket

(provide part1
         part2)

(struct tree (height width num-packages-vector) #:transparent)

(struct package ([vec #:mutable]) #:transparent)

(define convert-package-line-to-list
  (lambda (package-line)
    (list->vector (map (lambda (c)
                         (cond
                           [(char=? c #\#) 1]
                           [else 0]))
                       (string->list package-line)))))

(define make-package-from-char-grid
  (lambda (chargrid) (package (apply vector-append (map convert-package-line-to-list chargrid)))))

(define make-tree-from-line
  (lambda (line)
    (let ([treematch (regexp-match #rx"^([0-9]+)x([0-9]+): (.*)$" line)])
      (tree (string->number (second treematch))
            (string->number (third treematch))
            (list->vector (map string->number (string-split (fourth treematch))))))))

(define number-of-package-squares (lambda (package) (apply + (vector->list (package-vec package)))))

(define packages-fit-under-tree
  (lambda (package-vector tree)
    (for/sum ((num-packages (tree-num-packages-vector tree)) (package package-vector))
             (* num-packages (number-of-package-squares package)))))

(define packages-fit-under-trees
  (lambda (filename)
    (apply +
           (let-values ([(package-vector tree-list) (process-file filename)])
             (map (lambda (tree)
                    (cond
                      [(< (packages-fit-under-tree package-vector tree)
                          (* (tree-height tree) (tree-width tree)))
                       1]
                      [else 0]))
                  tree-list)))))

(define process-file
  (lambda (filename)
    (let ([package-vector (make-vector 6)]
          [tree-list '()])
      (call-with-input-file filename
                            (lambda (in)
                              (let loop ([line (read-line in)])
                                (cond
                                  [(eof-object? line) (values package-vector tree-list)]
                                  [(equal? line "") (loop (read-line in))]
                                  [(regexp-match #rx"^([0-9]):$" line)
                                   (vector-set! package-vector
                                                (string->number (second (regexp-match #rx"^([0-9]):$"
                                                                                      line)))
                                                (make-package-from-char-grid
                                                 (list (read-line in) (read-line in) (read-line in))))
                                   (loop (read-line in))]
                                  [else
                                   (set! tree-list (cons (make-tree-from-line line) tree-list))
                                   (loop (read-line in))])))))))

(define part1 (lambda (filename) (packages-fit-under-trees filename)))
(define part2 (lambda (filename) (packages-fit-under-trees filename)))
