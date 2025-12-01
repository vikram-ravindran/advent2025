#lang racket

(provide part1 part2)

(define process-file
  (lambda (filename)
    (file->lines filename)))


(define turn-dial
  (lambda (current-state rotation)
    (let* ((regoutput (regexp-match #rx"([LR])([0-9]+)" rotation))
           (direction (second regoutput))
           (mag (string->number (third regoutput))))
      (cond
        ((equal? direction "L") (modulo (- current-state mag) 100))
        ((equal? direction "R") (modulo (+ current-state mag) 100))
        (else (error "Incorrect specification!"))))))


(define part1
  (lambda (filename)
    (let loop 
      ((lines (process-file filename))
       (current-state 50) (password 0))
      (cond 
        ((empty? lines) password)
        (else 
            (let ((new-state (turn-dial current-state (car lines))))
              (loop (cdr lines) 
                    new-state
                    (if (= new-state 0) (+ password 1) password))))))))


(define part2
  (lambda (filename)
    (let ((lines (process-file filename)))
      (write lines))))

