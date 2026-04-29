#lang racket
(require csv-reading)

(define tidsplan (csv->list (open-input-file "tidsplan.csv")))

(define-values (lokaler plan) (match tidsplan
                  [(list lokaler plan ...) (values lokaler plan)]))

(print lokaler)
(for-each (lambda (tidspunkt)
            (match tidspunkt
              [(list dag "" ...) (print dag)]
              [(list tid program ...) (display tid)]))
          plan)

;; (display tidsplan)
