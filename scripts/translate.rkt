#lang racket
(require csv-reading)

(define tidsplan (csv->list (open-input-file "tidsplan.csv")))

(define-values (lokaler plan) (match tidsplan
                  [(list lokaler plan ...) (values lokaler plan)]))

(displayln lokaler)
(foldl (lambda (tidspunkt acc)
            (match tidspunkt
              [(list dag "" ...) (displayln dag)]
              [(list tid program ...) (displayln tid)
                                      (println program)]))
       '()
       plan)

;; (display tidsplan)
