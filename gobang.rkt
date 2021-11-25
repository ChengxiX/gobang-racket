#lang racket
;(define node (lambda (depth aim situ alphabeta) (
;                                                 if (< depth aim) (
;                                                                   if (= (remainder depth 2) 0)
;                                                                      (maxnode depth aim situ 0 alphabeta -1e20)
;                                                                      (minnode depth aim situ 0 alphabeta 1e20)
;                                                                      )
;                                                    (value situ)
;                                                    )))
;(define maxnode (lambda (depth aim situ sequence alpha beta) (
;                                                              let ((x (node (+ depth 1) aim situ beta)) (newsitu (placenext sequence situ))) (if (and (< x alpha) (not (= newsitu stopcom)))
;                                                                                                                                                 (let (y (maxnode depth aim newsitu sequence alpha (max x beta))) (if (< (car y) (car x)) x y)) x
;                                                                                                                                                 ))))
;(define minnode (lambda (depth aim situ sequence alpha beta) (
;                                                              let ((x (node (+ depth 1) aim situ beta)) (newsitu (placenext sequence situ))) (if (and (> x alpha) (not (= newsitu stopcom)))
;                                                                                                                                                 (let (y (maxnode depth aim newsitu sequence alpha (min x beta))) (if (> (car y) (car x)) x y)) x
;                                                                                                                                                 ))))
(define stopcom 4.2)
;(define value (lambda (situ) (
;                              
;                              )))

(define area (lambda (situ)
               (cons (
                      cons (min 14 (+ (apply max (map (lambda (x) (car x)) situ)) 2)) (min 14 (+ (apply max (map (lambda (x) (cdr x)) situ))))
                           )
                     (
                      cons (max 0 (- (apply min (map (lambda (x) (car x)) situ)) 2)) (max (- (apply min (map (lambda (x) (cdr x)) situ)) 2))
                           ))
               ))
(define next (lambda (diagonol x y situ) (if (<= x (car (car (cdr diagonol)))) (let ([a (if (= y (car (cdr (car (cdr diagonol))))) (next diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (next diagonol x (+ 1 y) situ))]) (if (eq? a stopcom) (cons x y) (if (member (cons x y) situ) a (cons (cons x y) a)))) stopcom)))
(next '((1 3) (5 4)) 1 3 '())