#lang racket
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim) (
                                                                   if (= (remainder depth 2) 0)
                                                                      (maxnode depth aim situ 0 alphabeta -1e20)
                                                                      (minnode depth aim situ 0 alphabeta 1e20)
                                                                      )
                                                    (value situ)
                                                    )))
(define maxnode (lambda (depth aim situ sequence alpha beta) (
                                                              let ((x (node (+ depth 1) aim situ beta)) (newsitu (changesitu sequence situ))) (if (and (< x alpha) (not (= newsitu stopcom)))
                                                                                                                                                  (max (maxnodedepth aim newsitu squence alpha (max x beta)) x) x
                                                                                                                                                  ))))
(define minnode (lambda (depth aim situ sequence alpha beta) (
                                                              let ((x (node (+ depth 1) aim situ beta)) (newsitu (changesitu sequence situ))) (if (and (> x alpha) (not (= newsitu stopcom)))
                                                                                                                                                  (min (maxnodedepth aim newsitu squence alpha (min x beta)) x) x
                                                                                                                                                  ))))
(define stopcom 4.2)
(define value (lambda (situ) (
                              
                              )))
(define change)
(define area (lambda (situ) (cons (
                                   cons (apply max (map (lambda (x) (car x)) situ)) (apply max (map (lambda (x) (cdr x)) situ))
                                        )
                                  (
                                   cons (apply min (map (lambda (x) (car x)) situ)) (apply min (map (lambda (x) (cdr x)) situ))
                                        ))
               ))