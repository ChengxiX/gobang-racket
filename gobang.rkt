#lang racket
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim) ;(
                                                                   ;if (= (remainder depth 2) 0)
                                                                      ;(maxnode depth aim situ 0 alphabeta -1e20 (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                                      ;(minnode depth aim situ 0 alphabeta 1e20 (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                                      ;)
                                                    (branch (if (= (remainder depth 2) 0) < >) depth aim situ 0 alphabeta -1e20 (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                    (list (value situ) situ)
                                                    )))

(define branch (lambda (opposite depth aim situ alpha beta able) (
                                                          let ((x (node (+ depth 1) aim (cons situ (car able)) beta))) (if (and (opposite (car x) alpha) (not (eq? able null)))
                                                                                                                           (let ((y (branch opposite depth aim situ alpha (min x beta) (cdr able)))) (if (opposite (car y) (car x)) x y)) x
                                                                                                                           ))))
(define AI-first 0) ;如果是0则是AI先手，如果是1则是AI后手（人先手）

;value函数返回值结构：数字
(define value (lambda (situ) (
                              1
                              )))

(define generate-diagonol (lambda (situ)
               (cons (
                      cons (min 14 (+ (apply max (map (lambda (x) (car x)) situ)) 2)) (min 14 (+ (apply max (map (lambda (x) (car (cdr x))) situ))))
                           )
                     (
                      cons (max 0 (- (apply min (map (lambda (x) (car x)) situ)) 2)) (max (- (apply min (map (lambda (x) (car(cdr x))) situ)) 2))
                           ))
               ))

(define generate-aval-map (lambda (diagonol x y situ) (if (<= x (car (cdr diagonol))) (if (member (cons x y) situ) (if (= y (car (cdr (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (generate-aval-map diagonol x (+ 1 y) situ)) (cons (cons x y) (if (= y (car (cdr (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (generate-aval-map diagonol x (+ 1 y) situ)))) null)))

(define put-xy-in-lines (lambda (situ depth) (if (eq? situ null) (list (hash) (hash) (hash) (hash)) (let* ((xy (car situ)) (next (put-xy-in-lines (cdr situ) (+ depth 1))) (vertical (first next)) (horizontal (second next)) (topleft-bottomright (third next)) (topright-bottomleft (fourth next)) (value-vertical (hash-ref vertical (car xy) #f)) (value-horizontal (hash-ref horizontal (car (cdr xy)) #f)) (value-topleft-bottomright (hash-ref topleft-bottomright (- (car (cdr xy)) (car xy)) #f)) (value-topright-bottomleft (hash-ref topright-bottomleft (+ (car (cdr xy)) (car xy)) #f)) (selves? (if (= (remainder depth 2) AI-first) #t #f)))
;chez fist啥的得写出来
                                                                                                      (;....~!
                                                                                                       )))))
