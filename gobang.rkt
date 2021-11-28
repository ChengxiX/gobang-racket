#lang racket
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim) ;(
                                                                   ;if (= (remainder depth 2) 0)
                                                                      ;(maxnode depth aim situ 0 alphabeta -1e20 (let ((dia (area situ))) (next dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                                      ;(minnode depth aim situ 0 alphabeta 1e20 (let ((dia (area situ))) (next dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                                      ;)
                                                    (maxmin (remainder depth 2) depth aim situ 0 alphabeta -1e20 (let ((dia (area situ))) (next dia (car (car dia)) (car (cdr (car dia))) situ)))
                                                    (list (value situ) situ)
                                                    )))
;(define maxnode (lambda (depth aim situ alpha beta able) (
;                                                          let ((x (node (+ depth 1) aim (cons situ (car able)) beta))) (if (and (< (car x) alpha) (not (eq? able null)))
;                                                                                                                           (let (y (maxnode depth aim situ alpha (max x beta) (cdr able))) (if (< (car y) (car x)) x y)) x
;                                                                                                                           ))))
;(define minnode (lambda (depth aim situ alpha beta able) (
;                                                          let ((x (node (+ depth 1) aim (cons situ (car able)) beta))) (if (and (> (car x) alpha) (not (eq? able null)))
;                                                                                                                           (let (y (maxnode depth aim situ alpha (min x beta) (cdr able))) (if (> (car y) (car x)) x y)) x
;                                                                                                                           ))))

(define maxmin (lambda (re depth aim situ alpha beta able) (
                                                          let ((x (node (+ depth 1) aim (cons situ (car able)) beta)) (fan (if (= re 0) < >))) (if (and (fan (car x) alpha) (not (eq? able null)))
                                                                                                                           (let ((y (maxmin re depth aim situ alpha (min x beta) (cdr able)))) (if (fan (car y) (car x)) x y)) x
                                                                                                                           ))))
(define xianshou? 0) ;如果是0则是AI先手，如果是1则是AI后手（人先手）

;value函数返回值结构：数字
(define value (lambda (situ) (
                              1
                              )))

(define area (lambda (situ)
               (cons (
                      cons (min 14 (+ (apply max (map (lambda (x) (car x)) situ)) 2)) (min 14 (+ (apply max (map (lambda (x) (car (cdr x))) situ))))
                           )
                     (
                      cons (max 0 (- (apply min (map (lambda (x) (car x)) situ)) 2)) (max (- (apply min (map (lambda (x) (car(cdr x))) situ)) 2))
                           ))
               ))

(define next (lambda (diagonol x y situ) (if (<= x (car (cdr diagonol))) (if (member (cons x y) situ) (if (= y (car (cdr (cdr diagonol)))) (next diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (next diagonol x (+ 1 y) situ)) (cons (cons x y) (if (= y (car (cdr (cdr diagonol)))) (next diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (next diagonol x (+ 1 y) situ)))) null)))

(define classify (lambda (situ depth) [if (eq? situ null) null (if (= (remainder depth 2) xianshou?) (cons (cons (car situ) 0) (classify (cdr situ) (+ 1 depth))) (cons (cons (car situ) 1) (classify (cdr situ) (+ 1 depth))))])) ;1是对方下的棋子，0是我方下的,输出示例'(((1 0) . 0) ((3 4) . 1) ((5 6) . 0))
(define vertical (lambda (situ depth) (if (eq? situ null) (hash) (let* ((a (vertical (cdr situ) (+ depth 1))) (xy (car situ)) (valueofx (hash-ref a (car xy) null)) (selves? (if (= (remainder depth 2) xianshou?) 0 1))) ;1是对方下的棋子，0是我方下的
                                                                        (if (eq? valueofx null) (hash-set a (car xy) (list (cons (car (cdr xy)) selves?))) (hash-set (hash-remove a (car xy)) (car xy) (cons (cons (car (cdr xy)) selves?) valueofx))))))) ;hash表在chez scheme里面有不同的写法。不可变散列表
(vertical '((1 3) (1 4) (1 5) (2 4) (6 7)) 0)