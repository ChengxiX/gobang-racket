#lang racket
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim)
                                                    (branch (if (= (remainder depth 2) 0) < >) depth aim situ alphabeta -1e20 (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (cdr (car dia)) situ)))
                                                    (list (value situ) situ)
                                                    )))

(define branch (lambda (opposite depth aim situ alpha beta able) (
                                                          let ((x (node (+ depth 1) aim (cons situ (car able)) beta))) (if (and (opposite (car x) alpha) (not (eq? (cdr able) null)))
                                                                                                                           (let ((y (branch opposite depth aim situ alpha (min x beta) (cdr able)))) (if (opposite (car y) (car x)) x y)) x
                                                                                                                           ))))
(define AI-first 0) ;如果是0则是AI先手，如果是1则是AI后手（人先手）

;value函数返回值结构：数字
(define value (lambda (situ) (
                              let ((situ-table (put-xy-in-lines situ 0))) (+ (value_lines_ (car situ-table) (hash-keys (car situ-table)))
                                                                             (value_lines_ (car (cdr situ-table)) (hash-keys (car (cdr situ-table))))
                                                                             (value_lines_ (car (cdr (cdr situ-table))) (hash-keys (cdr (cdr (car situ-table)))))
                                                                             (value_lines_ (car (cdr (cdr (cdr situ-table)))) (hash-keys (car (cdr (cdr (cdr situ-table))))))
                                                                             )
                              )))

(define generate-diagonol (lambda (situ)
               (list 
                     (
                      cons (max 0 (- (apply min (map (lambda (x) (car x)) situ)) 2)) (max 0 (- (apply min (map (lambda (x) (car(cdr x))) situ)) 2))
                           )
                     (
                      cons (min 14 (+ (apply max (map (lambda (x) (car x)) situ)) 2)) (min 14 (+ (apply max (map (lambda (x) (car (cdr x))) situ)) 2))
                           )
                     )
               ))

(define generate-aval-map (lambda (diagonol x y situ) (if (<= x (car (car (cdr diagonol)))) (if (member (cons x y) situ) (if (= y (cdr (car (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (car (cdr (car diagonol))) situ) (generate-aval-map diagonol x (+ 1 y) situ)) (cons (cons x y) (if (= y (cdr (car (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (cdr (car diagonol)) situ) (generate-aval-map diagonol x (+ 1 y) situ)))) null)))

(define put-xy-in-lines (lambda (situ depth) (if (eq? situ null) (list (hash) (hash) (hash) (hash)) (let* ((xy (car situ)) (next (put-xy-in-lines (cdr situ) (+ depth 1))) (vertical (first next)) (horizontal (second next)) (topleft-bottomright (third next)) (topright-bottomleft (fourth next)) (value-vertical (hash-ref vertical (car xy) #f)) (value-horizontal (hash-ref horizontal (car (cdr xy)) #f)) (value-topleft-bottomright (hash-ref topleft-bottomright (- (car (cdr xy)) (car xy)) #f)) (value-topright-bottomleft (hash-ref topright-bottomleft (+ (car (cdr xy)) (car xy)) #f)) (selves? (if (= (remainder depth 2) AI-first) #t #f)))
;chez fist啥的得写出来,#t是ai下的,#f是人类（对手下的）
                                                                                                      (list (if value-vertical (hash-set (hash-remove vertical (car xy)) (car xy) (cons (cons (car (cdr xy)) selves?) value-vertical)) (hash-set vertical (car xy) (list (cons (car (cdr xy)) selves?))))
                                                                                                            (if value-horizontal (hash-set (hash-remove horizontal (car (cdr xy))) (car (cdr xy)) (cons (cons (car xy) selves?) value-horizontal)) (hash-set horizontal (car (cdr xy)) (list (cons (car xy) selves?))))
                                                                                                            (if value-topleft-bottomright (hash-set (hash-remove topleft-bottomright (- (car (cdr xy)) (car xy))) (- (car (cdr xy)) (car xy)) (cons (cons (+ (car (cdr xy)) (car xy)) selves?) value-topleft-bottomright)) (hash-set topleft-bottomright (- (car (cdr xy)) (car xy)) (list (cons (+ (car (cdr xy)) (car xy)) selves?))))
                                                                                                            (if value-topright-bottomleft (hash-set (hash-remove topright-bottomleft (+ (car (cdr xy)) (car xy))) (+ (car (cdr xy)) (car xy)) (cons (cons (+ (car (cdr xy)) (car xy)) selves?) value-topright-bottomleft)) (hash-set topright-bottomleft (+ (car (cdr xy)) (car xy)) (list (cons (- (car (cdr xy)) (car xy)) selves?))))
                                                                                                            )
                                                                                                       ))))
                                                                                                       
(define sortbycar (lambda (line) (sort line (lambda (x y) (< (car x) (car y))))))
;valueline递归返回(value 自己的坐标 落子方 连击次数)
;需要((1 #t) (3 #f))
(define valueline (lambda (line) (if (eq? line null) null (let ((a (valueline (cdr line)))) 
                                                            (if (eq? a null) (list 0 (car (car line)) (car (cdr (car line))) 1) (if (> (car (cdr (cdr (cdr a)))) 4) (list (+ (if (car (cdr (cdr a))) (* attack-ratio 成五) (* -1 成五)) (car a)) (car (car line)) (car (cdr (car line))) 0) (let ((diff (- (car (cdr a)) (car (car line))))) (if (= diff 1) (if (not (xor (car (cdr (car line))) (car (cdr (cdr a))))) (list (car a) (car (car line)) (car (cdr (car line))) (+ 1 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (= (round (car a)) (car a)) (if (car (cdr (car line))) (* -1 (hash-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0)) (* (hash-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0) attack-ratio)) 0)) (car (car line)) (car (cdr (car line))) 0.5)) (if (< diff 3) (if (not (xor (car (cdr (car line))) (car (cdr (cdr a))))) (list (+ (car a) (if (not (car (cdr (cdr a)))) (* -1 (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0)) (* (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (car (cdr (car line))) (+ 1 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (car (cdr (car line))) (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) (* (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (car (cdr (car line))) 1)) (list (+ (car a) (if (not (car (cdr (cdr a)))) (* -1 (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0)) (* (hash-ref valuetable (+ (car a) 0.5) 0) attack-ratio))) (car (car line)) (car (cdr (car line))) 1))) ))
                                                            )))))

(define value_lines_ (lambda (table keys) (if (eq? keys null) 0 (+ (let ((aim-line (sortbycar (hash-ref table (car keys))))) (valueline (cons (-100000 (not (car (cdr (car aim-line)))))))) (value_lines_ table (cdr keys))))))

(define 成五 10000)
(define 活四 100)
(define 冲四 50)
(define 活三 40)
(define 眠三 10)
(define 活二 5)
(define 眠二 1)
(define valuetable (hash '5 成五 '4.5 成五 '4 活四 '3.5 冲四 '3 活三 '2.5 眠三 '2 活二 '1.5 眠二))
(define attack-ratio 1);进攻系数大于1进攻型，小于1防守型

(node 0 1 '((1 1) (2 1)) 0)