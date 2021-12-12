#lang racket
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim)
                                                    (branch (if (= (remainder depth 2) 0) <= >=) depth aim situ alphabeta (if (= (remainder depth 2) 0) -1e20 1e20) (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (cdr (car dia)) situ)))
                                                    (list (value situ) situ)
                                                    )))

(define branch (lambda (opposite depth aim situ alpha beta able) (
                                                          let ((x (node (+ depth 1) aim (cons (car able) situ) beta))) (if (and (opposite (car x) alpha) (not (eq? (cdr able) null)))
                                                                                                                           (let ((y (branch opposite depth aim situ alpha (if (opposite beta (car x)) (car x) beta) (cdr able)))) (if (opposite (car y) (car x)) x y)) x
                                                                                                                           ))))

;value函数返回值结构：数字
(define value (lambda (situ) (
                              let ((situ-table (put-xy-in-lines situ 0))) (+ (value_lines_ (car situ-table) (hash-keys (car situ-table)))
                                                                             (value_lines_ (car (cdr situ-table)) (hash-keys (car (cdr situ-table))))
                                                                             (value_lines_ (car (cdr (cdr situ-table))) (hash-keys (car (cdr (cdr situ-table)))))
                                                                             (value_lines_ (car (cdr (cdr (cdr situ-table)))) (hash-keys (car (cdr (cdr (cdr situ-table))))))
                                                                             )
                              )))

(define generate-diagonol (lambda (situ)
               (list 
                     (
                      cons (max 0 (- (apply min (map (lambda (x) (car x)) situ)) 2)) (max 0 (- (apply min (map (lambda (x) (cdr x)) situ)) 2))
                           )
                     (
                      cons (min 14 (+ (apply max (map (lambda (x) (car x)) situ)) 2)) (min 14 (+ (apply max (map (lambda (x) (cdr x)) situ)) 2))
                           )
                     )
               ))

(define generate-aval-map (lambda (diagonol x y situ) (if (<= x (car (car (cdr diagonol)))) (if (member (cons x y) situ) (if (= y (cdr (car (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (cdr (car diagonol)) situ) (generate-aval-map diagonol x (+ 1 y) situ)) (cons (cons x y) (if (= y (cdr (car (cdr diagonol)))) (generate-aval-map diagonol (+ 1 x) (cdr (car diagonol)) situ) (generate-aval-map diagonol x (+ 1 y) situ)))) null)))

(define put-xy-in-lines (lambda (situ depth) (if (eq? situ null) (list (hash) (hash) (hash) (hash)) (let* ((xy (car situ)) (next (put-xy-in-lines (cdr situ) (+ depth 1))) (vertical (car next)) (horizontal (car (cdr next))) (topleft-bottomright (car (cdr (cdr next)))) (topright-bottomleft (car (cdr (cdr (cdr next))))) (value-vertical (hash-ref vertical (car xy) #f)) (value-horizontal (hash-ref horizontal (cdr xy) #f)) (value-topleft-bottomright (hash-ref topleft-bottomright (- (cdr xy) (car xy)) #f)) (value-topright-bottomleft (hash-ref topright-bottomleft (+ (cdr xy) (car xy)) #f)) (selves? (= (remainder depth 2) AI-first)))
;chez fist啥的得写出来,#t是ai下的,#f是人类（对手下的）
                                                                                                      (list (if value-vertical (hash-set (hash-remove vertical (car xy)) (car xy) (cons (cons (cdr xy) selves?) value-vertical)) (hash-set vertical (car xy) (list (cons (cdr xy) selves?))))
                                                                                                            (if value-horizontal (hash-set (hash-remove horizontal (cdr xy)) (cdr xy) (cons (cons (car xy) selves?) value-horizontal)) (hash-set horizontal (cdr xy) (list (cons (car xy) selves?))))
                                                                                                            (if value-topleft-bottomright (hash-set (hash-remove topleft-bottomright (- (cdr xy) (car xy))) (- (cdr xy) (car xy)) (cons (cons (+ (cdr xy) (car xy)) selves?) value-topleft-bottomright)) (hash-set topleft-bottomright (- (cdr xy) (car xy)) (list (cons (+ (cdr xy) (car xy)) selves?))))
                                                                                                            (if value-topright-bottomleft (hash-set (hash-remove topright-bottomleft (+ (cdr xy) (car xy))) (+ (cdr xy) (car xy)) (cons (cons (+ (cdr xy) (car xy)) selves?) value-topright-bottomleft)) (hash-set topright-bottomleft (+ (cdr xy) (car xy)) (list (cons (- (cdr xy) (car xy)) selves?))))
                                                                                                            )
                                                                                                       ))))
                                                                                                       
(define sortbycar (lambda (line) (sort line (lambda (x y) (< (car x) (car y))))))
;valueline递归返回(value 自己的坐标 落子方 连击次数)
;需要((1 #t) (3 #f))
(define valueline (lambda (line) (if (eq? (cdr line) null) (list 0 (car (car line)) (cdr (car line)) 1) (let ((a (valueline (cdr line)))) 
                                                            (if (= (car (cdr (cdr (cdr a)))) 5) (list (+ (if (car (cdr (cdr a))) (* attack-ratio 成五) (* -1 成五)) (car a)) (car (car line)) (cdr (car line)) 0) (let ((diff (- (car (cdr a)) (car (car line))))) (if (= diff 1) (if (not (xor (cdr (car line)) (car (cdr (cdr a))))) (list (car a) (car (car line)) (cdr (car line)) (+ 1 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (= (round (car a)) (car a)) (if (cdr (car line)) (* -1 (hash-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0)) (* (hash-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0) attack-ratio)) 0)) (car (car line)) (cdr (car line)) 0.5)) (if (= diff 2) (if (not (xor (cdr (car line)) (car (cdr (cdr a))))) (list (car a) (car (car line)) (cdr (car line)) (+ 0.5 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (cdr (car line)) (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) (* (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (cdr (car line)) 1)) (list (+ (car a) (if (not (car (cdr (cdr a)))) (* -1 (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0)) (* (hash-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (cdr (car line)) 1))) ))
                                                            ))))

(define value_lines_ (lambda (table keys) (if (eq? keys null) 0 (+ (car (let ((aim-line (sortbycar (hash-ref table (car keys))))) (valueline (cons (cons -100000 (not (cdr (car aim-line)))) (hash-ref table (car keys)))))) (value_lines_ table (cdr keys))))))

(define 成五 100000)
(define 活四 1000)
(define 冲四 50)
(define 活三 40)
(define 眠三 10)
(define 活二 5)
(define 眠二 1)
(define valuetable (hash '4.5 成五  '4 活四 '3.5 冲四 '3 活三 '2.5 眠三 '2 活二 '1.5 眠二))
;(define attack-ratio 0.8);进攻系数大于1进攻型，小于1防守型；建议先手时进攻，后手时防守
;(define AI-first 0) ;如果是0则是人先手，如果是1则是AI先手，且AI先手时深度为偶数，人先手时为奇数

;test/debugging
;(node 0 5 (list (cons 9 9)) 1e20)

;获取console输入
(define get-two-from-console (lambda () (cons (string->int (read-line)) (read-line))))


(define index-li (lambda (depth index li) (if (= index depth) (car li) (index-li (+ 1 depth) index (cdr li))))) ;(index-li 1 n list) 
(define loop (lambda (situ) (let ((res (index-li 1 aim-depth-global (node 0 aim-depth-global situ 1e20)))) (write res) (loop (cons (get-two-from-console) (cons res situ))))))

;主程序
(define AI-first (read-line))
(define attack-ratio (read-line))
(define aim-depth-global (read-line))
(if (= AI-first 1) (let () (write (cons 8 8)) (loop (list (get-two-from-console) (cons 8 8)))) (loop (list (get-two-from-console))))