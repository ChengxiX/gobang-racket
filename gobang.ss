;(import (chezscheme))
(define node (lambda (depth aim situ alphabeta) (
                                                 if (< depth aim)
                                                    (branch (if (= (remainder depth 2) 0) <= >=) depth aim situ alphabeta (if (= (remainder depth 2) 0) -1e20 1e20) (let ((dia (generate-diagonol situ))) (generate-aval-map dia (car (car dia)) (cdr (car dia)) situ)))
                                                    (list (value situ) situ)
                                                    )))

(define branch (lambda (opposite depth aim situ alpha beta able) (
                                                          let ((x (node (+ depth 1) aim (cons (car able) situ) beta))) (if (and (opposite (car x) alpha) (not (eq? (cdr able) null)))
                                                                                                                           (let ((y (branch opposite depth aim situ alpha (if (opposite beta (car x)) (car x) beta) (cdr able)))) (if (opposite (car y) (car x)) x y)) x
                                                                                                                           ))))

(define w (make-eq-hashtable))
(define x (make-eq-hashtable))
(define y (make-eq-hashtable))
(define z (make-eq-hashtable))
(define value (lambda (situ) (
                              let ((situ-table (let () (set! w (make-eq-hashtable)) (set! x (make-eq-hashtable)) (set! y (make-eq-hashtable)) (set! z (make-eq-hashtable)) (put-xy-in-lines situ 0) (list w x y z)))) (+ (value_lines_ (car situ-table) (vector->list (hashtable-keys (car situ-table))))
                                                                             (value_lines_ (car (cdr situ-table)) (vector->list (hashtable-keys (car (cdr situ-table)))))
                                                                             (value_lines_ (car (cdr (cdr situ-table))) (vector->list (hashtable-keys (car (cdr (cdr situ-table))))))
                                                                             (value_lines_ (car (cdr (cdr (cdr situ-table)))) (vector->list (hashtable-keys (car (cdr (cdr (cdr situ-table)))))))
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

(define put-xy-in-lines (lambda (situ depth) (if (eq? situ null) null (let* ((xy (car situ)) (next (put-xy-in-lines (cdr situ) (+ depth 1))) (selves? (= (remainder depth 2) AI-first)))

                                                                                                      (hashtable-set! w (car xy) (cons (cons (cdr xy) selves?) (hashtable-ref w (car xy) null)))
                                                                                                      (hashtable-set! x (cdr xy) (cons (cons (car xy) selves?) (hashtable-ref x (cdr xy) null)))
                                                                                                      (hashtable-set! y (- (cdr xy) (car xy)) (cons (cons (car xy) selves?) (hashtable-ref y (- (cdr xy) (car xy)) null)))
                                                                                                      (hashtable-set! z (+ (cdr xy) (car xy)) (cons (cons (car xy) selves?) (hashtable-ref z (+ (cdr xy) (car xy)) null)))
                                                                                                       ))))
                                                                                                       
(define sortbycar (lambda (line) (sort (lambda (x y) (< (car x) (car y))) line)))

(define valueline (lambda (line) (if (eq? (cdr line) null) (list 0 (car (car line)) (cdr (car line)) 1) (let ((a (valueline (cdr line)))) 
                                                            (if (= (car (cdr (cdr (cdr a)))) 5) (list (+ (if (car (cdr (cdr a))) (* attack-ratio chengwu) (* -1 chengwu)) (car a)) (car (car line)) (cdr (car line)) 0) (let ((diff (- (car (cdr a)) (car (car line))))) (if (= diff 1) (if (not (xor (cdr (car line)) (car (cdr (cdr a))))) (list (car a) (car (car line)) (cdr (car line)) (+ 1 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (= (round (car a)) (car a)) (if (cdr (car line)) (* -1 (hashtable-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0)) (* (hashtable-ref valuetable (- (car (cdr (cdr (cdr a)))) 0.5) 0) attack-ratio)) 0)) (car (car line)) (cdr (car line)) 0.5)) (if (= diff 2) (if (not (xor (cdr (car line)) (car (cdr (cdr a))))) (list (car a) (car (car line)) (cdr (car line)) (+ 0.5 (car (cdr (cdr (cdr a)))))) (list (+ (car a) (if (cdr (car line)) (hashtable-ref valuetable (car (cdr (cdr (cdr a)))) 0) (* (hashtable-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (cdr (car line)) 1)) (list (+ (car a) (if (not (car (cdr (cdr a)))) (* -1 (hashtable-ref valuetable (car (cdr (cdr (cdr a)))) 0)) (* (hashtable-ref valuetable (car (cdr (cdr (cdr a)))) 0) attack-ratio))) (car (car line)) (cdr (car line)) 1))) ))
                                                            ))))
(define value_lines_ (lambda (table keys) (if (eq? keys null) 0 (+ (car (let ((aim-line (sortbycar (hashtable-ref table (car keys) #f)))) (valueline (cons (cons -100000 (not (cdr (car aim-line)))) aim-line)))) (value_lines_ table (cdr keys))))))
(define null '())
(define chengwu 100000)
(define huosi 700)
(define chongsi 400)
(define huosan 40)
(define miansan 8)
(define huoer 10)
(define mianer 1)
(define valuetable (make-eq-hashtable))
(hashtable-set! valuetable 4.5 chengwu)
(hashtable-set! valuetable 4 huosi)
(hashtable-set! valuetable 3.5 chongsi)
(hashtable-set! valuetable 3 huosan)
(hashtable-set! valuetable 2.5 miansan)
(hashtable-set! valuetable 2 huoer)
(hashtable-set! valuetable 1.5 mianer)
(define xor (lambda (a b) (and (or a b) (not (and a b)))))

(define attack-ratio 1)
;(define AI-first 1)

;testing
;(node 0 2 (list (cons 7 6) (cons 7 7)) 1e20)