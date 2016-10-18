#lang scheme
(define cal1 (list "mah name"))
(define cal2 (list "sw7" (list (list "PP" 1 2) (list "CC" 6 7) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 1 2) (list "Secret CC" 6 7) (list "Secret Pause" 69 1337))) (list "Meme time" 5 2))))
(define app1 (list "some text" 213123123 1222223))

(define (extract-title x)
  (cond
    ((appointment? x) (car x))
    ((calendar? x) (car x))
    (else (error "Cannot get title from non cal or app."))))

(define (extract-content cal)
  (match cal
    [(list name) '()]
    [(list name (list a ...)) a]
    [(? appointment?) (error "Appointment not calendar")]
    [_ (error "Got garbage")]))

(define (extract-appointments cal)
  (let* ((content (extract-content cal))
         (cals (filter calendar? content)))
    (append (filter appointment? content) (apply append (map extract-appointments cals)))))

(define (appointment? x)
  (match x
    [(list (? string?) (? number? from) (? number? to)) (< from to)]
    [_ #f]))

(define (calendar? x)
  (match x
    [(list (? string?) (? list?)) #t]
    [_ #f]))

(define (flatten-calendar cal)
  (cond
    (((negate calendar?) cal) (error "That is not a calendar"))
    (else
     (list (extract-title cal) (extract-appointments cal)))))

(define (add-appointment cal appointment)
  (cond
    ((null? cal) (error "FAK"))
    ((empty? appointment) (error "FAK"))
    ((empty? (cdr cal)) (list (car cal) (cons appointment '())))
    ((list? (cdr cal)) (list (car cal) (cons appointment (extract-content cal))))
    (else (error "FAK"))))

(define (timestamp->civil ts)
  (let* ((s (modulo ts 86400))
         (ts (floor (/ ts 86400)))
         (h (floor (/ s 3600)))
         (m (modulo (floor (/ s 60)) 60))
         (s (modulo s 60))
         (x (+ (floor (/ (+ (* ts 4) 102032) 146097)) 15))
         (b (- (+ (+ ts 2442113) x) (floor (/ x 4))))
         (c (floor (/ (- (* b 20) 2442) 7305)))
         (d (- (- b (* 365 c)) (floor (/ c 4))))
         (e (floor (/ (* d 1000) 30601)))
         (f (- (- d (* e 30)) (* e (floor (/ 601 1000))))))
    (cond
      ((< e 14) (list (- c 4716) (- e 1) f h m s))
      (else (list (- c 4715) (- e 13) f h m s)))))


;(add-appointment cal1 app1)

;(add-appointment (add-appointment cal1 app1) app1)

;(add-appointment (add-appointment cal1 app1) (list "memes" 123 456))

;(extract-content (add-appointment (add-appointment cal1 app1) (list "memes" 123 456)))
;(display "FILTER\n")
;(filter appointment? (extract-content (add-appointment (add-appointment cal1 app1) (list "memes" 123 456))))

;(filter appointment? (extract-content cal2))
;(filter calendar? (extract-content cal2))

(flatten-calendar cal2)
cal2
(current-seconds)
(timestamp->civil (current-seconds))