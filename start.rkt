#lang scheme
(define cal1 (list "mah name"))
(define cal2 (list "sw7" (list (list "PP" 1 2) (list "CC" 6 7) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 1 2) (list "Secret CC" 6 7) (list "Secret Pause" 69 1337))) (list "Meme time" 5 2))))
(define app1 (list "some text" 213123123 1222223))

(define ++ add1)
(define -- sub1)

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

;;HELPER FUNCTIONS
(define range (lambda (a [b null] [step 1])
  (cond
    [(null? b) (range 0 a step)] 
    [(>= a b) '()]
    [else (cons a (range (+ a step) b step))])))

;;TIMESTAMPS
(define SECS_IN_A_DAY 86400)

(define (second-in-day ts)
  (modulo ts SECS_IN_A_DAY))

(define (second ts)
  (modulo (second-in-day ts) 60))

(define (minute ts)
  (modulo (quotient (second-in-day ts) 60) 60))

(define (hour ts)
  (modulo (quotient (second-in-day ts) 3600) 24))

(define (day-in-forever ts) ;forever is a timeperiod from january 1st 1970 to the end of all time
  (quotient ts SECS_IN_A_DAY))

(define (day-in-week ts)
  (modulo (+ (day-in-forever ts) 3) 7))

(define (name-of-day ts)
  (list-ref '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") (day-in-week ts)))

(define (days-in-month month year)
  (let ((month (-- month)))
    (cond
      [(leap-year? year) (list-ref '(31 29 31 30 31 30 31 31 30 31 30 31) month)]
      [else (list-ref '(31 28 31 30 31 30 31 31 30 31 30 31) month)])))

(define (month-day-helper day month year)
  (let ((month-length (days-in-month month year)))
    (cond
      [(>= day month-length) (month-day-helper (- day month-length) (++ month) year)]
      [else (cons day month)])))

(define (day-in-month ts)
  (car (month-day-helper (day-in-year ts) 1 (year ts))))

(define (month ts)
  (cdr (month-day-helper (day-in-year ts) 1 (year ts))))
  
(define (leap-year? year)
  (cond
    [((negate number?) year) (error "Year provided not a number")]
    [(eq? (modulo year 400) 0) #t]
    [(eq? (modulo year 100) 0) #f]
    [(eq? (modulo year 4) 0) #t]
    [else #f]))

(define (day-year-helper day year)
  (let ((year-length (if (leap-year? year) 366 365)))
    (cond
      [(>= day year-length) (day-year-helper (- day year-length) (++ year))]
      [else (cons (++ day) year)])))

(define (day-in-year ts)
  (car (day-year-helper (day-in-forever ts) 1970)))

(define (year ts)
  (cdr (day-year-helper (day-in-forever ts) 1970)))


(define (timestamp->civil-date ts)
  (format "~A/~A/~A ~A | ~A:~A:~A" (year ts) (month ts) (day-in-month ts) (name-of-day ts) (hour ts) (minute ts) (second ts)))


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
(timestamp->civil-date (current-seconds))
(timestamp->civil-date 11847456550)
(map timestamp->civil-date (range (current-seconds) (+ (current-seconds) (* SECS_IN_A_DAY 14)) SECS_IN_A_DAY))




