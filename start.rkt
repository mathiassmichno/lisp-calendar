#lang scheme
(define cal2 (list "sw7" (list (list "PP" 1 2) (list "CC" 6 7) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 1 2) (list "Secret CC" 6 7) (list "Secret Pause" 69 1337))) (list "Meme time" 5 2))))
(define app1 (list "some text" 213123123 1222223))

(define (calendar title [content null])
  (cond
    [((negate string?) title) (error "Title must be provided")]
    [(null? content) (list title '("No"))]
    [(list? content)
     (list title (filter (lambda (x)
                           (or (appointment? x) (calendar? x ))) content))]
    [else (error "Content can only be list if supplied")]))

(define (appointment title from to)
  (let ((ap (list title from to)))
    (match ap
      [(list (? string? ti) (? number? fr) (? (lambda (x) (and (number? x) (> x from))) to)) (list ti fr to)]
      [_ (error "Not valid appointment")])))

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

(define (extract-fromtimestamp ap)
  (cond
    [(appointment? ap) (list-ref ap 1)]
    [else error("Not an appointment")]))

(define (extract-totimestamp ap)
  (cond
    [(appointment? ap) (list-ref ap 2)]
    [else error("Not an appointment")]))

(define (appointment? x)
  (match x
    [(list (? string?) (? number? from) (? number? to)) (< from to)]
    [_ #f]))

(define (calendar? x)
  (match x
    [(list (? string?) (or (? empty?) (list (or (? calendar?) (? appointment?)) ... ))) #t]
    [_ #f]))

(define (flatten-calendar cal)
  (cond
    [(calendar? cal) (list (extract-title cal) (extract-appointments cal))]
    [else (error "That is not a calendar")]))

(define (add-appointment cal appointment)
  (cond
    [(null? cal) (error "FAK")]
    [(empty? appointment) (error "FAK")]
    [(empty? (extract-content cal)) (calendar (extract-title cal) (list appointment))]
    [(list? (extract-content cal)) (calendar (extract-title cal) (append (extract-content cal) (list appointment)))]
    [else (error "FAK")]))

(define (add-calendar cal-a cal-b)
  (cond
    [(null? cal-a) (error "FAK")]
    [(empty? appointment) (error "FAK")]
    [(empty? (extract-content cal-a)) (calendar (extract-title cal-a) (list cal-b))]
    [(list? (extract-content cal-a)) (calendar (extract-title cal-a) (append (extract-content cal-a) (list cal-b)))]
    [else (error "FAK")]))

(define (find-appointments cal pred)
  (filter pred (extract-appointments cal)))

(define (find-first-helper list pred)
  (let loop ([l list])
    (cond
      [(null? l) #f]
      [else (let ([a (car l)])
              (if (pred a) a (loop (cdr l))))])))

(define (find-first-appointment cal pred)
  (find-first-helper (extract-appointments cal) pred))

(define (find-last-appointment cal pred)
  (find-first-helper (reverse (extract-appointments cal)) pred))
  
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   TEST   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cal1 (calendar "Pepe" (list (appointment "yay" 22 33) (calendar "Lol" (list (appointment "wat" 123 292) (appointment "nay" 456 788))))))
cal1
(flatten-calendar cal1)
(timestamp->civil-date (current-seconds))
;(map timestamp->civil-date (range (current-seconds) (+ (current-seconds) (* SECS_IN_A_DAY 14)) SECS_IN_A_DAY))

(define (fak? x)
  (match x
    [(list (? string?) (or (? empty?) (list (or (? string?) (? number?)) ... ))) #t]
    [_ #f]))

(calendar? cal1)
(calendar? (add-appointment cal1 (appointment "WOW" 23 24)))
(appointment? (appointment "yayaayayayay" 22 55))
(add-appointment cal1 (appointment "WOW" 23 24))

(find-first-appointment cal1 (lambda (a) (if (> (extract-fromtimestamp a) 20) #t #f)))
(find-last-appointment cal1 (lambda (a) (if (> (extract-fromtimestamp a) 20) #t #f)))