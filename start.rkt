#lang scheme
(define cal2 (list "sw7" (list (list "PP" 22 33) (list "CC" 66 77) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 11 22) (list "Secret CC" 13 14) (list "Secret Pause" 69 1337))) (list "Meme time" 21 22))))
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

(define (title x)
  (cond
    ((appointment? x) (car x))
    ((calendar? x) (car x))
    (else (error "Cannot get title from non cal or app."))))

(define (content cal)
  (match cal
    [(list name) '()]
    [(list name (list a ...)) a]
    [(? appointment?) (error "Appointment not calendar")]
    [_ (error "Got garbage")]))

(define (starts-before? a b)
  (cond
    [(not (and (appointment? a) (appointment? b))) (error "both arguments needs to be appointments")]
    [else (< (fromtimestamp a) (fromtimestamp b))]))

(define (ends-after? a b)
  (cond
    [(not (and (appointment? a) (appointment? b))) (error "both arguments needs to be appointments")]
    [else (> (totimestamp a) (totimestamp b))]))

(define (appointments cal)
  (let appointments_iter ([cals (list cal)] [app '()])
    (let* ((cont (apply append (map content cals)))
           (cals (filter calendar? cont)))
      (if (empty? cals)
          (sort (append app cont) starts-before?)
          (appointments_iter cals (append app (filter appointment? cont)))))))

(define (fromtimestamp ap)
  (cond
    [(appointment? ap) (list-ref ap 1)]
    [else error("Not an appointment")]))

(define (totimestamp ap)
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
    [(calendar? cal) (list (title cal) (appointments cal))]
    [else (error "That is not a calendar")]))

(define (add-appointment cal appointment)
  (cond
    [(null? cal) (error "FAK")]
    [(empty? appointment) (error "FAK")]
    [(empty? (content cal)) (calendar (title cal) (list appointment))]
    [(list? (content cal)) (calendar (title cal) (append (content cal) (list appointment)))]
    [else (error "FAK")]))

(define (add-calendar cal-a cal-b)
  (cond
    [(null? cal-a) (error "FAK")]
    [(empty? appointment) (error "FAK")]
    [(empty? (content cal-a)) (calendar (title cal-a) (list cal-b))]
    [(list? (content cal-a)) (calendar (title cal-a) (append (content cal-a) (list cal-b)))]
    [else (error "FAK")]))

(define (find-appointments cal pred)
  (filter pred (appointments cal)))

(define (find-first-helper list pred)
  (let loop ([l list])
    (cond
      [(null? l) #f]
      [else (let ([a (car l)])
              (if (pred a) a (loop (cdr l))))])))

(define (find-first-appointment cal pred)
  (find-first-helper (appointments cal) pred))

(define (find-last-appointment cal pred)
  (find-first-helper (reverse (appointments cal)) pred))

(define (appointments-overlap? ap1 ap2)
  (cond
    [(or (<= (totimestamp ap1) (fromtimestamp ap2))
         (<= (totimestamp ap2) (fromtimestamp ap1))) #f]
    [else #t]))

(define (calendars-overlap? cal1 cal2)
  (let* ((aps1 (appointments cal1))
         (aps2 (appointments cal2)))
    (ormap (lambda (a)
             (ormap (lambda (b) (appointments-overlap? a b)) aps2)) aps1)))
  
;;HELPER FUNCTIONS


(define (range a [b null] [step 1])
  (cond
    [(null? b) (range 0 a step)] 
    [else
     (let range_iter ([f a] [l (list a)])
       (if (>= (+ f step) b)
           l
           (range_iter (+ f step) (append l (list (+ f step))))))]))


(define (days-in-range from-time to-time)
  (range
   (+ (- from-time (remainder from-time SECS_IN_A_DAY)) (-- SECS_IN_A_DAY))
   (if (> to-time SECS_IN_A_DAY)
       (-- (+ to-time (remainder to-time SECS_IN_A_DAY)))
       (-- SECS_IN_A_DAY))
   SECS_IN_A_DAY))


(define (start-and-end-of-month ts)
  (let* ([d (day-in-month ts)]
         [month-length (days-in-month (month ts) (year ts))]
         [month-start (if (= d 1)
                          ts
                          (- ts (* (-- d) SECS_IN_A_DAY)))]
         [month-end   (if (= d month-length)
                          ts
                          (+ ts (-- (* (- month-length (-- d)) SECS_IN_A_DAY))))])
    (list month-start month-end)))


(define (range-in-month ts)
  (match (start-and-end-of-month ts)
    [(list start end) (days-in-range start end)]))


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
      [(> day month-length) (month-day-helper (- day month-length) (++ month) year)]
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
  (format "~A/~A/~A ~A" (year ts) (month ts) (day-in-month ts) (name-of-day ts)))

(define (timestamp->civil-time ts)
  (format "~A:~A:~A" (hour ts) (minute ts) (second ts)))


(define (present-calendar-html cal from-time to-time)
  (let ((c (calendar (title cal) (filter (lambda (x)
                                           (appointments-overlap? x (appointment "fake" from-time to-time)))
                                         (appointments cal)))))
  (write-to-file "calendar.html" (html-document (title cal) (agenda c)))))

(define (agenda cal)
  (let* ([aps (appointments cal)]
         [cal-start (fromtimestamp (first aps))]
         [cal-end (totimestamp (first (sort aps ends-after?)))])
    (map
     (lambda (last-sec-in-day)
       (format "<h4>~A</h4><ul>~A</ul>"
               (timestamp->civil-date last-sec-in-day)
               (string-append*
                (map
                 (lambda (a)
                   (format "<li>~A from ~A to ~A</li>"
                           (title a)
                           (timestamp->civil-time (fromtimestamp a))
                           (timestamp->civil-time (totimestamp a))))
                 (find-appointments cal (lambda (ap) (appointments-overlap? ap (appointment "pseudo" (- last-sec-in-day (-- SECS_IN_A_DAY)) last-sec-in-day))))))))
     (days-in-range cal-start cal-end))))

(define (html-document title [body-content null])
  (let ([html_string (format "<!doctype html><title>~A</title>" title)])
    (if (null? body-content)
        html_string
        (string-append html_string (if (string? body-content)
                                       (format "<body>~A</body>" body-content)
                                       (format "<body>~A</body>" (string-append* body-content)))))))

(define (write-to-file filename content)
  (with-output-to-file filename (lambda () (display content))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   TEST   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define test-cal (eval (call-with-input-file "test_calendar" read) ns))

(define cal1 (calendar "Pepe" (list (appointment "yay" 1 11) (calendar "Lol" (list (appointment "wat" 3 4) (appointment "nay" 6 8))))))
(timestamp->civil-date (current-seconds))
;(map timestamp->civil-date (range (current-seconds) (+ (current-seconds) (* SECS_IN_A_DAY 14)) SECS_IN_A_DAY))


(calendar? cal1)
(calendar? (add-appointment cal1 (appointment "WOW" 23 24)))
(calendar? (add-calendar cal1 (calendar "WOW" (list (appointment "meme" 4 20) (appointment "dank" 1 2)))))
(define da-cal (add-calendar cal1 (calendar "WOW" (list (appointment "meme" 4 20) (appointment "dank" 1 2))))) 
(appointment? (appointment "yayaayayayay" 22 55))
(add-appointment cal1 (appointment "WOW" 23 24))

(find-first-appointment cal1 (lambda (a) (if (> (fromtimestamp a) 2) #t #f)))
(find-last-appointment cal1 (lambda (a) (if (> (fromtimestamp a) 20) #t #f)))

(calendars-overlap? cal1 cal2)
;(present-calendar-html test-cal 0 (current-seconds))
(write-to-file "test.html" (html-document (title test-cal) (agenda test-cal)))
(define oct-a (range-in-month (current-seconds)))
(define oct-b (range-in-month 1477958399))
(timestamp->civil-date (list-ref oct-a 0))
(timestamp->civil-date (list-ref oct-b 0))
(timestamp->civil-date (last oct-a))
(timestamp->civil-date (last oct-b))