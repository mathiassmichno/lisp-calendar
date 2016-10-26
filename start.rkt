#lang scheme
(define cal2 (list "sw7" (list (list "PP" 22 33) (list "CC" 66 77) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 11 22) (list "Secret CC" 13 14) (list "Secret Pause" 69 1337))) (list "Meme time" 21 22))))
(define app1 (list "some text" 213123123 1222223))

(define (calendar title [content '()])
  (let ([cal (list title content)])
    (with-calendar cal
      (lambda (cal)
        cal))))

(define (appointment title from to)
  (let ((ap (list title from to)))
    (with-appointment ap
      (lambda (ap)
        ap))))

(define ++ add1)
(define -- sub1)

(define (title x)
  (cond
    ((appointment? x) (car x))
    ((calendar? x) (car x))
    (else (error "Cannot get title from non cal or app."))))

(define (with-calendar cal f)
  (if (calendar? cal)
      (f cal)
      (error "Not a calendar")))

(define (with-appointment app f)
  (if (appointment? app)
      (f app)
      (error "Not an appointment")))

(define (content cal)
  (with-calendar
    cal
    (lambda (c)
      (match c
        [(list name) '()]
        [(list name (list a ...)) a]))))

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
  (with-appointment
    ap
    (lambda (a)
      (list-ref a 1))))

(define (totimestamp ap)
  (with-appointment
    ap
    (lambda (a)
      (list-ref a 2))))

(define (appointment? x)
  (match x
    [(list (? string?) (? number? from) (? number? to)) (< from to)]
    [_ #f]))

(define (calendar? x)
  (match x
    [(list (? string?) (or (? empty?) (list (or (? calendar?) (? appointment?)) ... ))) #t]
    [_ #f]))

(define (flatten-calendar cal)
  (with-calendar
    cal
    (lambda (c)
      (list (title cal) (appointments cal)))))

(define (add-appointment cal appointment)
  (with-calendar
    cal
    (lambda (c)
      (with-appointment
        appointment
        (lambda (a)
          (cond
            [(empty? (content c)) (calendar (title c) (list a))]
            [(list? (content c)) (calendar (title c) (append (content c) (list a)))]))))))

(define (add-calendar cal-a cal-b)
  (with-calendar cal-a
    (lambda (cal-a)
      (with-calendar cal-b
        (lambda (cal-b)
          (calendar (title cal-a) (append (content cal-a) (list cal-b))))))))

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
  (and (< (fromtimestamp ap1) (totimestamp ap2))
       (< (fromtimestamp ap2) (totimestamp ap1))))

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
       (if (> (+ f step) b)
           l
           (range_iter (+ f step) (append l (list (+ f step))))))]))


(define (days-in-range from-time to-time)
  (range
   (+ (- from-time (remainder from-time SECS_IN_A_DAY)) (-- SECS_IN_A_DAY))
   (if (> to-time SECS_IN_A_DAY)
       (-- (+ to-time (- SECS_IN_A_DAY (remainder to-time SECS_IN_A_DAY))))
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
                          (+ ts (* (- month-length d) SECS_IN_A_DAY)))])
    (list month-start month-end)))


(define (range-in-month ts)
  (match (start-and-end-of-month ts)
    [(list start end) (days-in-range start end)]))

(define (pseudo-appointment end-time)
  (appointment "pseudo" (- end-time (-- SECS_IN_A_DAY)) end-time))

(define (leftpad-string str width char)
  (format "~A~A"
          (make-string
           (- width (string-length str))
           char)
          str))

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

(define (pad-time t)
  (leftpad-string (number->string t) 2 #\0))

(define (timestamp->civil-time ts)
  (string-join
   (map pad-time
        (list (hour ts) (minute ts) (second ts)))
   ":"))

(define (timestamp->civil ts)
  (string-append
   (timestamp->civil-date ts)
   " "
   (timestamp->civil-time ts)))

(define (timestring-relative-to-day appointment last-sec-in-day)
  (let ([pa (pseudo-appointment last-sec-in-day)])
    (cond
      [(and
        (< (fromtimestamp appointment) (fromtimestamp pa))
        (> (totimestamp appointment) (totimestamp pa))) "continued (all day)"]
      [(< (fromtimestamp appointment) (fromtimestamp pa))
       (string-append
        "continued, ends at "
        (timestamp->civil-time (totimestamp appointment)))]
      [(> (totimestamp appointment) (totimestamp pa))
       (string-append
        (timestamp->civil-time (fromtimestamp appointment))
        " continues to next day")]
      [else (format
             "from ~A to ~A"     
             (timestamp->civil-time (fromtimestamp appointment))
             (timestamp->civil-time (totimestamp appointment)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (present-calendar-html cal from-time to-time)
  (let ((c (calendar (title cal) (filter (lambda (x)
                                           (appointments-overlap? x (appointment "fake" from-time to-time)))
                                         (appointments cal)))))
  (write-to-file (format "~A.html" (title cal)) (html-document (title cal) (agenda c)))))

(define (agenda cal)
  (let* ([aps (appointments cal)]
         [cal-start (fromtimestamp (first aps))]
         [cal-end (totimestamp (first (sort aps ends-after?)))])
    (format
     "<h1><marquee>~A</marquee></h1>~n~A"
     (title cal)
     (string-join
      (filter (lambda (s) (string-contains? s "<li>"))
              (map
               (lambda (last-sec-in-day)
                 (format "<h4>~A</h4>~n<ul>~n~A</ul>"
                         (timestamp->civil-date last-sec-in-day)
                         (string-append*
                          (map
                           (lambda (a)
                             (format "<li><strong>~A</strong> ~A</li>~n"
                                     (title a)
                                     (timestring-relative-to-day a last-sec-in-day)))
                           (find-appointments cal
                                              (lambda (ap)
                                                (appointments-overlap? ap (pseudo-appointment last-sec-in-day))))))))
               (days-in-range cal-start cal-end)))
      "\n"))))

(define (html-document title [body-content null])
  (let ([html_string (format "<!doctype html>~n<title>~A</title>~n" title)])
    (if (null? body-content)
        html_string
        (string-append html_string (if (string? body-content)
                                       (format "<body>~n~A~n</body>" body-content)
                                       (format "<body>~n~A~n</body>" (string-append* body-content)))))))

(define (write-to-file filename content)
  (with-output-to-file filename (lambda () (display content)) #:exists 'replace))



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
"t"
(appointment? (appointment "yayaayayayay" 22 55))
(add-appointment cal1 (appointment "WOW" 23 24))

(find-first-appointment cal1 (lambda (a) (if (> (fromtimestamp a) 2) #t #f)))
(find-last-appointment cal1 (lambda (a) (if (> (fromtimestamp a) 20) #t #f)))

(calendars-overlap? cal1 cal2)
(present-calendar-html test-cal 1475280000 1477958399)
(write-to-file "test.html" (html-document (title test-cal) (agenda test-cal)))
(define oct-a (range-in-month (current-seconds)))
(define oct-b (range-in-month 1477958399))
(timestamp->civil-date (list-ref oct-a 0))
(timestamp->civil-date (list-ref oct-b 0))
(timestamp->civil-date (last oct-a))
(timestamp->civil-date (last oct-b))

"TESTING RANGE IN MONTH FUNC"
(define test-range (range 1475280000 1477958399 1337))
(define oct-days (days-in-range 1475280666 1477957666))
(define oct-ulti (map (lambda (x) (list x (range-in-month x))) test-range))
(map (lambda(a)
   (format "~A  ~A ~A  ~A  ~A"
           (timestamp->civil (first a))
           (days-in-month (month (first a)) (year (first a)))
           (length (last a))
           (map timestamp->civil (start-and-end-of-month (first a)))
           (map timestamp->civil (list (first (last a)) (last (last a)))))) (filter (lambda (x) (= 1477871999 (last (last x)))) oct-ulti))

"TESTING START AND END OF MONTH FUNC"
(define wat-ulti (map (lambda (x) (list x (start-and-end-of-month x))) (range 1475280000 1477958399 (* 30 60))))
(map
 (lambda(a)
   (format "~A    ~A"
           (timestamp->civil (first a))
           (map timestamp->civil (last a))))
 (filter
  (lambda (x)
    (let ((x (last x)))
      (not
       (and (and (>= (first x) 1475280000) (<= (first x) 1475366399))
            (and (>= (last x) 1477872000) (<= (last x) 1477958399))))))
  wat-ulti))

(calendar? (add-calendar cal1 (calendar "meem")))