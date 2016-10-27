#lang scheme
;'''''''''''''''''''''''''''''''''';
;    --- PP Mini Project I ---     ;
;                                  ;
; Name:    Mathias Sass Michno     ;
; StudyNo. 20130250                ;
; E-Mail:  mmichn13@student.aau.dk ;
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cal2 (list "sw7" (list (list "PP" 22 33) (list "CC" 66 77) (list "Pause" 69 1337) (list "Secret Calendar" (list (list "Secret PP" 11 22) (list "Secret CC" 13 14) (list "Secret Pause" 69 1337))) (list "Meme time" 21 22))))
(define app1 (list "some text" 213123123 1222223))

; Calendar constructor function. Can be called with or without content
(define (calendar title [content '()])
  (let ([cal (list title content)])
    (with-calendar cal
      (lambda (cal)
        cal))))

; Appointment constructor function. Constructs an appointment from a title, and a from and to timestamp
(define (appointment title from to)
  (let ((ap (list title from to)))
    (with-appointment ap
      (lambda (ap)
        ap))))

; "Getter" for the title of either a calendar or appointment 
(define (title x)
  (cond
    ((appointment? x) (car x))
    ((calendar? x) (car x))
    (else (error "Cannot get title from non cal or app."))))

; Helper function which takes a calendar and a function and applies the function to the calender
; if the given calender is a valid calendar
(define (with-calendar cal f)
  (if (calendar? cal)
      (f cal)
      ((error "Not a calendar"))))

; Helper function which takes an appointment and a function and applies the function to the appointment
; if the given appointment is a valid appointment
(define (with-appointment app f)
  (if (appointment? app)
      (f app)
      (error "Not an appointment")))

; "Getter" for content of a calendar
(define (content cal)
  (with-calendar
    cal
    (lambda (c)
      (last cal))))

; Predicate which checks if appointment a starts before appointment b
(define (starts-before? a b)
  (with-appointment a
    (lambda (a)
      (with-appointment b
        (lambda (b)
          (< (fromtimestamp a) (fromtimestamp b)))))))

; Predicate which checks if appointment a ends after 
(define (ends-after? a b)
  (with-appointment a
    (lambda (a)
      (with-appointment b
        (lambda (b)
          (> (totimestamp a) (totimestamp b)))))))

; "Getter" for all appointments and sub-calendars appointments of a calendar
(define (appointments cal)
  (with-calendar cal
    (lambda (cal)
      (let appointments_iter ([cals (list cal)] [app '()])
        (let* ((cont (apply append (map content cals)))
               (cals (filter calendar? cont)))
          (if (empty? cals)
              (sort (append app cont) starts-before?)
              (appointments_iter cals (append app (filter appointment? cont)))))))))

; "Getter" for the from timestamp of an appointment 
(define (fromtimestamp ap)
  (with-appointment ap
    (lambda (ap)
      (list-ref ap 1))))

; "Getter" for the to timestamp of an appointment
(define (totimestamp ap)
  (with-appointment ap
    (lambda (ap)
      (list-ref ap 2))))

; Predicate which matches an appointment
(define (appointment? x)
  (match x
    [(list (? string?) (? number? from) (? number? to)) (< from to)]
    [_ #f]))

; Predicate which matches a calendar
; Also, the calendar must be a valid calendar, i.e. its content must be valid calendars or appointments
(define (calendar? x)
  (match x
    [(list (? string?) (or (? empty?) (list (or (? calendar?) (? appointment?)) ... ))) #t]
    [_ #f]))

; Flattens a calendar, i.e. pulls every sub-calendar's appointment up to the root level
(define (flatten-calendar cal)
  (with-calendar cal
    (lambda (cal)
      (list (title cal) (appointments cal)))))

; Adds appointment to cal. Evaluates to a copy of cal with appointment added
(define (add-appointment cal appointment)
  (with-calendar
    cal
    (lambda (c)
      (with-appointment
        appointment
        (lambda (a)
          (calendar (title c) (append (content c) (list a))))))))

; Generalized removing from calendar cal.
; Takes a predicate pred, and optionally a type-pred (i.e. appointment? or calendar?),
; if no type-pred is given it anything matching the pred will be removed
; otherwise all content matching both will be removed,
; i.e. a copy of cal without said content will be the result of this function.
; The calender structure with all sub levels of calendars will be intact
(define (remove-from-calendar cal pred [type-pred (negate null?)])
  (with-calendar cal
    (lambda (cal)
      (let sub-cal-loop ([c cal])
        (let ([aps (filter (lambda (ap)
                             (and (appointment? ap)
                                  (if (type-pred ap) ((negate pred) ap) #t)))
                           (content c))]
              [cls (filter (lambda (cal)
                             (and (calendar? cal)
                                  (if (type-pred cal) ((negate pred) cal) #t)))
                           (content c))])
          (calendar
           (title c)
           (append
            aps
            (map sub-cal-loop cls))))))))

; Remove all appointments matching pred from calendar cal
(define (remove-appointments cal pred)
  (remove-from-calendar cal pred appointment?))

; Remove all calendars matching pred from calendar cal
(define (remove-calendars cal pred)
  (remove-from-calendar cal pred calendar?))

; Add cal-b to cal-a if both are valid calendars
; and output the new calendar
(define (add-calendar cal-a cal-b)
  (with-calendar cal-a
    (lambda (cal-a)
      (with-calendar cal-b
        (lambda (cal-b)
          (calendar (title cal-a) (append (content cal-a) (list cal-b))))))))

; Find all appointments on a calendar cal (and all sub calendars appointments)
; which matched a predicate pred
(define (find-appointments cal pred)
  (filter pred (appointments cal)))

; Find the first occurence in list which matched a predicate pred.
; evalueate to false of nothing is found
(define (find-first-helper list pred)  
  (if (null? list)
      #f
      (let ([a (car list)])
        (if (pred a)
            a
            (find-first-helper (cdr list) pred)))))

; Find earliest appointment in a calendar cal, which matches a predicate pred
(define (find-first-appointment cal pred)
  (with-calendar cal
    (lambda (cal)
      (find-first-helper (appointments cal) pred))))

; Find latest appointment in a calendar cal, which matches a predicate pred
(define (find-last-appointment cal pred)
  (with-calendar cal
    (lambda (cal)
      (find-first-helper (reverse (appointments cal)) pred))))

; Predicate which checks if two appointments overlap
(define (appointments-overlap? ap1 ap2)
  (with-appointment ap1
    (lambda (ap1)
      (with-appointment ap2
        (lambda (ap2)
          (and (< (fromtimestamp ap1) (totimestamp ap2))
               (< (fromtimestamp ap2) (totimestamp ap1))))))))

; Predicate which checks if two calendars overlap
; i.e. if any appointment in cal1 overlaps with any appointment in cal2
(define (calendars-overlap? cal1 cal2)
  (with-calendar cal1
    (lambda (cal1)
      (with-calendar cal2
        (lambda (cal2)
          (let* ((aps1 (appointments cal1))
                 (aps2 (appointments cal2)))
            (ormap
             (lambda (a) (ormap
                          (lambda (b) (appointments-overlap? a b))
                          aps2))
             aps1)))))))

;;HELPER FUNCTIONS

; Evalueates to a list containing a range from a to b with step
; Can be applied to 1, 2 or 3 arguments:
;   if 1 argument  - the range will be from 0 to the given number with 1 as step
;   if 2 arguments - the range will be from 1st arg to 2nd arg with 1 as step
;   if 3 arguments - the range will be from 1st arg to 2nd arg with 3rd arg as step
(define (range a [b null] [step 1])
  (if (null? b)
      (range 0 a step)
      (let range_iter ([f a] [res-list (list a)])
        (if (> (+ f step) b)
            res-list
            (range_iter (+ f step) (append res-list (list (+ f step))))))))

; Evalueates to a list of timestamps representing days from from-time to to-time
; All timestamps will be the last second of the day i.e. <a given date> 23:59:59
(define (days-in-range from-time to-time)
  (range
   (+ (- from-time (remainder from-time SECS_IN_A_DAY)) (sub1 SECS_IN_A_DAY))
   (sub1 (+ to-time (- SECS_IN_A_DAY (remainder to-time SECS_IN_A_DAY))))
   SECS_IN_A_DAY))

; Outputs a list containing two timestamps
; the first timestamp being within the first day
; and the second timestamp within the last day of the month relative to ts
(define (start-and-end-of-month ts)
  (let* ([d (day-in-month ts)]
         [month-length (days-in-month (month ts) (year ts))]
         [month-start (if (= d 1)
                          ts
                          (- ts (* (sub1 d) SECS_IN_A_DAY)))]
         [month-end   (if (= d month-length)
                          ts
                          (+ ts (* (- month-length d) SECS_IN_A_DAY)))])
    (list month-start month-end)))

; Evalueates to a list of timestamps representing all days in the month of which ts is within
(define (range-in-month ts)
  (match (start-and-end-of-month ts)
    [(list start end) (days-in-range start end)]))

; Outputs an appointment beginning 23 hours 59 minutes and 29 seconds before end-time
; and ending at end-time. (The appointment will span a day)
(define (pseudo-appointment end-time)
  (appointment "pseudo" (- end-time (sub1 SECS_IN_A_DAY)) end-time))

; Pad a given string to width with char
; Will never be removed
(define (leftpad-string str width char)
  (format "~A~A"
          (make-string
           (- width (string-length str))
           char)
          str))

;;TIMESTAMPS

; Number of seconds in a day
(define SECS_IN_A_DAY 86400)

; Calculate which seconds a given timestamp ts is in the day
(define (second-in-day ts)
  (modulo ts SECS_IN_A_DAY))

; Calculate the second of a timestamp ts
(define (second ts)
  (modulo (second-in-day ts) 60))

; Calculate the minute of a timestamp ts
(define (minute ts)
  (modulo (quotient (second-in-day ts) 60) 60))

; Calculate the hour of a timestamp ts
(define (hour ts)
  (modulo (quotient (second-in-day ts) 3600) 24))

; Calculate which day since january 1st 1940 ts represents
(define (day-in-forever ts) ;forever is a timeperiod from january 1st 1970 to the end of all time
  (quotient ts SECS_IN_A_DAY))

; Calculate which day a timestamp ts represents in the week
(define (day-in-week ts)
  (modulo (+ (day-in-forever ts) 3) 7))

; Output the name of a given weekday relative to the timestamp ts
(define (name-of-day ts)
  (list-ref '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") (day-in-week ts)))

;
(define (days-in-month month year)
  (let ((month (sub1 month)))
    (cond
      [(leap-year? year) (list-ref '(31 29 31 30 31 30 31 31 30 31 30 31) month)]
      [else (list-ref '(31 28 31 30 31 30 31 31 30 31 30 31) month)])))

(define (month-day-helper day month year)
  (let ((month-length (days-in-month month year)))
    (cond
      [(> day month-length) (month-day-helper (- day month-length) (add1 month) year)]
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
      [(>= day year-length) (day-year-helper (- day year-length) (add1 year))]
      [else (cons (add1 day) year)])))

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
      [(and (< (fromtimestamp appointment) (fromtimestamp pa))
            (> (totimestamp appointment) (totimestamp pa)))
       "continued (all day)"]
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
(flatten-calendar test-cal)
(remove-appointments (add-calendar test-cal cal1) (lambda (a) (string-contains? (title a) "ay")))
(remove-from-calendar (add-calendar test-cal cal1) appointment? (lambda (a) (string-contains? (title a) "ay")))