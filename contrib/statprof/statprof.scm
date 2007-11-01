;; statprof.scm -- A statistical profiler for Gambit-C 4.0

;; See the README file for license and usage information.

;; $Id: statprof.scm,v 1.9 2005/03/14 07:35:49 guillaume Exp $


;; ----------------------------------------------------------------------------
;; Profiling & interruption handling

;; Buckets should probably be hashtables for better performance
(define *buckets* '())
(define *total* 0)
(define *unknown* 0)

(define (profile-start!)
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

;; As an improvement, we could use ##continuation-parent and ##object->global-var->identifier
;; to identify more precisely where the code was in the ##continuation-next ... continuation
(define (identify-continuation cont)
  
  (define (continuation-location cont)
    (let ((locat (##continuation-locat cont)))
      (if locat
          (let ((file (##container->file (##locat-container locat))))
            (if file
                (let* ((filepos (##position->filepos (##locat-position locat)))
                       (line (##filepos-line filepos))
                       (col (##filepos-col filepos)))
                  (list file line col))
              #f))
        #f)))
  
  (or (continuation-location cont)
      (let ((next (##continuation-next cont)))
        (if (##not next)
            #f
          (identify-continuation next)))))

(define (profile-heartbeat!)
  (##continuation-capture
    (lambda (cont)
      (##thread-heartbeat!)
      (let ((id (identify-continuation cont)))
        (if (##not id)
            (set! *unknown* (##fx+ *unknown* 1))
          (let ((bucket (assoc (##car id) *buckets*)))
            (set! *total* (##fx+ *total* 1))
            (if (##not bucket)
                (begin
                  (set! *buckets* (##cons
                                    (##cons (##car id)
                                          ;; fixme: arbitrary hard limit
                                          ;; on the length of source
                                          ;; files
                                          (##make-vector 50000 0))
                                    *buckets*))
                  (set! bucket (##car *buckets*))))
            (vector-set! (##cdr bucket)
                         (##cadr id)
                         (##fx+ (vector-ref (##cdr bucket)
                                            (##cadr id))
                                1))))))))


;; ----------------------------------------------------------------------------
;; Function to generate an sexp report

(define (write-sexp-profile-report profile-name)
  (call-with-output-file profile-name
    (lambda (port)
      (pp
        (cons *total*
              (cons *unknown*
                    (map (lambda (bucket)
                           (let ((file (car bucket))
                                 (data (cdr bucket)))
                             (cons file
                                   (let iter ((n (- (vector-length data) 1))
                                              (lines '()))
                                     (if (>= n 0)
                                         (let ((count (vector-ref data n)))
                                           (if (= count 0)
                                               (iter (- n 1) lines)
                                             (iter (- n 1) (cons (list n count) lines))))
                                       lines)))))
                         *buckets*)))
        port))))


;; ----------------------------------------------------------------------------
;; Functions to generate an HTML report

(define (write-html-profile-report profile-name)

  (define (iota1 n)
    (let loop ((n n)
               (l '()))
      (if (>= n 1)
          (loop (- n 1) (cons n l))
          l)))
  
  (define (maximum v)
    (let ((max -1)
          (len (##vector-length v)))
      (let loop ((n 0))
        (if (##fixnum.= n len)
            max
          (begin
            (let ((elem (##vector-ref v n)))
              (if (##fixnum.> elem max)
                  (set! max elem)))
            (loop (##fixnum.+ n 1)))))))
  
  (define directory-name (string-append (current-directory)
                                        profile-name
                                        "/"))
  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f)
   (lambda ()
     (create-directory (list path: directory-name
                             permissions: #o755))))
  
  (let ((max-intensity
         (apply max
                (map maximum (map cdr *buckets*)))))

    (map
     (lambda (bucket)
       (let ((file (car bucket))
             (data (cdr bucket)))
       
         (define (get-color n)
           (let ((i (vector-ref data n)))
             (if (= i 0)
                 (as-rgb (vector-ref palette 0))
                 (let ((x (* (/ (log (+ 1. i))
                                (ceiling (log max-intensity)))
                             (- (vector-length palette) 1))))
                   (as-rgb (vector-ref palette
                                       (inexact->exact (ceiling x))))))))

         (with-output-to-file (string-append
                               directory-name
                               (path-strip-directory file)
                               ".html")
           (let ((lines (call-with-input-file file
                          (lambda (p) (read-all p read-line)))))
             (lambda ()
               (display
                (sexp->html
                 `(html
                   (body
                    (table
                     cellspacing: 0
                     cellpadding: 0
                     border: 0
                     style: "font-size: 12px;"
                     ,@(map
                        (lambda (line line#)
                          `(tr
                            (td ,(string-append
                                  (number->string line#)
                                  ": "))
                            ;; (td
                            ;;  align: center
                            ;;  ,(let ((n (vector-ref data line#)))
                            ;;     (if (= n 0)
                            ;;         ""
                            ;;         (string-append "["
                            ;;                        (number->string n)
                            ;;                        "/"
                            ;;                        (number->string *total*)
                            ;;                        "]"))))
                            
                            (td
                             align: center
                             ,(let ((n (vector-ref data line#)))
                                (if (= n 0)
                                    ""
                                    (string-append
                                     (number->string
                                      (round% (/ n *total*)))
                                     "%%% "))))
                               
                            (td (pre style: ,(string-append
                                              "background-color:#"
                                              (get-color line#))
                                     ,line))))
                        lines
                        (iota1 (length lines)))))))))))))
     
     *buckets*))

  (with-output-to-file (string-append directory-name "index.html")
    (lambda ()
      (display
       (sexp->html
        `(html
          (body
            (p "total = " ,*total*)
            (p "unknown = " ,*unknown*)
           ,@(map (lambda (info)
                    (let ((file-path (string-append
                                      directory-name
                                      (path-strip-directory (car info))
                                      ".html")))
                      `(p (a href: ,file-path ,file-path)
                          " ["
                          ,(cdr info)
                          " %%%]")))
                  (sort > (map (lambda (bucket)
                                 (cons (car bucket)
                                       (round% (/ (vector-sum (cdr bucket))
                                                  *total*))))
                               *buckets*)
                    cdr)))))))))

(define (round% n)
  (/ (round
      (* 10000 n))
     100.))

(define (vector-sum v)
  (let ((sum 0)
        (len (##vector-length v)))
    (let loop ((n 0))
      (if (##fixnum.= n len)
          sum
        (begin
          (set! sum (##fixnum.+ sum (##vector-ref v n)))
          (loop (##fixnum.+ n 1)))))))


;; ----------------------------------------------------------------------------
;; Text formatting

(define (pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))
    
    (let loop ((i 0)
               (acc '()))
      (if (= i step)
          (reverse acc)
          (loop (+ i 1)
                (cons (map
                       (lambda (x o)
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (pad-left (number->string x 16) 2 #\0))
          col)))

(define palette
  (list->vector
   (cons '(255 255 255)
         (gradient '(127 127 255)
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Sort

(define (sort test seq key)
  (define (sort-list l smaller key)
    (define (merge-sort l)
      (define (merge l1 l2)
        (cond ((null? l1) l2)
              ((null? l2) l1)
              (else
               (let ((e1 (car l1)) (e2 (car l2)))
                 (if (smaller (key e1) (key e2))
                     (cons e1 (merge (cdr l1) l2))
                   (cons e2 (merge l1 (cdr l2))))))))
      
      (define (split l)
        (if (or (null? l) (null? (cdr l)))
            l
          (cons (car l) (split (cddr l)))))
      
      (if (or (null? l) (null? (cdr l)))
          l
        (let* ((l1 (merge-sort (split l)))
               (l2 (merge-sort (split (cdr l)))))
          (merge l1 l2))))
    
    (merge-sort l))
  
  (sort-list seq test key))


;; ----------------------------------------------------------------------------
;; Included file "html.scm"
;; ----------------------------------------------------------------------------

;; html.scm -- A simple html generator for Gambit-C 4.0

;; Written by Guillaume Germain (germaing@iro.umontreal.ca)
;; This code is released in the public domain.


(define (stringify x)
  (with-output-to-string ""
    (lambda ()
      (display x))))

(define (to-escaped-string x)
  (stringify
   (map (lambda (c)
          (case c
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            ((#\&) "&amp;")
            (else c)))
        (string->list
         (stringify x)))))

;; Quick and dirty conversion of s-expressions to html
(define (sexp->html exp)
  
  ;; write the opening tag
  (define (open-tag exp)
    (cond
     ;; null tag isn't valid
     ((null? exp)
      (error "null tag"))
     
     ;; a tag must be a list beginning with a symbol
     ((and (pair? exp)
           (symbol? (car exp)))
      (list "<"
            (car exp)
            " "
            (maybe-args (car exp) (cdr exp))))
     
     (else
      (error "invalid tag" exp))))

  ;; take care of the keywords / arguments
  (define (maybe-args tag exp)

    (cond
     ;; does the rest of the list begins with a keyword
     ((and (pair? exp)
           (keyword? (car exp)))

      ;; does the keyword has an associated value?
      (if (or (null? (cdr exp))
              (keyword? (cadr exp)))
          ;; no, we don't put an associated value
          (list (keyword->string (car exp))
                " "
                (maybe-args tag (cdr exp)))
          ;; yes, we take the next element in the list as the value
          (list (keyword->string (car exp))
                "=\""
                (cadr exp)
                "\" "
                (maybe-args tag (cddr exp)))))

     ;; must just be some content
     (else
      (content tag exp))))

  ;; handle the content of the tag and closing it
  (define (content tag exp)
    (cond
     ;; no content...
     ((null? exp)
      ;;(list "></" tag ">"))           ; close as "<br></br>"
      (list "/>"))                      ; close as "<br/>"

     ;; write the content, handle tags inside
     ((pair? exp)
      (list ">"
            (map (lambda (e)
                   (if (pair? e)
                       (open-tag e)
                       (to-escaped-string e)))
                 exp)
            "</"
            tag
            ">"))

     ;; non-null terminated list?
     (else
      (error "strange content..."))))

  ;; we rely on Gambit's flattening of list when printed with DISPLAY
  (with-output-to-string ""
                         (lambda ()
                           (display (open-tag exp)))))
