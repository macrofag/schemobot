
;------------------------------------------------------------------------------
(define string-null? (lambda (s) (equal? s "")))
(define tt (lambda (l) (map (lambda (p) (list (car p)(cdr p))) l)))
(define (make-xml-reader xml-port ns)
  (define (init-parser c)
    (let ((ret c))
      ((ssax:make-parser
        DOCTYPE
        (lambda (port docname systemid internal-subset? seed)
               (printf "DOCTYPE:~%~a~%~a~%~a~%~a~%~a~%"
                       port docname systemid internal-subset? seed)
               (values #f '() '() seed))
        DECL-ROOT
        (lambda (e s)
          (printf "DECL-ROOT: ~a~%~a~%" e s)
          s)
        NEW-LEVEL-SEED
        (lambda (e a n ec s)
          (set! ret
                (call/cc (lambda (itc)
                           (ret itc (list 'new-level 
                                          ;(list e (list '@ a))
                                          (let ((ee 
                                                 (if (pair? e)
                                                     (string->symbol
                                                      (string-append
                                                       (symbol->string (car e))
                                                       ":"
                                                       (symbol->string (cdr e))))
                                                     e)
                                                    ))
                                            (if (null? a)
                                                (list ee)
                                                (list ee (cons '@ a)))
                                            )
                                          )))))
          
          '()
          )
        CHAR-DATA-HANDLER
        (lambda (s1 s2 s)
          'char-data-handler
          (when (not (string-null? s1))
            (set! ret
                  (call/cc (lambda (itc)
                             (ret itc (list 'char-data s1))))))
          (cons (if (string-null? s2) s1 (cons s2 s1)) s)
          
          )
        FINISH-ELEMENT
        (lambda (e a n p s)
          (set! ret
                (call/cc (lambda (itc)
                           (ret itc (list 'finish-level e)))))
          (cons e 
                (if (null? a) 
                    (reverse s) 
                    (cons (cons '@ (tt a)) 
                          (reverse s))))
          
          )
        ) xml-port ns)
      (ret '() '())))
  (let ((con init-parser))
    (lambda () ;reader body
      (if (null? con)
          '()
          (let-values 
              (((c date) (call/cc con)))
            (set! con c)
            date)))))
          
;(define reader
;  (let-values ( ( (inp outp) (tcp-accept 
;                              listener)))
;    (make-xml-reader inp)))

(define (skip-to-newlevel raeder)
  (let loop ((it (reader)))
    (if (eq? (car it) 'new-level)
        (cadr it)
        (loop (reader)))))

(define (tfr r h)
  (tag-full-reader r (list '() h)))
(define (tag-full-reader reader head)
 ; (printf "tag : ~a ~%" head)
  (let ((it (reader)))
  ;  (printf "tag : ~a ~%head: ~a ~%" it head)
    (cond
      ((eq? (car it) 'new-level)
       (tag-full-reader reader 
                        (cons (tag-full-reader reader 
                                               (reverse (cadr it))
                                               )
                              head
                              ))
       )
      ((eq? (car it) 'char-data)
       (tag-full-reader reader (cons (cadr it) head))
       )
      ((eq? (car it) 'finish-level)
       (reverse head)
       )
      )))
(define (skip-level n reader)
  (if (> n 0)
      (let ((it (reader)))
        (display n)(newline)
        
        (skip-level (cond 
                      ((eq? (car it) 'finish-level)
                       (- n 1))
                      ((eq? (car it) 'new-level)
                       (+ n 1))
                      (else n))
                    reader))
      'done))
(define (read-tag-skip reader)
  (let ((s (skip-to-newlevel reader)))
    (tag-full-reader reader (reverse s))))
;------------------------------------------------------------------------------
