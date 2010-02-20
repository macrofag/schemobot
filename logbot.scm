(require scheme/enter)
(enter! "sxmpp_mod.scm")
(load "stream_read_xml.scm")
(load "jpass.scm")
(require scheme/date)
(require scheme/base)

(define init-comline
  (lambda (jid pass)
    (call/cc 
     (lambda (outto)
       (let ((exit outto))
         (with-xmpp-ports 
          jid pass (in out)
          (let loop ((doit (lambda (x) '()))
                     (to outto))
            (if (eq? doit 'exit)
                (set! exit to)
                (call-with-values
                 (lambda () (call/cc (lambda (cc) (to cc (doit in)))))
                 loop))))
         (exit))))))

(define (make-exec jid pass)
  (let-values (((itt some) (init-comline jid pass)))
    (lambda (do)
      (if (not (continuation? itt))
          'exited
          (let-values (((cc res) (call/cc (lambda (cc) (itt do cc)))))
            (set! itt cc)
            res)))))
            
(define com-test
  (let ((n 0))
    (lambda ()
      (call/cc
       (lambda (outto)
         (let ((exit outto))
           (let loop ((doit (lambda () '()))
                      (to outto))
             (printf "~a call~%" n)
             (set! n (+ 1 n))
             (if (eq? doit 'exit)
                 (exit 'trololo)
                   (call-with-values
                    (lambda () (call/cc (lambda (cc) (to cc (doit)))))
                    loop)))
           (exit)))))))
(define (make-test-com)
  (let-values (((itt some) (com-test)))
    (printf "~a~%" some)
    (lambda (do)
      (if (not (continuation? itt))
          'exited
          (let-values (((cc res)
                        (call/cc (lambda (cc) (itt do cc)))))
            (set! itt cc)
            res)))))

(define-syntax half-apply
  (syntax-rules ()
    ((_ name vals ...)
     (let ((vl (list vals ...)))
       (lambda a (apply name (append a vl)))))
    ))

(define exec (make-exec jid pass))
(define (get-conf-presence conf chatname) 
  (ssxml 
   `(presence 
     (@ (from ,jid) 
        (to ,(string-append (cut-resource conf) "/" chatname))))))
;(exec (lambda (in) (send (message "cf-ins@jabber.ru" "trololo"))))
;(exec (lambda (it) (send conf-presence)))
;(define res (exec (lambda (in) (raw-xmpp-response (read-async in)))))             
;(define itt (exec (lambda (in) in)))

(define (resp in)
  (let ((str (read-async in)))
    (printf "received: ~a~%parsed as ~a~%" str (raw-xmpp-response str))
    (void)))
          
(define (chat-message to do)
  (ssxml `(message (@ (from ,jid) (to ,to) (type "groupchat")) (x (status (@ (code "100")))) (body ,do))))

(define (trololo)
  (exec (lambda (in) (send (chat-message (cut-resource jid) chatjid
                                         "trololo")))))
;(trololo)
(define (send2jid j mes) 
  (exec (lambda (in) (send (ssxml
                            `(message (@ (from ,jid) (to ,j))
                                      (body ,mes)))))))
(define in
  '())
(exec (lambda (i) (set! in i)))
(define reader
  (make-xml-reader in 
                   '((jc . "jabber:client")
                     (stream ."http://etherx.jabber.org/streams"))
                   ))

(reader);note!: при зависаниях записи прочитать станзу
(define ns1 '((jc . "jabber:client")))
(define (try-read-message reader)
  (let ((bt ((sxpath "//jc:body/text()" ns1)
             (read-tag-skip reader))
            ))
    (when (not (null? bt))
      (for-each display bt)
      (newline)
      (void))))

(define (readloop-all)
  (let ((stz (read-tag-skip reader)))
    (printf "raw:--------------------------~% ~a~%-------------------------------~%" stz)
    (when (and (eq? 'jabber:client:message (car stz))
               (equal? (cdr (assq 'type (cdadr stz))) "groupchat"))
      (printf "~a:~a~%" 
              (cdr (assq 'from (cdadr stz)))
              ((sxpath "//jc:body/text()" ns1) stz)))
    )
  ;endless loop
  (readloop-all))

(define (sxp-get-body-text stz)
  ((sxpath "//jc:body/text()" ns1) stz))


(date-display-format  'rfc2822)
(define-syntax forever
  (syntax-rules ()
    ((_ body ...)
     (letrec ((loop (lambda () body ... (loop)))) (loop)))))
(define confs (vector->list (current-command-line-arguments)))
;(define confs (list "scheme@conference.jabber.ru"))
(define files (make-hash))
(for-each (lambda (name) 
            (hash-set! files name 
                       (open-output-file 
                        (string-append "log/" (cut-resource name))
                        #:exists 'append)))
          confs)
(define (log2file chatname name body)
  (let ((port (hash-ref files chatname (current-output-port))))
    (display (format "[~a]~a -> ~a~%"
                     (date->string (seconds->date (current-seconds)) #t) 
                     name body) port)
    (flush-output port)))
(define (start-log)
  (exec (lambda (in)
          (for-each send 
                    (map (half-apply get-conf-presence "trololog") confs))))
  (forever
   (let ((stz (read-tag-skip reader)))
     (when (and (eq? 'jabber:client:message (car stz)))
       (let ((ct (cdr (assq 'type (cdadr stz))))
             (from (cdr (assq 'from (cdadr stz))))
             (body ((sxpath "//jc:body/text()" ns1) stz)))
         (printf "~a:~a~%" 
                (cdr (assq 'from (cdadr stz)))
                body)
        (when (equal? ct "groupchat")
          (log2file (cut-resource from) (jid-resource from)
                    body))
         )))
   ))

(start-log)
(define (ntloop-privat)
  (let ((stz (read-tag-skip reader)))
    (when (and (eq? 'jabber:client:message (car stz)))
      (let ((ct (cdr (assq 'type (cdadr stz))))
            (from (cdr (assq 'from (cdadr stz)))))
        (printf "~a:~a~%" 
                (cdr (assq 'from (cdadr stz)))
                ((sxpath "//jc:body/text()" ns1) stz))
        (when (or (equal? ct "chat")
                  (and (equal? ct "groupchat")
                       (regexp-match? (regexp "trololobot") (car (sxp-get-body-text stz)))))
          (exec (lambda (in)
                  (send (ssxml 
                         `(message (@ (to ,(if (equal? ct "groupchat")
                                                 (cut-resource from)
                                                 from)) 
                                      (from ,(cut-resource jid)) (type ,ct))
                                   (x (status (@ (code "100")))) 
                                   (body 
                                    ,(string-append (jid-resource from)": нет ты")))))
                                           
                        )))))
    )
  ;endless loop
  (ntloop-privat))


;<?xml version='1.0'?>
;<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='3322720432' from='jabber.ru' xml:lang='ru'>
;<iq from='jabber.ru' id='session' type='error'>
; <error code='503' type='cancel'>
;  <service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
; </error>
;</iq>
;<iq type='result' id='auth'/>
;<presence from='cf-ins@jabber.ru/tkabber' to='blahhh@jabber.ru/testbot' xml:lang='ru-RU'>
; <status>был бы девкой - был бы моэ. а так просто дебил (с)перто</status>
; <priority>8</priority>
;</presence>
;<presence from='blahhh@jabber.ru/testbot' to='blahhh@jabber.ru/testbot'/>
                        


