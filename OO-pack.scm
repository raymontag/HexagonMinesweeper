(module OO-pack scheme
 (provide create-instance instance?
          Root send send-msg send/safe send-msg/safe class if if-true if-false)
 
 ;; Dieses Teachpack stellt eine Umgebung zur Verfuegung, in der eine
 ;; angenaeherte objektorientierte Programmierung ermoeglicht wird.
 ;; Es ist eine Modifikation eines entsprechenden Pakets, das am 
 ;; MIT im Fruehjahr 2006 im Kurs 6.001 fuer eine grosse 
 ;; Programmieraufgabe benutzt wurde.
 ;;   
 ;; Begriffe und Terminologie:
 ;;
 ;; Objekte sind Instanzen einer Klasse. Jedes individuelles Objekt
 ;; hat seine eigene Identitaet. Jede Instanz kennt ihre Klasse und
 ;; die Klasse hat eine Liste von Methoden, die jedes Objekt der
 ;; Klasse "versteht". Man kann einem Objekt eine Nachricht senden,
 ;; auf die das Objekt reagiert. Jede Nachricht besteht 
 ;; aus dem Namen einer Methode und eventuell weiteren Argumenten
 ;; Das Objekt reicht diese Nachricht an seine Klasse weiter, die
 ;; dann in der eigenen Methodenliste oder aber in einer Methodenliste
 ;; einer Oberklasse nach dem Namen sucht und die entsprechende 
 ;; Prozedur mit den mitgelieferten Argumenten ausfuehrt.
 ;;
 ;;
 ;; Klassen beschreiben gleicharte Objekte. Diese Objekte haben die
 ;; gleichen (internen) Variablen (Instanzenvariablen, Zustand des
 ;; Objekts) und reagieren auf die gleiche Nachrichten.
 ;; Jede Klasse erbt Eigenschaften von seiner Oberklasse.
 ;; Die vererbten Eigenschaften sind Instanzenvariable und Methoden,
 ;; dabei koennen beide durch Neudefinition ueberschrieben werden.
 ;;
 ;; Jede Klasse wird in Scheme durch eine Prozedur (den "class-maker") erzeugt,
 ;; die als ersten Parameter "self" haben muss. Die Parameter dieser Prozedur
 ;; und die lokalen Variablen dieser Prozedur bilden zusammen mit denen der
 ;; Oberklasse die Instanzenvariablen eines Objektes der Klasse.
 ;; Hier definierte lokale Prozeduren sind dann nur von den Objekten der Klasse
 ;; aus benutzbar. 
 ;; Anschliessend wird mit Hilfe der Prozedur "make-methods" aus einer Liste
 ;; von Namen und Prozeduren eine Methodenliste fuer diese Klasse erzeugt.
 ;; Die Methodenliste wird zusammen mit dem Namen der Klasse und der 
 ;; Oberklasse mit der Prozedur "make-class" zu einer Klasse verbunden. 
 ;; 
 ;; Die Wurzel der Klassenhierarchie ist "root-class". Jede andere Klasse
 ;; sollte direkte oder indirekte Unterklasse dieser Klasse sein. 

 ;;------------------------------------------------------------
 ;; Einzelne Objekte einer Klasse werden auch als Instanzen der
 ;; Klasse bezeichnet. Instanzen werden hier als mit einem Etikett
 ;; (tag) 'instance versehene Datenstrukturen implementiert, die das Objekt
 ;; darstellen. Wird einem Objekt eine Nachricht geschickt, so wird
 ;; diese an die Klasse weitergeschickt, die zweite Komponente der
 ;; Datenstruktur ist. Ist in einer Klasse die Methode 'INIT definiert,
 ;; so wird diese bei Erzeugung des Objekts automatisch ausgefuehrt
 
       

 (define (make-instance)
   (mcons 'instance (mcons #f '())))
 
 (define (instance? x)
   (and (mpair? x) (eq? (mcar x) 'instance)))
       
 
 (define (instance-class instance) (mcar (mcdr instance)))
 
 (define (set-instance-class! instance class)
   (set-mcar! (mcdr instance) class))
  
; (define-syntax (create-instance stx)
;   (syntax-case stx ()
;     ((src class-maker . args)
;      (syntax/loc stx
;        (let* ((instance (make-instance))
;               (class (class-maker instance)))
;          (set-instance-class! instance class)
;          (send instance init . args)
;          instance)))))
;        
        
 
 (define (create-instance class-maker . args)
   (let* ((instance (make-instance))
          (class (class-maker instance)))
     (set-instance-class! instance class)
     (let ((init (get-method 'init instance)))
       (if (method? init) 
           (with-handlers ((exn:fail:contract:arity?
                            (lambda (exn)                              
                              (raise (make-exn (string-append (format "init class ~s: " (car (send-msg instance 'type))) 
                                                              (exn-message exn)) (exn-continuation-marks exn))))))
             (apply init args)) #f))
     instance))
 
 
  
 ;;------------------------------------------------------------
 ;; Klassen werden in Scheme als Dispatch-Prozeduren dargestellt, die
 ;; von "make-class" erzeugt werden. Eine Klasse reagiert auf jeden 
 ;; Fall auf die Nachrichten TYPE und METHODS, die hier implementiert werden.
 
 (define (make-class classname methods super-class)
   (cond ((not (symbol? classname))  
          (error "bad classname" classname))
         ((not (method-list? methods))
          (error "bad method list" methods))
         ((not (instance? super-class))
          (error "bad super-class" super-class))
         (else
          (lambda (message)
            (cond 
              ((eq? message 'type)
               (lambda () (cons classname (send-msg super-class 'type))))
              ((eq? message 'methods)
               (lambda ()
                 (remove-duplicates (append (reverse (method-names methods))
                                            (send-msg super-class 'methods))
                                    )))
              (else
               (let ((entry (method-lookup message methods)))
                 (if entry
                     (cadr entry)
                     (get-method message super-class))))))
          )))
 
 (define (class? x)
   (procedure? x))
 
 (define (->class x)
   (cond ((instance? x)
          (instance-class x))
         ((class? x)
          x)
         (else
          (raise-user-error "I don't know how to make a class from" x))))
 
 ;;------------------------------------------------------------
 ;; make-methods erzeugt eine Liste von Paaren (name,proc), die als
 ;; Eingabe fuer make-class geeignet ist. Die Liste bekommt zur 
 ;; Unterscheidung ein Etikett (tag) 'methods.
 
 
 (define (make-methods . args)
   (define (helper lst result)
     (cond ((null? lst) result)
           
           ; error catching
           ((null? (cdr lst))
            (error "unmatched method (name,proc) pair"))
           ((not (symbol? (car lst)))
            ;	   (if (procedure? (car lst))
            ;	       (pp (car lst)))
            (error "invalid method name" (car lst)))
           ((not (procedure? (cadr lst)))
            (error "invalid method procedure" (cadr lst)))
           
           (else
            (helper (cddr lst) (cons (list (car lst) (cadr lst)) result)))))
   (cons 'methods (helper args '())))
 
 (define (method-list? methods)
   (and (pair? methods) (eq? (car methods) 'methods)))
  
 (define (method-lookup message methods)
   (assq message (cdr methods)))
 
 (define (method-names methods)
   (map car (cdr methods)))
 
 

 ;;------------------------------------------------------------
 ;; Die Wurzelklasse enthaelt die IS-A Methode. 
 ;; Jede Klasse muss direkt oder indirekt Unterklasse dieser
 ;; Klasse sein.
 
 (define (Root self)
   (lambda (message)
     (cond 
       ((eq? message 'init) (lambda () 'ok))
       ((eq? message 'inspect) (lambda () 'ok))
       ((eq? message 'type) (lambda () (list 'Root)))
       ((eq? message 'show) (lambda () (list 'instance (car (send-msg self 'type)))))
       ((eq? message 'methods) (lambda () (list 'inspect 'init 'is-a 'type 'show 'methods)))
       ((eq? message 'is-a)
        (lambda (type)
          (list? (memq type (send-msg self 'type)))))
       (else
        'NO-METHOD
        ))))
 
 ;;------------------------------------------------------------
 ;; Objekten werden Nachrichten geschickt. Jede Nachricht besteht 
 ;; aus dem Namen einer Methode und eventuell weiteren Argumenten
 ;; Das Objekt reicht diese Nachricht an seine Klasse weiter, die
 ;; dann in der eigenen Methodenliste oder aber in einer Methodenliste
 ;; einer Oberklasse nach dem Namen sucht und die entsprechende 
 ;; Prozedur mit den mitgelieferten Argumenten ausfuehrt. 

  (define (send-msg object message . args)
    (if (instance? object)
        (let ((method (with-handlers ((exn:fail:user?
                                       (lambda (exn) 
                                         (error 'send-msg (format "expects a class object with method ~s: ~s" message object)))))
                        (get-method message object))))
          (cond ((method? method)
                 (apply method args))
                (else
                 (error 'send-msg "expects class object  with method ~s: class ~s" message (car (send-msg object 'type))))))
        (error 'send-msg (format "expects a class object with method ~s: ~s" message object))))
  
  (define (send-msg/safe default object message . args)
    (if (instance? object)
        (let ((method (with-handlers ((exn:fail:user?
                                       (lambda (exn) 
                                         (error 'send-msg/safe (format "expects a class object with method ~s: ~s" message object)))))
                        (get-method message object))))
          (cond ((method? method)
                   (apply method args))
                (else default)))
        (error 'send-msg (format "expects a class object with method ~s: ~s" message object))))
    
  (define (get-method message object)
    ((->class object) message))
 
  (define (method? x)
    (procedure? x))
 

 ;;;------------------------------------------------------------
 ;;; Utility procedures
 ;
 ;
  
; (define (filter predicate lst)
;   (cond ((null? lst) '())
;         ((predicate (car lst))
;          (cons (car lst) (filter predicate (cdr lst))))
;         (else (filter predicate (cdr lst)))))
; 
 (define (remove-duplicates lst)
   (if (null? lst)
       '()
       (cons (car lst)
             (remove-duplicates (filter (lambda (x) 
                                          (not (eq? x (car lst))))
                                        lst)))))
  
  (define-syntax (if stx)
    (syntax-case stx (quote)
      ((src-if bed true) (syntax (cond (bed true))))
      ((src-if bed true . false) (syntax (cond (bed true) (else . false))))))
        
  (define-syntax (if-true stx)
    (syntax-case stx (quote)
      ((src-if bed . true) (syntax (cond (bed . true))))))

  (define-syntax (if-false stx)
    (syntax-case stx (quote)
      ((src-if bed . true) (syntax (cond ((not bed) . true))))))

  (define-syntax (method-expand stx)
    (syntax-case stx ()
      ((_ result ()) (syntax (make-methods . result)))
      ((_ result (name . rest))
       (syntax
        (method-expand ('name name . result) rest)))))
  
  (define-syntax (let-expand stx)
    (syntax-case stx ()
      ((_ last result () . body) (syntax (letrec (last . result) . body)))
      ((_ last result (first . rest) . body) (syntax (let-expand last (first . result) rest . body)))))
    
  (define-syntax (send stx)
    (syntax-case stx (quote)
      ((src-send object (quote message) . arguments) 
       (syntax/loc stx (src-send object message . arguments)))
      ((src-send src-object message . arguments)
       (cond ((identifier? (syntax message))
              (quasisyntax/loc stx
                (let ((object src-object))
;                  (display "send ") (display 'message) (display " to class ") (display object) (newline)
                  (if (instance? object)
                      (let ((method (get-method 'message object)))
                        (cond ((method? method)
                               (unsyntax
                                (syntax/loc stx
                                  (method . arguments))))
                              (else (raise-syntax-error 'send 
                                                        (format "expects class object with method ~s: <#class ~s>" 'message (car (send-msg object 'type))) 
                                                        (syntax (unsyntax (syntax/loc stx send)))))))
                      (raise-syntax-error 'send 
                                          (format "expects class object with method ~s: ~s" 'message object) 
                                          (syntax (unsyntax (syntax/loc stx send))))))))
             (else (raise-syntax-error #f "expected an message identifier" stx (syntax message)))))))


  (define-syntax (send/safe stx)
    (syntax-case stx (quote)
      ((src-send default object (quote message) . arguments) 
       (syntax/loc stx (src-send default object message . arguments)))
      ((src-send default src-object message . arguments)
       (cond ((identifier? (syntax message))
              (quasisyntax/loc stx
                (let ((object src-object))
;                  (display "send ") (display 'message) (display " to class ") (display object) (newline)
                  (if (instance? object)
                      (let ((method (get-method 'message object)))
                        (cond ((method? method)
                               (unsyntax
                                (syntax/loc stx
                                  (method . arguments))))
                              (else default)))
                      (raise-syntax-error 'send 
                                          (format "expects class object with method ~s: ~s" 'message object) 
                                          (syntax (unsyntax (syntax/loc stx send))))))))
             (else (raise-syntax-error #f "expected an message identifier" stx (syntax message)))))))

  (define-syntax (class stx)
    (syntax-case stx (define methods attributes attribute method)
      
      ;; Setzen der Defaultwerte
      ((src-class ((name id) (root-name root-id)) . rest) 
       (syntax/loc stx
         (src-class ((name id) (root-name root-id) () () ()) . rest)))
      
      ((src-class (name-part) . rest) 
       (syntax/loc stx 
         (src-class (name-part Root) . rest)))
      
      ((src-class ((name id) root-name) . rest) 
       (with-syntax ((root-id (datum->syntax (syntax src-class) 'super)))
         (syntax/loc stx 
           (src-class ((name id) (root-name root-id)) . rest))))
      
      ((src-class (name root-part) . rest) 
       (with-syntax ((id (datum->syntax (syntax src-class) 'self)))
         (syntax/loc stx 
           (src-class ((name id) root-part) . rest))))
      
      ;; Erzeugen der endgültigen Klasse
      ((src-class ((name id) (root-name root-id) define-list attribute-list method-list))
       (syntax/loc stx
         (define (name id)
           (let-expand (root-id (mcons 'instance (mcons (root-name id) '()))) () define-list
                       (make-class
                        'name
                        (method-expand ('inspect (lambda () 
                                                   (define (helper attributes values)
                                                     (cond ((null? attributes) 'ok)
                                                           (else
                                                            (helper (cdr attributes) (cdr values))
                                                            (let ((name (car attributes))
                                                                  (value (car values)))
                                                              (display name)
                                                              (display " = ")
                                                              (if (instance? value)
                                                                  (display (send-msg value 'show))
                                                                  (display value))
                                                              (newline)))))
                                                   (helper 'attribute-list (list . attribute-list))
                                                   (send-msg root-id 'inspect))) method-list)
                        root-id)))))
      ;; Einlesen von Defines
       ((src-class (name root-name define-list attribute-list method-list) (define (define-name . define-args) . define-body) . rest)
        (with-syntax ((src-lambda (datum->syntax (syntax define-name) 'lambda)))
          (syntax/loc stx
            (src-class (name 
                        root-name 
                        ((define-name (src-lambda define-args . define-body)) . define-list) 
                        attribute-list 
                        method-list) . rest))))
      
       ((src-class (name root-name define-list attribute-list method-list) (define define-name define-value) . rest)
          (syntax/loc stx
            (src-class (name 
                        root-name 
                        ((define-name define-value) . define-list) 
                        attribute-list
                        method-list) . rest)))
      
      ;; Einlesen von Attributwerten und Attributlisten
       ((src-class (name root-name define-list attribute-list method-list) (attribute attribute-name attribute-value) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((attribute-name attribute-value) . define-list) 
                    (attribute-name . attribute-list) 
                    method-list) . rest)))
      
       ((src-class (name root-name define-list attribute-list method-list) (attribute attribute-name) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((attribute-name (void)) . define-list) 
                    (attribute-name . attribute-list) 
                    method-list) . rest)))
      
       ((src-class (name root-name define-list attribute-list method-list) (attributes) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    define-list 
                    attribute-list 
                    method-list) . rest)))
      
       ((src-class (name root-name define-list attribute-list method-list) (attributes (attribute-name attribute-value) . attribute-rest) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((attribute-name attribute-value) . define-list) 
                    (attribute-name . attribute-list)
                    method-list) 
                   (attributes . attribute-rest) . rest)))
      
       ((src-class (name root-name define-list attribute-list method-list) (attributes attribute-name . attribute-rest) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((attribute-name (void)) . define-list) 
                    (attribute-name . attribute-list)
                    method-list) (attributes . attribute-rest) . rest)))
      
      ;; Einlesen von Methoden und Methodenlisten
      ((src-class (name root-name define-list attribute-list method-list) (method (method-name . method-args) . method-body) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((method-name (lambda method-args . method-body)) . define-list) 
                    attribute-list 
                    (method-name . method-list)) . rest)))
      
      ((src-class (name root-name define-list attribute-list method-list) (method method-name method-value) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((method-name method-value) . define-list) 
                    attribute-list
                    (method-name . method-list)) . rest)))
      
      ((src-class (name root-name define-list attribute-list method-list) (methods) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    define-list 
                    attribute-list 
                    method-list) . rest)))
      
      ((src-class (name root-name define-list attribute-list method-list) (methods ((method-name . method-args) . method-body) . method-rest) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((method-name (lambda method-args . method-body)) . define-list) 
                    attribute-list 
                    (method-name . method-list)) (methods . method-rest) . rest)))
      
      ((src-class (name root-name define-list attribute-list method-list) (methods (method-name method-value) . method-rest) . rest)
       (syntax/loc stx
        (src-class (name 
                    root-name 
                    ((method-name method-value) . define-list) 
                    attribute-list 
                    (method-name . method-list)) (methods . method-rest) . rest)))
      
      ;; Verbieten von weiteren Klassen-Member
      ((_ (name root-name define-list attribute-list method-list) class-part . rest)
       (raise-syntax-error #f "unkown class member." stx (syntax class-part)))))                                                        
  
 )
