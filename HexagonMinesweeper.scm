#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HexagonMinesweeper
;;
;; Written by Torben Wiechers and Karsten-Kai König
;;
;; Copyright (C) 2010-2011 Torben Wiechers, Karsten-Kai König <KKoenig@posteo.de>
;;
;; This program is free software; you can redistribute it and/or modify it under 
;; the terms of the GNU General Public License as published by the Free Software 
;; Foundation; either version 3 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY 
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
;; PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this 
;; program; if not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (file "OO-pack.scm"))
(require graphics/graphics)

(open-graphics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Help-procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count predicate sequence)
  (define (iter rest count)
    (cond ((null? rest) count)
          ((predicate (car rest)) (iter (cdr rest) (+ count 1)))
          (else (iter (cdr rest) count))))
  (iter sequence 0))

(define (find? predicate sequence)
  (cond ((null? sequence) #f)
        ((predicate (car sequence)) #t)
        (else (find? predicate (cdr sequence)))))

(define (evenup l)
  (if (null? l) '()
      (let ((first (car l))
            (rest (cdr l)))
        (cond ((null? first) (evenup rest))
              ((pair? first) (evenup (cons (car first) (cons (cdr first) rest))))
              (else (cons first (evenup rest)))))))

(define (union list1 list2)
  (cond ((null? list1) list2)
        ((memq (car list1) list2) (union (cdr list1) list2))
        (else (union (cdr list1) (cons (car list1) list2)))))

(define (intersect list1 list2)
  (cond ((null? list1) '())
        ((memq (car list1) list2) (cons (car list1) (intersect (cdr list1) list2)))
        (else (intersect (cdr list1) list2))))

(define (disjoint? list1 list2)
  (cond ((null? list1) #t)
        ((memq (car list1) list2) #f)
        (else (disjoint? (cdr list1) list2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define FIELD_OFFSETX 10)
(define FIELD_OFFSETY 18)
(define FIELD_DELTAX 14)
(define FIELD_DELTAY 12)
(define FIELD_OFFSETBOTTOM 10)
(define FIELD_OFFSETRIGHT 4)
(define FIELD_POINTS (list (make-posn 0 0) (make-posn 7 4) (make-posn 7 12) (make-posn 0 16) (make-posn -7 12) (make-posn -7 4)))
(define OUTER_POINTS (list (make-posn 6 5) (make-posn 6 11) (make-posn 0 15) (make-posn -6 11)))
(define INNER_POINTS (list (make-posn -6 11) (make-posn -6 5) (make-posn 0 1) (make-posn 6 5)))

(define BACKGROUND_COLOR (make-rgb 255/255 255/255 255/255))
(define TEXT_COLOR (make-rgb 20/255 20/255 20/255))
(define WALL_COLOR (make-rgb 50/255 50/255 50/255))
(define BORDER_COLOR (make-rgb 90/255 90/255 90/255))
(define BOMB_COLOR (make-rgb 255/255 0/255 0/255))
(define BASE_COLOR (make-rgb 140/255 220/255 100/255))
(define BASE_EMPTY_COLOR (make-rgb 170/255 230/255 130/255))
(define CLOSED_COLOR (make-rgb 120/255 120/255 120/255))
(define OUTER_COLOR (make-rgb 90/255 90/255 90/255))
(define INNER_COLOR (make-rgb 140/255 140/255 140/255))
(define BOMB_COLORold (make-rgb 200/255 50/255 50/255))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window
;;
;; The class >>Window<< represents the window in which the game will be played.
;; >>Window<< open it und initialize basic functions.
;;
;; ******
;; Window
;; ******
;; viewport:
;; need-redraw:
;; buffer:
;; height:
;; width:
;; title:
;; startms:
;; redrawer:
;; ******
;; init
;; close
;; width
;; height
;; title
;; redraw
;; draw
;; mouse-clicked
;; key-pressed
;; need-redraw?
;; set-need-redraw
;; current-time
;; update
;; start
;; ******
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (Window)
  
  (attribute _viewport)
  (attribute _need-redraw #f)
  (attribute _buffer)
  (attribute _height)
  (attribute _width)
  (attribute _title)
  (attribute _startms 0)
  
  (attribute redrawer '())
  
  (method (init title width height backcolor) 
          (set! _title title)
          (set! _width width)
          (set! _height height)
          (set! _viewport (open-viewport _title _width _height))
          (set! _buffer (open-pixmap _title _width _height))
          ((draw-solid-rectangle _buffer) (make-posn 0 0) _width _height backcolor))
  
  (method (close)
          (close-viewport _viewport)
          (close-viewport _buffer))
  
  (method (width) _width)
  (method (height) _height)
  (method (title) _title)
  
  (method (redraw) 
          (send self draw _buffer)
          (copy-viewport _buffer _viewport)
          (set! _need-redraw #f))
  
  (method (draw viewport) 'MUST-OVERRIDE)
  (method (mouse-clicked posn left? right?) 'MUST-OVERRIDE)
  (method (key-pressed key-value) 'MUST-OVERRIDE)
  
  (method (need-redraw?) _need-redraw)
  (method (set-need-redraw) (set! _need-redraw #t) #t)
  (method (current-time) (if (= _startms 0) 0 (- (current-milliseconds) _startms)))
  
  (method (update time) 'MUST-OVERRIDE)
  
  (method (start) 
          (set! _startms (current-milliseconds))
          ((init-world _viewport) '())
          (send self set-need-redraw)
          ((set-on-tick-event _viewport) 
           20
           (lambda (currentState) 
             (let ((mouse (ready-mouse-click _viewport)))
               (if mouse (send self mouse-clicked (mouse-click-posn mouse) (left-mouse-click? mouse) (right-mouse-click? mouse))))
             (let ((key (ready-key-press _viewport)))
               (if-true key 
                        (viewport-flush-input _viewport)
                        (send self key-pressed (key-value key))))
             (send self update (send self current-time))
             (if (send self need-redraw?) (send self redraw))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Field
;;
;; The class >>Field<< represents a single field and its state. >>Field<< checks
;; the interagations with the field and react to that effect.
;;
;; ******
;; Field
;; ******
;; x:
;; y:
;; opened:
;; marked:
;; adjacentFields:
;; need-redraw:
;; ******
;; init
;; x
;; y
;; adjacent-fields
;; set-adjacent-fields
;; adjacent-field?
;; opened?
;; marked?
;; closed?
;; border?
;; open-field
;; mark-field
;; close-field
;; available-fields
;; available-count
;; open-available-fields
;; mark-available-fields
;; bomb-count
;; unmarked-count
;; need-redraw?
;; set-need-redraw
;; draw
;; draw-interior
;; inside?
;; mouse-clicked
;; show
;; ******
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (Field)
  
  (attribute _x)
  (attribute _y)
  
  (attribute _opened #f)
  (attribute _marked #f)
  (attribute _adjacentFields '())
  
  (attribute _need-redraw #t)
  
  (method (init x y)
          (set! _x x)
          (set! _y y))
  
  (method (x) _x)
  (method (y) _y)
  
  (method (adjacent-fields) _adjacentFields)
  (method (set-adjacent-fields fields) 
          (set! _adjacentFields fields))
  
  (method (adjacent-field? field)
          (if (and (or (= (+ _x 1) (send field x))
                       (= (- _x 1) (send field x)))
                   (= _y (send field y)))
              #t
              (cond ((or (= _y 0) (= (remainder _y 2) 0)) (if (and (or (= _x (send field x))
                                                                       (= (- _x 1) (send field x)))
                                                                   (or (= (+ _y 1) (send field y))
                                                                       (= (- _y 1) (send field y))))
                                                              #t
                                                              #f))
                    ((= (remainder _y 2) 1) (if (and (or (= _x (send field x))
                                                         (= (+ _x 1) (send field x)))
                                                     (or (= (+ _y 1) (send field y))
                                                         (= (- _y 1) (send field y))))
                                                #t
                                                #f)))))
  
  
  (method (opened?) _opened)
  (method (marked?) _marked)
  (method (closed?) (and (not _opened) (not _marked)))
  (method (border?) (and (opened?) (find? (lambda (field) (send field closed?)) _adjacentFields)))
  
  (method (open-field)
          (cond ((not _opened)
                 (set! _opened #t)
                 (send self show)
                 (set! _need-redraw #t))
                (else '())))
  
  (method (mark-field)
          (cond ((not _opened)
                 (set! _marked #t)
                 (send self show)
                 (set! _need-redraw #t))
                (else '())))
  
  (method (close-field)
          (send self set-need-redraw)
          (set! _opened #f)
          (set! _marked #f)
          '())
  
  
  
  (method (available-fields) (filter (lambda (field) (send field closed?)) (send self adjacent-fields)))
  (method (available-count) (count (lambda (field) (send field closed?)) (send self adjacent-fields)))
  
  (method (open-available-fields) (map (lambda (field) (send field open-field) (send field set-need-redraw)) (send self available-fields)))
  (method (mark-available-fields) (map (lambda (field) (send field mark-field) (send field set-need-redraw)) (send self available-fields)))
  
  (method (bomb-count) 
          (count (lambda (field) (send field is-a 'Bomb)) _adjacentFields))
  
  (method (unmarked-count) (- (send self bomb-count) (count (lambda (field) (send field marked?)) (send self adjacent-fields))))
  
  (method (need-redraw?) _need-redraw)
  (method (set-need-redraw) (set! _need-redraw #t) #t)
  
  (method (draw viewport)
          (let ((left (+ (* _x FIELD_DELTAX) FIELD_OFFSETX (if (even? _y) 0 (/ FIELD_DELTAX 2))))
                (top (+ (* _y FIELD_DELTAY) FIELD_OFFSETY)))
            (set! _need-redraw #f)
            (send self draw-interior viewport left top)
            ((draw-polygon viewport) FIELD_POINTS (make-posn left top) BORDER_COLOR)))
  
  (method (draw-interior viewport left top) 
          (define (offset-shift current)
            (make-posn (+ left (posn-x current)) (+ top (posn-y current))))
          (define (draw-lines viewport lines color)
            (if-false (null? (cdr lines))
                      ((draw-line viewport) (car lines) (cadr lines) color)
                      (draw-lines viewport (cdr lines) color)))
          (let ((corner (make-posn left top)))
            (cond (_marked 
                   ((draw-solid-polygon viewport) FIELD_POINTS corner BACKGROUND_COLOR)
                   ((draw-solid-polygon viewport) (list (make-posn 0 4) (make-posn 0 10) (make-posn 5 6)) corner BOMB_COLORold)
                   ((draw-polygon viewport) (list (make-posn -1 4) (make-posn -1 13)) corner TEXT_COLOR)
                   )
                  (else
                   ((draw-solid-polygon viewport) FIELD_POINTS corner CLOSED_COLOR)
                   (draw-lines viewport (map offset-shift OUTER_POINTS) OUTER_COLOR)
                   (draw-lines viewport (map offset-shift INNER_POINTS) INNER_COLOR)))))
  
  
  (method (inside? x y) 
          (let ((left (+ (* _x FIELD_DELTAX) FIELD_OFFSETX (if (even? _y) 0 (/ FIELD_DELTAX 2))))
                (top (+ (* _y FIELD_DELTAY) FIELD_OFFSETY)))
            (send self inside-polygon? x y (list (make-posn (+ left 0) (+ top 0))
                                                 (make-posn (+ left 7) (+ top 4))
                                                 (make-posn (+ left 7) (+ top 12)) 
                                                 (make-posn (+ left 0) (+ top 16))
                                                 (make-posn (+ left -7) (+ top 12))
                                                 (make-posn (+ left -7) (+ top 4))))))
  
  (method (inside-polygon? x y polygon)
          (define (help x y polygon)
            (cond ((= 1 (length polygon)) #t)
                  (else (let* ((x1 (posn-x (car polygon)))
                               (x2 (posn-x (car (cdr polygon))))
                               (y1 (posn-y (car polygon)))
                               (y2 (posn-y (car (cdr polygon))))
                               (a (+ (* (- x x1) (- y2 y1)) (* (- (- x2 x1))(- y y1)))))
                          (cond ((> a 0) #f)
                                (else (help x y (cdr polygon))))))))
          (help x y (reverse (cons (car polygon) (reverse polygon)))))  
  
  (method (mouse-clicked left? right?)
          (cond (left? (cond ((and (send self opened?) (= (send self unmarked-count) 0))
                              (send self open-available-fields))
                             (else (send self open-field))))
                (right? (cond ((send self opened?)
                               (send self mark-available-fields))
                              (else (send self mark-field))))))
  
  (method (show) (list 'Field _x _y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Empty
;;
;; The class >>Empty<< represents an empty field and initialize additional functions
;;
;; ******
;; Empty
;; ******
;; draw-interior
;; open-field
;; ******
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (Empty Field)
  
  
  (method (open-field)
          (cond ((send self opened?) '())
                ((= 0 (send self unmarked-count)) (send super open-field) (send self open-available-fields) (evenup (send self available-fields)))
                ((= (send self bomb-count) 0) (send super open-field) (send self open-available-fields) (evenup (send self adjacent-fields)))
                (else (send super open-field))))
  
  
  (method (draw-interior viewport left top)
          (cond ((send super opened?)
                 (let ((bombs (send super bomb-count)))
                   (cond ((> bombs 0)
                          ((draw-solid-polygon viewport) FIELD_POINTS (make-posn left top) BASE_COLOR)
                          ((draw-string viewport) (make-posn (- left 2) (+ top 13)) (number->string bombs) TEXT_COLOR))
                         (else 
                          ((draw-solid-polygon viewport) FIELD_POINTS (make-posn left top) BASE_EMPTY_COLOR)))))
                (else 
                 (send super draw-interior viewport left top)))))

(define (create-Empty x y) (create-instance Empty x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wall
;;
;; The class >>Wall<< represants a field at the border.
;;
;; ******
;; Wall
;; ******
;; open-field
;; mark-field
;; closed?
;; marked?
;; draw-inter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (Wall Field) 
  
  (method (open-field) '())
  (method (mark-field) '())
  
  (method (closed?) #f)
  (method (marked?) #f)
  
  (method (draw-interior viewport left top) 
          ((draw-solid-polygon viewport) FIELD_POINTS (make-posn left top) WALL_COLOR)))

(define (create-Wall x y) (create-instance Wall x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bomb
;;
;; The class >>Bomb<< represents a field containing a bomb.
;;
;; ******
;; Bomb
;; ******
;; draw-interior
;; ******
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(class (Bomb Field)
  (method (draw-interior viewport left top)
          (if (send super opened?)
              ((draw-solid-polygon viewport) FIELD_POINTS (make-posn left top) BOMB_COLOR)
              (send super draw-interior viewport left top))))
(define (create-Bomb x y) (create-instance Bomb x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Minesweeper
;;
;; The class >>Minesweeper<< represents the game and controls it.
;;
;; ******
;; Minesweeper
;; ******
;; width:
;; height:
;; fields:
;; finished:
;; starttime:
;; currenttime:
;; finishedtime:
;; ******
;; init
;; load-fields
;; acceptable-field
;; fields
;; need-redraw?
;; draw
;; finished?
;; solved?
;; check-solved
;; mouse-clicked
;; key-pressed
;; ******
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (Minesweeper Window)
  
  (attribute _width)
  (attribute _height)
  (attribute _fields)
  
  (attribute _finished #f)
  
  (attribute _starttime 0) 
  (attribute _currenttime 0)
  (attribute _finishedtime 0)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Load game
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (method (init width height) 
          (set! _width width)
          (set! _height height)
          (send super init "Minesweeper" 
                (+ (* width FIELD_DELTAX) FIELD_OFFSETX FIELD_OFFSETRIGHT) 
                (+ (* height FIELD_DELTAY) FIELD_OFFSETY FIELD_OFFSETBOTTOM) BACKGROUND_COLOR)
          (load-fields))
  
  (method (load-fields)
          (display "Try to find acceptable field ...") (newline)
          (set! _fields (create-fields 0 0 '() '() '() '()))
          (let ((unempties (filter (lambda (field) (and (send field is-a 'Empty)  (= (send field bomb-count) 0))) _fields)))
            (if (null? unempties)
                (load-fields)
                (let ((index (random (length unempties))))
                  (send (list-ref unempties index) open-field)
                  (cond ((send self acceptable-field?)
                         (send self set-need-redraw)
                         (set! _finished #f)
                         (display "Found acceptable field.") (newline))
                        (else
                         (load-fields)))))))
  
  (define (create-fields x y all line1 line2 line3)
    (define (line nr) (filter (lambda (field) (= (send field y) nr)) all))
    (cond ((>= x _width) (cond ((> y 1) (calculate-adjacents line1 line2 line3) (create-fields 0 (+ y 1) all line2 line3 '())) (else (create-fields 0 (+ y 1) all line2 line3 '()))))
          ((= y _height) (calculate-adjacents line1 line2 line3) all)
          (else 
           (let ((newfield (create-field x y)))
             (create-fields (+ x 1) y (cons newfield all) line1 line2 (cons newfield line3))))))
  
  (define (calculate-adjacents line1 line2 line3)
    (map (lambda (field) (send field set-adjacent-fields (filter (lambda (feld) (send feld adjacent-field? field)) (append line1 line2 line3)))) line2))
  
  
  
  (define (create-field x y) 
    (cond ((or (= x 0) (= y 0) (= x (- _width 1)) (= y (- _height 1)))
           (create-Wall x y))
          ((= 1 (random 6)) (create-Bomb x y))
          (else (create-Empty x y))))
  
  (method (acceptable-field?) #t)
  
  (method (fields) _fields)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Draw game
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (method (need-redraw?) 
          (let ((seconds (round (/ (- (send self current-time) _starttime) 1000))))
            (or 
             (and (> seconds _currenttime) (set! _currenttime seconds))
             (send super need-redraw?)
             (find? (lambda (field) (send field need-redraw?)) _fields))))
  
  (method (draw viewport)
          ((draw-solid-rectangle viewport) (make-posn 5 0) 300 15 BACKGROUND_COLOR)
          ((draw-string viewport) (make-posn 5 12) 
                                  (string-append "Time: " 
                                                 (number->string (if _finished _finishedtime _currenttime))
                                                 ", free fields: " 
                                                 (number->string (count (lambda (field) (send field closed?)) _fields))
                                                 ", missing bombs: "
                                                 (number->string (count (lambda (field) (and (send field closed?) (send field is-a 'Bomb))) _fields)))
                                  TEXT_COLOR)
          (for-each (lambda (field) (cond ((send field need-redraw?) (send field draw viewport)))) _fields))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Gamecontrol
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (method (finished?) 
          (or _finished
              (find? (lambda (field) (and (send field is-a 'Bomb) (send field opened?))) _fields)))
  
  (method (solved?)
          (not (find? (lambda (field) (and (send field is-a 'Empty) (not (send field opened?)))) _fields)))
  
  (method (check-solved)
          (cond ((send self finished?) 
                 (set! _finished #t)
                 (set! _finishedtime _currenttime)
                 (for-each (lambda (field) 
                             (if (or (send field closed?) 
                                     (and (send field marked?) (send field is-a 'Empty))) (send field open-field)))
                           _fields))
                ((send self solved?)
                 (set! _finished #t)
                 (set! _finishedtime _currenttime))))
  
  (method (mouse-clicked posn left? right?) 
          (let ((x (posn-x posn))
                (y (posn-y posn)))
            (if-false (send self finished?)
                      (find? (lambda (field) (if (send field inside? x y) (send field mouse-clicked left? right?) #f)) _fields)
                      (send self check-solved))))
  
  (method (key-pressed key-value) 
          (cond ((eq? key-value #\r) 
                 (send self load-fields) 
                 (set! _starttime (send self current-time)) 
                 (set! _currenttime 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SolvableMinesweeper
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class (SolvableMinesweeper Minesweeper)
  
  (method (acceptable-field?) (solvable?))
  
  (method (key-pressed key-value) 
          (if (eq? key-value #\s) (send self solve) (send super key-pressed key-value)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Solvestrategies for Minesweeper
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (method (solve)
          (if-false (send self finished?)
                    (send self single-solve-step)
                    (send self check-solved)))
  
  
  (method (solvable?) (define (help rest closed)
                        (cond ((null? rest) #t  (map (lambda (field) (send field close-field)) closed))
                              ((send self single-solve-step) (send self full-solve-step) (help (filter (lambda (field) (send field closed?)) rest) closed))
                              (else #f)))
          (help (send self fields) (filter (lambda (field) (send field closed?)) (send self fields))))
  
  
  
  (method (full-solve-step) (let ((vorher (filter (lambda (feld) (send feld closed?)) (send self fields))))
                              (map (lambda (field) (send self solve-field field)) (filter (lambda (feld) (send feld opened?)) (send self fields)))
                              (intersect (filter (lambda (feld) (send feld opened?)) (send self fields)) vorher)))
  
  
  (method (single-solve-step) (find? (lambda (field) (not (null? (send self solve-field field)))) (send self fields)))
  
  
  (method (solve-field field) (cond ((= 0 (send field unmarked-count)) (send field open-available-fields))
                                    ((= (send field unmarked-count) (send field available-count)) (send field mark-available-fields))
                                    (else '())))
  
  
  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BetterMinesweeper
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(class (BetterMinesweeper SolvableMinesweeper)
  
  (attribute _mark-lst '())
  (attribute _makeitbetter '())
  
  
  (method (full-solve-step)
          (let ((full (send super full-solve-step))
                (all (send super fields)))
            (cond ((and 
                    (=
                     (count (lambda (field) (send field marked?)) all)
                     (count (lambda (field) (and (send field closed?) (send field is-a 'Bomb))) all))
                    (null? full)) 
                   (map (lambda (field) (send field open-field)) (filter (lambda (field) (not (send field marked?))) all))) 
                  ((null? full) (send self mark-after-full-solve))
                  (else full))))
  
  (method (single-solve-step)
          (let ((single (send super single-solve-step))
                (all (send super fields)))
            (cond ((and 
                    (=
                     (count (lambda (field) (send field marked?)) all)
                     (count (lambda (field) (and (send field closed?) (send field is-a 'Bomb))) all))
                    (not single)) 
                   (map (lambda (field) (send field open-field)) (filter (lambda (field) (not (send field marked?))) all))) 
                  (else single))))
  
  (method (mark-after-full-solve)
          
          (define (without-empty-list rest lst)
            (cond ((null? lst) rest)                 
                  ((null? (car lst)) (without-empty-list rest (cdr lst)))
                  (else (without-empty-list (cons (car lst) rest) (cdr lst)))))
          
          (define (intersect-all lst erg)
            (cond ((null? lst) erg)
                  ((null? (without-empty-list '() erg)) (intersect-all (cdr lst) (car lst)))
                  ((null? (without-empty-list '() (car lst))) (intersect-all (cdr lst) erg))
                  (else (intersect-all (cdr lst) (intersect (car lst) erg)))))
          
          (define (help cnt lst rest conslst)
            (cond ((null? _mark-lst) (map (lambda (field) (send field close-field)) _makeitbetter) 
                                     (cond ((or (null? conslst) (null? (evenup (without-empty-list '() (intersect-all (cdr conslst) (car conslst))))))
                                            '()
                                            (display "null"))
                                           (else (map (lambda (field) (send field mark-field)) (evenup (without-empty-list '() (intersect-all (cdr conslst) (car conslst)))))
                                                 (display "else"))))
                  ((null? rest)
                   (set! _mark-lst (cdr _mark-lst))
                   (help cnt lst _mark-lst conslst))
                  ((= cnt 1)
                   (let* ((all (send super fields))
                          (makeitbetter (map solve-field2 
                                             (filter (lambda (field) (send field opened?)) 
                                                     (evenup (map (lambda (field) (send field adjacent-fields)) _makeitbetter))))))
                     (cond ((null? makeitbetter) 
                            (map (lambda (field) (send field close-field)) _mark-lst)
                            (help cnt '() rest (cons lst conslst)))   
                           (else (help 0 (cons makeitbetter lst) rest conslst)))))
                  (else (send (car rest) mark-field)
                        (help 1 (cons (car _mark-lst) lst) (cdr rest) conslst))) 
            ) 
          (set! _mark-lst (filter (lambda (field) (not (send field marked?))) 
                                  (filter (lambda (field) (> (send field available-count) 0)) 
                                          (filter (lambda (field) (send field closed?)) 
                                                  (evenup (map (lambda (field) (send field adjacent-fields)) 
                                                               (filter (lambda (field) (send field opened?)) (send self fields))))))))
          (set! _makeitbetter _mark-lst)
          (help 0 '() _mark-lst '()))  
  
  
  
  (define (solve-field field)
    (let ((unmarked (send field unmarked-count)))
      (cond ((= unmarked 0) (send field open-available-fields))
            ((= (send field available-count) unmarked) (send field mark-available-fields))  
            (else '()))))
  
  (define (solve-field2 field)
    (cond ((< (send field available-count) (send field unmarked-count)) (send field mark-available-fields))
          (else '()))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application-start
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define mywindow (create-instance Minesweeper 30 20))
(define mywindow (create-instance SolvableMinesweeper 30 20))
;(define mywindow (create-instance BetterMinesweeper 30 20))
(send mywindow start)
