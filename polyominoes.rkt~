;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
(require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

(define grid1 (list (list #\a #\a)
                    (list #\a #\a)
                    (list #\a #\.)))

(define grid2 (list (list #\q #\q #\q)
                    (list #\. #\q #\.)
                    (list #\. #\q #\.)))

(define grid3 (list (list #\a #\a)
                    (list #\a #\a)
                    (list #\a #\a)))

(define grid4 (list (list #\. #\a #\.)
                    (list #\a #\a #\a)
                    (list #\. #\a #\.)))

;; (1)

;; (a)
;; (build-2dlist x y f) is a two-dimensional equivalent of build-list
;; Build-2dlist: Nat Nat (Nat Nat -> X) -> (list (listof X) (listof X))
;; Examples:

(check-expect (build-2dlist 3 2 +) (list (list 0 1 2) (list 1 2 3)))

(check-expect (build-2dlist 3 3 +) (list (list 0 1 2) (list 1 2 3) (list 2 3 4)))

(define (build-2dlist x y f)
  (build-list y (lambda (a) (build-list x (lambda (b) (f b a))))))

;; Tests:

(check-expect (build-2dlist 3 3 list) (list (list (list 0 0) (list 1 0) (list 2 0))
                                            (list (list 0 1) (list 1 1) (list 2 1))
                                            (list (list 0 2) (list 1 2) (list 2 2))))

;; (b)
;; (all-positions w h) produces a (listof Pos) containing all possible positions
;;    in a grid with width w and height h
;; All-Positions: Nat Nat -> (listof Pos)
;; Requires: w and h are greater than zero
;; Examples:

(check-expect (all-positions 1 1) (list (make-pos 0 0)))

(check-expect (all-positions 3 2) (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                        (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)))

(define (all-positions w h)
  (foldr append empty (build-2dlist w h make-pos)))

;; Tests:

(check-expect (all-positions 3 3) (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                        (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)
                                        (make-pos 0 2) (make-pos 1 2) (make-pos 2 2)))

;; (2)

;; (all-orientations polyo) produces a list of Grids representing all the possible
;;   rotations and reflections of polyo
;; All-Orientations: Grid -> (listof Grid)
;; Examples:

(check-expect (all-orientations grid4) (list grid4))

(check-expect (all-orientations grid3) (list (list (list #\a #\a)
                                                   (list #\a #\a)
                                                   (list #\a #\a))
                                             
                                             (list (list #\a #\a #\a)
                                                   (list #\a #\a #\a))))

(define (all-orientations polyo)
  (local [(define orig-polyo polyo)
          (define reflect-polyo (foldl cons empty polyo))
          (define (list-rotations polyo)
            ;; (list-rotations polyo) produces the list of all distinct grids
            ;;   representing the rotations of polyo
            ;; List-Rotations: Grid -> (listof Grid)
            (local [(define (rotate polyo)
                      ;; (rotate polyo) produces the grid representing the rotation of polyo
                      ;; Rotate: Grid -> Grid
                      (cond [(empty? (first polyo)) empty]
                            [else (cons (foldl cons empty (map first polyo))
                                        (rotate (map rest polyo)))]))]
              (cond [(equal? orig-polyo (rotate polyo)) (list polyo)]
                    [else (cons polyo (list-rotations (rotate polyo)))])))]
    (cond [(member? reflect-polyo (list-rotations polyo)) (list-rotations polyo)]
          [else (append (list-rotations polyo)
                        (local [(define (list-rotations1 polyo)
                                  ;; same as (list-rotation polyo)
                                  (local [(define (rotate polyo)
                                            (cond [(empty? (first polyo)) empty]
                                                  [else (cons (foldl cons
                                                                     empty
                                                                     (map first polyo))
                                                              (rotate (map rest polyo)))]))]
                                    (cond [(equal? reflect-polyo (rotate polyo))
                                           (list polyo)]
                                          [else (cons polyo
                                                      (list-rotations1
                                                       (rotate polyo)))])))]
                          (list-rotations1 reflect-polyo)))])))

;; Tests:

(check-expect (all-orientations grid2) (list (list (list #\q #\q #\q)
                                                   (list #\. #\q #\.)
                                                   (list #\. #\q #\.))
                                             
                                             (list (list #\. #\. #\q)
                                                   (list #\q #\q #\q)
                                                   (list #\. #\. #\q))
                                             
                                             (list (list #\. #\q #\.)
                                                   (list #\. #\q #\.)
                                                   (list #\q #\q #\q))
                                             
                                             (list (list #\q #\. #\.)
                                                   (list #\q #\q #\q)
                                                   (list #\q #\. #\.))))

(check-expect (all-orientations grid1) (list (list (list #\a #\a)
                                                   (list #\a #\a)
                                                   (list #\a #\.))
                                             
                                             (list (list #\a #\a #\a)
                                                   (list #\. #\a #\a))
                                             
                                             (list (list #\. #\a)
                                                   (list #\a #\a)
                                                   (list #\a #\a))
                                             
                                             (list (list #\a #\a #\.)
                                                   (list #\a #\a #\a))
                                             
                                             (list (list #\a #\.)
                                                   (list #\a #\a)
                                                   (list #\a #\a))
                                             
                                             (list (list #\a #\a #\a)
                                                   (list #\a #\a #\.))
                                             
                                             (list (list #\a #\a)
                                                   (list #\a #\a)
                                                   (list #\. #\a))
                                             
                                             (list (list #\. #\a #\a)
                                                   (list #\a #\a #\a))))

;; (3)

;; (first-empty-pos grid) produce the position of the first #\. in grid. The first
;;   #\. is the leftmost #\. in the uppermost row that has one.
;; First-Empty-Pos: Grid -> Pos
;; Examples:

(check-expect (first-empty-pos grid1) (make-pos 1 2))

(check-expect (first-empty-pos grid2) (make-pos 0 1))

(define (first-empty-pos grid)
  (local [(define (first-#\. grid x y)
            ;; (first-#\. grid) produces the position of the first empty position in grid
            ;; First-#\.: Grid Nat Nat -> Pos
            ;; Requires: x and y are zeros
            (cond [(empty? grid) false]
                  [(not (member? #\. (first grid))) (first-#\. (rest grid) x (add1 y))]
                  [(equal? #\. (first (first grid))) (make-pos x y)]
                  [else (first-#\. (cons (rest (first grid)) (rest grid)) (add1 x) y)]))]
    (first-#\. grid 0 0)))
;; Tests:

(check-expect (first-empty-pos grid3) false)

(check-expect (first-empty-pos grid4) (make-pos 0 0))

;; (4)

;; (superimpose base top pos) produces a new Grid in which top is laid over base
;;  such that pos indicates the location of the upper left corner of top
;; Superimpose: Grid Grid Pos -> Grid
;; Examples:

(check-expect (superimpose (list (list #\b #\. #\. #\. #\.)
                                 (list #\b #\b #\b #\. #\.)
                                 (list #\. #\b #\. #\. #\.))
                           grid1
                           (make-pos 0 0))
              (list (list #\a #\a #\. #\. #\.)
                    (list #\a #\a #\b #\. #\.)
                    (list #\a #\b #\. #\. #\.)))

(check-expect (superimpose (list (list #\b #\. #\. #\. #\.)
                                 (list #\b #\b #\b #\. #\.)
                                 (list #\. #\b #\. #\. #\.))
                           grid1
                           (make-pos 3 0))
              (list (list #\b #\. #\. #\a #\a)
                    (list #\b #\b #\b #\a #\a)
                    (list #\. #\b #\. #\a #\.)))

(define (superimpose base top pos)
  (local [(define (overwrite-line base top x length-top)
            ;; (overwrite-line base top x length-top) produce a list in which top
            ;;   is laid over base such that x is the position of (first top)
            ;; Overwrite-Line: (listof Char) (listof Char) Nat Nat -> (listof Char)
            ;; Requires: length-top is (length top)
            (cond [(empty? base) empty]
                  [(= x 0) (cond [(= length-top 0) base]
                                 [(empty? base) empty]
                                 [(equal? (first top) #\.)
                                  (overwrite-line base (rest top)
                                                  (add1 x) (sub1 length-top))]
                                 [else (overwrite-line (cons (first top) (rest base))
                                                       (rest top)
                                                       (add1 x)
                                                       (sub1 length-top))])]
                  [else (cons (first base) (overwrite-line (rest base)
                                                           top
                                                           (sub1 x)
                                                           length-top))]))]
    (cond [(= (pos-y pos) 0)
           (foldr (lambda (fbase ftop y)
                    (cons (overwrite-line fbase ftop
                                          (pos-x pos) (length ftop)) y))
                  empty
                  base
                  (local [(define (shrink-list n lst)
                            ;; (shrink-list n lst) produces the first n
                            ;;  elements of lst
                            ;; Shrink-List: Nat (listof X) -> (listof X)
                            (cond [(empty? lst) empty]
                                  [(= 0 n) empty]
                                  [else (cons (first lst)
                                              (shrink-list (sub1 n)
                                                           (rest lst)))]))]
                    (cond [(= (length base) (length top)) top]
                          [(< (length base) (length top))
                           (shrink-list (length base) top)]
                          [else
                           (reverse
                            (local [(define (add-to-lst n lst)
                                      ;; (add-to-lst n lst) adds to lst n
                                      ;;  lists of #\. of the same length
                                      ;;  as the lists in lst
                                      ;; Add-To-Lst: Nat Grid -> Grid
                                      (cond [(= 0 n) lst]
                                            [else (cons
                                                   (local [(define (construct n)
                                                             ;; (construct n) produces
                                                             ;;  a list of n #\.
                                                             ;; Construct: Nat -> (listof Char)
                                                             (cond [(= n 0) empty]
                                                                   [else (cons
                                                                          #\.
                                                                          (construct
                                                                           (sub1 n)))]))]
                                                     (construct (length (first lst))))
                                                   (add-to-lst (sub1 n) lst))]))]
                              (add-to-lst (- (length base) (length top))
                                          (reverse top))))])))]
          [else (cons (first base)
                      (superimpose (rest base)
                                   top
                                   (make-pos (pos-x pos) (sub1 (pos-y pos)))))])))

;; Tests:

(check-expect (superimpose (list (list #\b #\. #\. #\. #\.)
                                 (list #\b #\b #\b #\. #\.)
                                 (list #\. #\b #\. #\. #\.)
                                 (list #\. #\b #\. #\. #\.))
                           grid4
                           (make-pos 2 0))
              (list (list #\b #\. #\. #\a #\.)
                    (list #\b #\b #\a #\a #\a)
                    (list #\. #\b #\. #\a #\.)
                    (list #\. #\b #\. #\. #\.)))

(check-expect (superimpose (list (list #\b #\. #\. #\. #\.)
                                 (list #\b #\b #\b #\. #\.)
                                 (list #\. #\b #\. #\. #\.))
                           grid1
                           (make-pos 4 1))
              (list (list #\b #\. #\. #\. #\.)
                    (list #\b #\b #\b #\. #\a)
                    (list #\. #\b #\. #\. #\a)))

(check-expect (superimpose (list (list #\b #\. #\. #\. #\.)
                                 (list #\b #\b #\b #\. #\.)
                                 (list #\. #\b #\. #\. #\.))
                           grid1
                           (make-pos 4 3))
              (list (list #\b #\. #\. #\. #\.)
                    (list #\b #\b #\b #\. #\.)
                    (list #\. #\b #\. #\. #\.)))

;; A temporary neighbours function that always fails.  
;; Provide only the purpose, contract and function definition.

;; (neighbours s) produces a list of States in which one additional polyomino
;;  has been placed in the puzzle and removed from the list of pices yet to be placed
;; Neighbours: State -> (listof State)

(define (neighbours s)
  (local [(define (my-superimpose base top pos)
            (local [(define (n#\.s lst x)
                      ;; (n#\.s lst) produces the number of #\. before the first
                      ;;   non #\. character
                      ;; N#\.s: (listof Char) Nat -> Nat
                      ;; Requires: x is zero
                      (cond
                        [(empty? lst) x]
                        [(not (char=? (first lst) #\.)) x]
                        [else (n#\.s (rest lst) (add1 x))]))]
              (superimpose base top (make-pos (- (pos-x pos) (n#\.s (first top) 0))
                                              (pos-y pos)))))
          (define (superimposable? base top pos)
            (local [(define new-grid (my-superimpose base top pos))
                    (define (first-non-empty lst)
                      ;; (first-non-empty lst) produces the first non-empty square of lst
                      ;; First-Non-Empty: (listof Char) -> Char
                      ;; Requires: lst contains a non empty square
                      (cond [(not (equal? (first lst) #\.)) (first lst)]
                            [else (first-non-empty (rest lst))]))]
              (local [(define (n-of-char grid char)
                        ;; (n-of-char grid char) produces the number of occurences of char in grid
                        ;; N-Of-Char: Grid Char -> Nat
                        (foldr (lambda (x y)
                                 (+ (foldr (lambda (a b) (cond [(equal? a char) (add1 b)]
                                                               [else b]))
                                           0
                                           x) y))
                               
                               0
                               grid))
                      (define (n-of-char-not-from-top grid char)
                        ;; (n-of-char-not-from-top grid char) produces the number of non-empty
                        ;;  characters in grid that are not char
                        ;; N-Of-Char-Not-From-Top: Grid Char -> Nat
                        (foldr (lambda (x y)
                                 (+ (foldr (lambda (a b) (cond [(not (or (equal? a #\.)
                                                                         (equal? a char)))
                                                                (add1 b)]
                                                               [else b]))
                                           0
                                           x) y))
                               0
                               grid))]
                (and (= (n-of-char top (first-non-empty (first top)))
                        (n-of-char new-grid (first-non-empty (first top))))
                     (= (n-of-char-not-from-top base (first-non-empty (first top)))
                        (n-of-char-not-from-top new-grid (first-non-empty (first top))))))))]
    (local [(define (offsets puzzle top pieces)
              ;; (offsets puzzle top pieces) produces a list of State in which
              ;;   each State is the result of the superposition of any orientation
              ;;   of top on puzzle and the list of the pieces remaining (without top)
              ;; Offsets: Grid Grid (listof Grid) -> (listof States)
              (local [(define lst (all-orientations top))
                      (define fep (first-empty-pos puzzle))]
                (foldr (lambda (x y)
                         (cond [(superimposable? puzzle x fep)
                                (cons (make-state (my-superimpose puzzle x fep)
                                                  (filter
                                                   (lambda (x)
                                                     (cond [(equal? x top) false]
                                                           [else true]))
                                                   pieces)) y)]
                               [else y]))
                       empty
                       lst)))]
      (foldr append empty
             (foldr (lambda (x y) (cons (offsets (state-puzzle s)
                                                 x (state-pieces s)) y))
                    empty
                    (state-pieces s))))))

;; (solve-puzzle grid polys viz-style)
;; Solve a polyomino puzzle, given the initially empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to consume
;; 'offline for viz-style.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)
;
;;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)
;
;; Display every step of the search as it progresses.
(solve-puzzle
 (strlist->grid '("...." "...." "...." "...." "...." "...."))
 (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
 'interactive)

