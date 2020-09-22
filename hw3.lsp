;
; CS161 Hw3: Sokoban
;
; 005230642
; Rui Xu
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;   
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond 
  	((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond 
  	((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	    );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond 
  	((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; Helper function: contains (item row)
; Returns T if item is in the row.
(defun contains (item row)
	(cond
		((null row) nil)
		((equal item (car row)) t)
		(t (contains item (cdr row)))
		)
	)

; goal-test (s)
; Returns T if and only if s is a goal state.

(defun goal-test (s)
  (cond
  	((null s) t)
  	((contains box (car s)) nil)
  	(t (goal-test (cdr s)))
  	)
  );end defun


; EXERCISE: Modify this function to return the list of 
; successor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;


; Helper function: isOutOfBounds (s pos)
; Checks whether the pos is invalid. The col and row must be greater than 0 
; and less than the columns and rows of the s respectively.
; Returns T if the pos is invalid.

(defun isOutOfBounds (s pos)
	(or
		(< (second pos) 0)
		(< (first pos) 0)
		(> (second pos) (- (length s) 1))
		(> (first pos) (- (length (first s)) 1))
		)
	)

; Helper function: get-square-helper (row col)
; Checks the row passed and returns the item at column col.

; * Note that row here is not a number but a list.

(defun get-square-helper (row col)
	(cond
		((null row) nil)
		((= 0 col) (car row))
		(t (get-square-helper (cdr row) (- col 1)))
		)
	)

; Helper function: get-square (s pos)
; Returns the item in s at position (col, row).

(defun get-square (s pos)
	(let* (
		(col (first pos))
		(row (second pos)))
	    (cond
	    	((isOutOfBounds s pos) wall)
	    	((= 0 row) (get-square-helper(car s) col))
	    	(t (get-square (cdr s) (list col (- row 1))))
	    	)
		)
	)

; Helper function: isEmpty (square)
; Returns T if and only if a square is a star or a blank.

(defun isEmpty (square)
	(or (isStar square) (isBlank square))
	)

; Helper function: isMovable (src dest)
; Returns T if there exists a box at position src and it can
; be moved to position dest.

(defun isMovable (src dest)
	(and (or (isBox src) (isBoxStar src)) (isEmpty dest))
	)

; Helper function: set-square-helper (row col v)
; Takes a list row and returns the same list with the element at
; column col replaced with v.

; * Note that row here is not a number but a list.

(defun set-square-helper (row col v)
	(cond
		((null row) nil)
		((= 0 col) (cons v (cdr row)))
		(t (cons (car row) (set-square-helper (cdr row) (- col 1) v)))
		)
	)

; Helper function: set-square (s col row v)
; Returns the same state with the element in s at (col, row) replaced with v.

(defun set-square (s col row v)
	(cond
		((null s) nil)
		((= 0 row) (cons (set-square-helper (car s) col v) (cdr s)))
		(t (cons (car s) (set-square (cdr s) col (- row 1) v)))
		)
	)

; Helper function: move (s src dest withstar withoutstar)
; Checks the current square of the object; if the src has a star, the object will leave
; behind a star when it moves; if the dest has a star, then the object will be placed 
; on dest with a star as withstar. Returns the new state after the move.

(defun move (s src dest withstar withoutstar)
	(let* (
		(old (get-square s src))
		(new (get-square s dest)))
		(cond
			((= withstar old)
				(cond
					((isStar new) (set-square (set-square s (first src) (second src) star) (first dest) (second dest) withstar))
					(t (set-square (set-square s (first src) (second src) star) (first dest) (second dest) withoutstar))
					)
				)
			(t
				(cond
					((isStar new) (set-square (set-square s (first src) (second src) blank) (first dest) (second dest) withstar))
					(t (set-square (set-square s (first src) (second src) blank) (first dest) (second dest) withoutstar))
					)
				)
			);end cond
		);end let
	)

; Helper function: move-keeper (s src dest)
; Move the keeper from src to dest.

(defun move-keeper (s src dest)
	(move s src dest keeperstar keeper)
	)

; Helper function: move-box (s src dest)
; Move a box from src to dest.

(defun move-box (s src dest)
	(move s src dest boxstar box)
	)

; Helper function: try-move (s dir)
; Move the keeper in s.
; dir is the desired direction represented as (c, r) where c is the column movement (left / right)
; and r is the row movement (up / down).
; If there is no box, moves the keeper to the next square; if there is a box, moves the box first
; and then the keeper.
; Returns NIL if the move is invalid, otherwise the new state after the move.

(defun try-move (s dir)
	(let* (
		(pos (getKeeperPosition s 0))
		(next (list (+ (first dir) (first pos)) (+ (second dir) (second pos))))
		(nextsquare (get-square s next))
		(nextnext (list (+ (first dir) (first next)) (+ (second dir) (second next))))
		(nextnextsquare (get-square s nextnext))
		);end let

		(cond
			((isEmpty nextsquare) (move-keeper s pos next))
			((isMovable nextsquare nextnextsquare) (move-keeper (move-box s next nextnext) pos next))
			(t nil)
			)
		);end cond
	)


; next-states (s)
; Takes a state as input and returns the list of all states that can be reached from the
; given state in one move.

(defun next-states (s)
	(cleanUpList
		(list
			(try-move s '(0 -1)) ; UP
			(try-move s '(0 1))  ; DOWN
			(try-move s '(-1 0)) ; LEFT
			(try-move s '(1 0))  ; RIGHT 
			)
		)
	)



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; h0 (s)
; Returns the constant 0.

(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; Helper function: count-boxes-row
; Returns the number of boxes not on a star in a given list, row.

; * Note that row here is not a number but a list.

(defun count-boxes-row (row)
	(cond
		((null row) 0)
		((isBox (car row)) (+ 1 (count-boxes-row (cdr row))))
		(t (count-boxes-row (cdr row)))
		)
	)

; h1 (s)
; Returns the number of boxes which are not on goal positions in the given state.
; ANSWER: This is heuristic admissible because by h1, the estimate cost is less
; than the true cost. The boxes can be moved to goals without limitation.

(defun h1 (s)
	(cond
		((null s) 0)
		(t (+ (count-boxes-row (car s)) (h1 (cdr s))))
		)
  )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; Helper function: dis (num)
; Returns absolute value of num.

(defun dis (num)
	(cond
		((< num 0) (- 0 num))
		(t num)
		)
	)

; Helper function: manhattan (src dest)
; Returns the manhattan distance from src to dest.

(defun manhattan (src dest)
	(+ (dis (- (first src) (first dest))) (dis (- (second src) (second dest))))
	)

; Helper function: min-dist (src dests curr)
; Returns the smallest manhattan distance from src to any item in the list dests.

(defun min-dist (src dests curr)
	(cond
		((null dests) curr)
		(t
			(let* (
				(dist (manhattan src (car dests))))
				(cond
					((null curr) (min-dist src (cdr dests) dist))
					(t (cond
						((< dist curr) (min-dist src (cdr dests) dist))
						(t (min-dist src (cdr dests) curr))
						)
						)
					)
				);end let
			)
		);end cond
	)

; Helper function: sum-min-dist (srcs dests)
; Returns the sum of the minimum distances from every item in srcs to a destination in dests.

(defun sum-min-dist (srcs dests)
	(cond
		((or (null srcs) (null dests)) 0)
		(t (+ (min-dist (car srcs) dests nil) (sum-min-dist (cdr srcs) (cdr dests))))
		)
	)

; Helper function: sum-keeper-dist (s keeper dests)
; Returns the sum of distance of keeper to every destination in dests.

(defun sum-keeper-dist (s keeper dests)
	(cond
		((null dests) 0)
		(t (+ (manhattan keeper (car dests)) (sum-keeper-dist s keeper (cdr dests))))
		)
	)

; Helper function: get-items-helper (row r c item)
; Returns a list of the coordinates of each item in a row.

; * Note that row here is not a number but a list.

(defun get-items-helper (row r c item)
	(cond
		((null row) nil)
		((= item (car row)) (cons (list c r) (get-items-helper (cdr row) r (+ 1 c) item)))
		(t (get-items-helper (cdr row) r (+ 1 c) item))
		)
	)

; Helper function: get-items (s r item)
; Returns a list of the coordinates of every item found in s.

(defun get-items (s r item)
	(cond
		((null s) nil)
		(t (append (get-items-helper (car s) r 0 item) (get-items (cdr s) (+ r 1) item)))
		)
	)


; h005230642 (s)
; Heuristic function. Returns the sum of approximate minimum distance between boxes and stars,
; keeper and boxes.

(defun h005230642 (s)
	(let* (
		(boxes (get-items s 0 box))
		(stars (get-items s 0 star))
		(pos (getKeeperPosition s 0)))
	    (+ (sum-min-dist boxes stars) (sum-keeper-dist s pos boxes))
		)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(27,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(102,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(120,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(202,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(171,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(2844,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1109,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(2486,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(17588,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(14836,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(14182,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(4903,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(39162,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(45069,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(241493,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(??,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(564,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(224,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
