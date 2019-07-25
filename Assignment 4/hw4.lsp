; Programming Assignment 4
; Jingjing Nie, 304567417

; MANHATTAN takes in two arguments, the tile state, and a number used as counter.
; The state of the tiles is represented as a list of tile numbers, and the number
; counter is passed in as the initial value 0. It then calculates the Manhattan 
; distance between the passed in state with the goal state, (0 1 2 3 4 5 6 7 8),
; and returns the distance as a number.
(defun MANHATTAN (state i)
	(cond ((null state) 0)
	(t (let ((l (abs (- (car state) i))))
		(cond
			; for positions (2,3) or (5,6), cannot go directly from one to the other,
			; do the number of moves needed should be numberical difference plus two.
			((or (and (= i 2) (= (car state) 3)) (and (= i 3) (= (car state) 2)) 
				(and (= i 5) (= (car state) 6)) (and (= i 6) (= (car state) 5)))
				(+ (+ l 2) (MANHATTAN (cdr state) (+ i 1))))
			; do not count the difference for blank tiles
			((= (car state) 0) (MANHATTAN (cdr state) (+ i 1)))
			; for positions separated by more than 3 or 6 units, the moves can be
			; achieved by going up or down instead, so value should minus 2 or 4.
			((>= l 3) (+ (- l 2) (MANHATTAN (cdr state) (+ i 1))))
			((>= l 6) (+ (- l 4) (MANHATTAN (cdr state) (+ i 1))))
			(t (+ l (MANHATTAN (cdr state) (+ i 1)))))
	)))
)

; GETLENGTH takes in a single argument, the list representing the state of tiles.
; It then calculates and returns the length of the list.
(defun GETLENGTH (lst)
	(cond ((null lst) 0)
		(t (+ 1 (GETLENGTH (cdr lst)))))
)

; GETBLANKPOS takes two arguments, a list representing the state of tiles, and a
; number used to help find the position, with an initial value of 0. It then returns
; the position / index of the blank tile in the frame
(defun GETBLANKPOS (lst pos)
	(cond ((= (car lst) 0) pos)
		(t (GETBLANKPOS (cdr lst) (+ pos 1))))
)

; MOVABLETILES takes in two arguments, a list representing the state of tiles, and 
; a number representing the position of the blank tile. It returns a list of tiles
; that can move to the blank position.
(defun MOVABLETILES (lst pos)
	(nth pos '((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7)))
)

; REMOVEMOVABLE takes in two arguments, a list representing the state of tiles, and
; a number representing the position of the previous blank tile, i.e, where the current
; tile moves from. It then removes that position from the new movable tile list of
; the current blank tile, and returns the new movable list.
(defun REMOVEMOVABLE (lst pos)
	(cond ((null lst) nil)
	(t (cond ((not (= pos (car lst)))
		(append (list (car lst)) (REMOVEMOVABLE (cdr lst) pos)))
		(t (REMOVEMOVABLE (cdr lst) pos))
		)))
)

; TOTINVERSION takes in a single argument, that is a list representing the state of
; tiles, and returns the number of total inversions in the list.
(defun TOTINVERSION (lst)
	(cond ((null lst) 0)
		(t 
			(cond ((not (= (car lst) 0))
			(+ (TOTINVERSION (cdr lst)) (NUMINVERSION (cdr lst) (car lst))))
			(t (TOTINVERSION (cdr lst))))
		))
)

; NUMINVERSION takes in two arguments, a list representing the state of tiles, and
; a number. It checks the number of inversions of that number with respect to the
; rest of the list, and returns the number found.
(defun NUMINVERSION (lst num)
	(cond ((null lst) 0)
	(t (cond ((not (= (car lst) 0))
			(cond ((> num (car lst)) (+ 1 (NUMINVERSION (cdr lst) num)))
			(t (NUMINVERSION (cdr lst) num))))
		(t (NUMINVERSION (cdr lst) num)))
	))
)

; VALID takes in a single argument, a list reresenting the state of the tiles,
; and returns whether the list can successfully reach the goal state.
(defun VALID (lst)
	(cond ((= (rem (TOTINVERSION lst) 2) 0) T)
		(t nil))
)

; MAKEMOVE takes in three arguments, a list reresenting the state of the tiles,
; a number representing the blank tile position, and a number representing the
; position that the blank tile will move to. It then calls MOVE to return a list
; representing the new state after the move.
(defun MAKEMOVE (lst blankpos moveto)
	(let ((movetilenum (nth moveto lst)))
		(MOVE lst blankpos moveto movetilenum 0)
		)
)

; MOVE takes in five arguments. The first one is a list reresenting the state of
; the tiles. The second one is a number representing the position of the blank tile.
; The third one is a number representing the position that the blank tile will move
; to. The forth one is the number of the tile that is to be moved, and the last one
; is a number used as a counter. It then makes the move based on the passed in values,
; and returns a list representing the new state after the move.
(defun MOVE (lst blankpos moveto movetilenum counter)
	(cond ((null lst) nil)
	(t (cond
		; append the others as the same order unless the two moved ones 
		((= counter moveto) 
			(append (list 0) (MOVE (cdr lst) blankpos moveto movetilenum (+ counter 1)) ))
		((= counter blankpos)
			(append (list movetilenum) (MOVE (cdr lst) blankpos moveto movetilenum (+ counter 1)) ))
		(t (append (list (car lst)) (MOVE (cdr lst) blankpos moveto movetilenum (+ counter 1)) ))
	)))
)

; ITERATION takes in six arguments. The first one is a list reresenting the state of
; the tiles. The second one is a number representing the position of the blank tile.
; The third one is a list of movable tiles that can be moved to the blank space near
; them. The forth one is a number representing the g(n) value. The fifth one is a number
; representing the threshod value, and the last one is a list storing the possible moves
; as the result to goal state. It performs the iteration part of the IDA* algorithm, and
; stops DFS when f(n) exceeds the threshod value. It returns a list of moves after the
; iteration.
(defun ITERATION (lst blankpos movable gn threshod result)
	; keeps the maximum search depth as 31. i.e. no more than 31 moves
	(cond ((<= gn 31)
	; stops when the goal state is reached
	(cond ((not (= (MANHATTAN lst 0) 0))		
		; use a different result format to spcify the end of an iteration
		; without finding the successful results
		(cond ((null movable) (append (list 10) (list gn) ))
		; (cond ((null (cdr movable)) (append (list 10) result))
	(t 
		(let ((newlst (MAKEMOVE lst blankpos (car movable))))
		; obtain the f(n) value and check with the threshod value
		; stops dfs if the threshod value is exceeded
		(let ((fn (+ gn (MANHATTAN newlst 0))))
		(cond ((<= fn threshod) 
			(let ((newblankpos (GETBLANKPOS newlst 0)))
			(let ((newmovable (REMOVEMOVABLE (MOVABLETILES newlst newblankpos) blankpos)))
			(let ((newresult (ITERATION newlst newblankpos newmovable (+ gn 1) threshod 
									(append result (list (nth (car movable) lst))))))
			(cond ((and (= (car newresult) 10) (< (cadr newresult) 31))
					(ITERATION lst blankpos (cdr movable) gn threshod result))
			; (cond ((and (= (car newresult) 10) (>= (cadr newresult) 31))
					; (print newresult))
				(t newresult))
			)))
			)
		(t (ITERATION lst blankpos (cdr movable) gn threshod result))
		)
	)))))
	(t result)) 
	)
	(t result))
)

; IDAHELPER takes two arguments, a list representing the state of the tiles, and
; a number representing the threshod. It starts ITERATION and pass in the corresponding
; threshod values and checks whether the returned result is the moves leading to the
; goal state or new iteration needs to be called with updated threshod value.
(defun IDAHELPER (lst threshod)
	(let ((blankpos (GETBLANKPOS lst 0)))
	(let ((movable (MOVABLETILES lst blankpos)))
	(let ((result (ITERATION lst blankpos movable 0 threshod nil)))
		(cond ((null result) "fail")
		(t (cond ((= (car result) 10)
			(cond ((< (cadr result) 31)
				(IDAHELPER lst (+ threshod 2)))
				(t "fail")))
			(t result))))
	)))
)

; IDA* takes in a single argument, a list representing the state of the tiles, and returns
; either a list of moves that can reach the goal state or "fail" if the imput state cannot
; reach the goal state.
(defun IDA* (lst)
	(cond ((not (VALID lst)) "fail")
	(t	(let ((threshod (MANHATTAN lst 0)))
	(IDAHELPER lst threshod)
	)))
)