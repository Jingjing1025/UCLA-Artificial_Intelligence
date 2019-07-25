; Programming Assignment 7
; Jingjing Nie, 304567417


; Representation of the state of the HANOI problem. 
; The state is marked by a series of letters representing the pegs, and since the sizes
; of the discs are different, the discs are arranged in decreasing order, with the index
; number representing their relative sizes.

; GETLENGTH takes in one argument, which is a list of letters representing the state
; of the HANOI problem, and then returns its length.
(defun GETLENGTH (lst)
	(cond ((null lst) 0)
		(t (+ 1 (GETLENGTH (cdr lst)))))
)

; GETFIRSTDIFF takes in three arguments. The first one is a a list of letters representing 
; the initial state of the HANOI problem, the second one, similarly, is a list of letters
; representing the final state of the HANOI problem, and the last one is a number used as 
; a counter. It then returns the first pair of different letters in these two states.
(defun GETFIRSTDIFF (initial final counter)
	(cond ((null initial) nil)
		(t (cond ((equal (car initial) (car final))
			(GETFIRSTDIFF (cdr initial) (cdr final) (- counter 1)))
			(t (append (list (car initial)) (list (car final)) (list counter)))
			))
	)
)

; GETSECOND takes in five arguments. The first one is a letter representing the peg the
; disc is placed on. The second one is a list of letters representing the state of the 
; HANOI problem. The third one is a number representing the index of the currently looking
; disc. The forth one is a number representing the index of the next disc on top of the
; current disc. The last one is a number used as a counter. It then returns the index of
; the next disc sitting on directly on top of the current disc.
(defun GETSECOND (val lst firstindex secondindex counter)
	(cond ((null lst) secondindex)
		(t (cond ((equal val (car lst))
				(cond ((or (<= firstindex counter) (> secondindex 0))
					(GETSECOND val (cdr lst) firstindex secondindex (- counter 1)))
				(t (GETSECOND val (cdr lst) firstindex counter (- counter 1)))))
			(t (GETSECOND val (cdr lst) firstindex secondindex (- counter 1))))
		)
	)
)
; (write (GETSECOND 'A '(A A A) 1 -1 3))

; GETTOP takes in four arguments. The first one is a letter representing the peg the
; disc is placed on. The second one is a list of letters representing the state of the 
; HANOI problem. The third one is a number representing the index of the disc that is
; placed on topmost level of this peg. The last one is a number used as a counter. It 
; then returns the index of the disc that is placed on topmost level of this peg.
(defun GETTOP (val lst top counter)
	(cond ((null lst) top)
		(t (cond ((equal val (car lst))
			(GETTOP val (cdr lst) counter (- counter 1)))
		(t (GETTOP val (cdr lst) top (- counter 1))))
	))
)
; (write (GETTOP 'B '(A B A A) -1 4))

; GETFIRSTSMALLER takes in five arguments. The first one is a letter representing the peg the
; disc is placed on. The second one is a number representing the index of the disc that wants
; to move to this peg. The third one a list of letters representing the state of the HANOI 
; problem. The forth one is a number representing the index of the largest disc on this peg 
; that is smaller than the disc that wants to be moved to this peg. The last one is a number
; used as a counter. It then returns the index of the largest disc on this peg that is smaller 
; than the disc that wants to be moved to this peg.
(defun GETFIRSTSMALLER (val index lst result counter)
	(cond ((null lst) -1)
		(t (cond ((equal val (car lst))
			(cond ((> counter index)
				(GETFIRSTSMALLER val index (cdr lst) result (- counter 1)))
				(t counter)))
			(t (GETFIRSTSMALLER val index (cdr lst) result (- counter 1)))
		))
	)
)	
; (write (GETFIRSTSMALLER 'B 1 '(A B C B A) -1 5))

; GETAUXTOWER takes in three arguments. The first two are two letters representing the two pegs
; that the disc is currently on and is going to be moved to. The third one is a list of letters
; representing the state of the HANOI problem. It then returns the auxillary peg, i.e., the peg
; other than these two.
(defun GETAUXTOWER (a b lst)
	(cond ((equal a (car lst)) (GETAUXTOWER a b (cdr lst)))
		((equal b (car lst)) (GETAUXTOWER a b (cdr lst)))			
		(t (car lst))
	)
)
; (write (GETAUXTOWER 'A 'C '(A B C)))

; MOVE takes in four arguments. The first one is a letter representing the peg that the disc is
; going to be moved to. The second one is the index of this disc. The third one is a list of letters
; representing the state of the HANOI problem. The last one is a number used as a counter. It then
; returns the new state adter this movement.
(defun MOVE (new index state counter)
	(cond ((null state) nil)
		(t  
			(cond ((= index counter) 
				(append (list new) (MOVE new index (cdr state) (- counter 1))))
				(t (append (list (car state)) (MOVE new index (cdr state) (- counter 1))))
			)	
		)
	)
)
; (write (MOVE 'B 2 '(A B C A) 4))

; TRY takes in six arguments. The first two are two letters representing the two pegs that the
; disc is currently on and is foing to be moved to. The third one is the index of the disc that
; wants to be moved. The forth one is a list of letters representing the state of the HANOI problem.
; The fifth one is a list representing the list of legal moves that can achieve the purpose of
; moving the disc to the goal state. The last one is a number used to store the number of discs.
; It then returns the new state after completeing the goal of moving disc of the index from peg
; X to peg Y, as well as a list of legal moves suggesting the solution for this purpose.
(defun TRY (X Y indX currstate solution N)
	(let ((indY (GETTOP Y currstate -1 N)))
	(cond ((= indY -1)
		(let ((nextX (GETSECOND X currstate indX -1 N)))
		(cond ((> nextX 0)
			(let ((result (TRY X (GETAUXTOWER X Y '(A B C)) nextX currstate solution N)))
				(TRY X Y indX (car result) (cdr result) N))
			)
		(t 
			(append (list (MOVE Y indX currstate N)) (append solution (list (list indX X Y))))) ))
		)
	(t 
		(let ((nextX (GETSECOND X currstate indX -1 N)))
		(let ((nextY (GETFIRSTSMALLER Y indX currstate -1 N)))
		(cond ((and (= nextX -1) (= nextY -1))
			(append (list (MOVE Y indX currstate N)) (append solution (list (list indX X Y))))
			)
		(t (cond ((> nextX nextY)
				(let ((result (TRY X (GETAUXTOWER X Y '(A B C)) nextX currstate solution N)))
					(TRY X Y indX (car result) (cdr result) N))
				)
			(t (let ((result (TRY Y (GETAUXTOWER X Y '(A B C)) nextY currstate solution N)))
					(TRY X Y indX (car result) (cdr result) N))
				)
			)
		))
		)) )
	))
)
; (write (TRY 'A 'C 5 '(A B C C B) nil 5))

; HANOIAUX takes in four arguments. The first one is a list of letters representing the initial 
; state of the HANOI problem. The second one is a list of letters representing the final state of 
; the HANOI problem. The third one is a list representing the list of legal moves that can achieve 
; the purpose of moving the discs such that the final state is reached from the initial state. 
; The last one is a number used to store the number of discs in this problem. It then returns
; a list of legal moves as the solutions that helps the initial state become the final state.
(defun HANOIAUX (initial final solution N)
	; (write initial)
	(cond ((equal initial final) solution)
		(t (let ((diff (GETFIRSTDIFF initial final N)))
			(let ((X (car diff)))
			(let ((Y (cadr diff)))
			(let ((index (third diff)))
				; (write diff)
			; (let ((tmpstate (GETTMPSTATE (- N index) initial)))
				; (write tmpstate)
			(let ((result (TRY X Y index initial solution N)))
			(HANOIAUX (car result) final (cdr result) N)
			)))))
		)
	)
)

; HANOI takes in two arguments. The first one is a list of letters representing the initial 
; ste of the HANOI problem. The second one is a list of letters representing the final state 
; of the HANOI problem. It then figures out the number of discs involved in this case, and 
; calls HANOIAUX with all the parameters to start searching for the solution. It then returns
; a list of legal moves as the solutions that helps the initial state become the final state.
(defun HANOI (initial final)
	(HANOIAUX initial final '() (GETLENGTH initial))
)


; TEST functions. No arguments are taken in, and the solution of the HANOI problem is returned.

(defun test1 ()
	(HANOI '(A B C A) '(C B A C))
)

(defun test2 ()
	(HANOI '(C B A) '(A B C))
)

(defun test3 ()
	(HANOI '(A A A) '(C C C))
)

(defun test4 ()
	(HANOI '(A B C C B) '(C B A A B))
)

(defun test5 ()
	(HANOI '(A B C A B C) '(A C B C B A))
)

(defun test6 ()
	(HANOI '(B A B A B A) '(A B A B A B))
)

(defun test7 ()
	(HANOI '(A B B C C C) '(A C C A B A))
)

(defun test8 ()
	(HANOI '(A A A A A A A)'(C C C C C C C))
)

(defun test9 ()
	(HANOI '(C C B B B A A A A) '(C C B B B A A A A))
)

(defun test10 ()
	(HANOI '() '())
)

(defun test11 ()
	(HANOI '(B B B A A A A) '(A C A C A C A))
)

(defun test12 ()
	(HANOI '(B C C B B A A A) '(C A B C B A B C))
)