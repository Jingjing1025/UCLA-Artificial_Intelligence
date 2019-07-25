; Programming Assignment 9
; Jingjing Nie, 304567417

; Defconstant VARIABLES and CLAUSEPRODS used to test the production system interpreter.
(defconstant VARIABLES '(X Y Z U V))

(defconstant CLAUSEPRODS
	'(
	((E X Y)(A (I X Y) (I Y X)))
	((E X Y Z)(A (E X Y) (E X Z)))

	((I X Y)(O (N X) Y))

	((N (N X)) X)
	((N (O X Y))(A (N X) (N Y)))
	((N (A X Y))(O (N X) (N Y)))
	((N (O X Y Z))(A (N X) (N Y) (N Z)))
	((N (A X Y Z))(O (N X) (N Y) (N Z)))	

	((O (A X Y) Z) (A (O X Z) (O Y Z)))
	((O X (A Y Z)) (A (O X Y) (O X Z)))	
	((O (A X Y Z) U) (A (O X U) (O Y U) (O Z U)))
	((O X (A Y Z U)) (A (O X Y) (O X Z) (O X U)))
	
	((O (A X Y) Z U) (O (O (A X Y) Z) U))
	((O X (A Y Z) U) (O (O X (A Y Z)) U))
	((O X Y (A Z U)) (O (O X Y) (A Z U)))

	((O (A X Y Z) U V) (O (O (A X Y Z) U) V))
	((O X (A Y Z U) V) (O (O X (A Y Z U)) V))
	((O X Y (A Z U V)) (O (O X Y) (A Z U V)))
	)
)

; ISVARIABLE takes in two arguments. The first one is an atom, and the second one is a list
; representing all variables defined. It then checks whether the atom is a variable, and 
; returns T if it is, and Nil if it is not.
(defun ISVARIABLE (a vars)
	(cond ((null vars) nil)
	(t (cond ((equal a (car vars)) T)
		(t (ISVARIABLE a (cdr vars))))))
)

; GETNUM takes in two arguments. The first one is a list representing the matched variables
; found, and the second one is used to count the number of second level elements in the list.
; It then returns the number after traversing through the list and the number represents the
; number of elements found that can be matched to the variables in the production list.
(defun GETNUM (lst num)
	(cond ((null lst) num)
	(t (GETNUM (cdr lst) (+ num 1)))
	)
)

; GETNUMVAR takes in three arguments. The first one is a list representing the left hand side,
; or the rules of the production list. The second one is a list representing all variables 
; defined. The last one is a number used to count the number of variables present in the list.
; It then traverses through the list and returns that number.
(defun GETNUMVAR (lst vars num)
	(cond ((null lst) num)
	(t (cond ((atom (car lst))
			(cond ((ISVARIABLE (car lst) vars)
				(GETNUMVAR (cdr lst) vars (+ num 1)))
			(t (GETNUMVAR (cdr lst) vars num)))
			)
		(t (let ((result (GETNUMVAR (car lst) vars 0)))
			(GETNUMVAR (cdr lst) vars (+ num result))
			)
		)
	))
	)
)

; GETPOSITION takes in three arguments. The first one is an atom representing the variable, and
; the second one is a list representing all variables. The last one is a number used to count the
; index of that variable in the variable list. It then traverses through the variable list and
; return the index number.
(defun GETPOSITION (a lst num)
	(cond ((null lst) num)
	(t (cond ((equal a (car lst)) num)
		(t (GETPOSITION a (cdr lst) (+ num 1)))))
	)
)

; GETITEM takes in two arguments. The first one is a list represening the matched pairs found
; in the working memory that can match to the variables in the left hand side of the production 
; list, and the second one is a number used to check whether the current item needs to be returned.
; It then traverses through the list and return the item specified by the value num. 
(defun GETITEM (lst num)
	(cond ((null lst) nil)
	((= num 1) (car lst))
	(t (GETITEM (cdr lst) (- num 1)))
	)
)

; PLUGIN takes in five arguments. The first three arguments are lists or atoms that will be
; plugged in the action list to form new states in the working memory. The forth one is a 
; list representing the right hand side of the production list, which the variables need to 
; be plugged into. The last one is a list representing the new state in the working memory. 
; It then exchange the variables in the production list with the inputs and return a list
; representing the new state.
(defun PLUGIN (varlist prod newprod)
	(cond ((null prod) newprod)
		((atom prod)
			(cond ((ISVARIABLE prod VARIABLES) (GETITEM varlist (GETPOSITION prod VARIABLES 1)))
			(t prod))
		)
		(t (cond ((atom (car prod))
				(cond ((ISVARIABLE (car prod) VARIABLES)
					(let ((var (GETITEM varlist (GETPOSITION (car prod) VARIABLES 1))))
					(PLUGIN varlist (cdr prod) (append newprod (list var)) ))	
					) 
				(t 
					(let ((tmp (append newprod (list (car prod)))))
					(PLUGIN varlist (cdr prod) (append newprod (list (car prod))) ))))
					)
			(t 
			(let ((result (PLUGIN varlist (car prod) '() )))
			(PLUGIN varlist (cdr prod) (append newprod (list result)) )
			)
		)))
	)

)

; MATCHCHECK takes in two arguments. The first one is a list representing the matched elements
; for the variables, and the second one is the left hand side or the ruls in the production list.
; It then checks whether all variables in the rule list have been paired, and returns the matched
; pairs if all variables are paired, and Nil if not.
(defun MATCHCHECK (matched lhs)
	(cond ((equal (GETNUM matched 0) (GETNUMVAR lhs VARIABLES 0))
		matched)
	(t nil))
)

; MATCH takes in three arguments. The first one is a list representing the current state in the
; working memory, and the second one is a list representing the left hand side or the ruls in 
; the production list. The last one is used to store the elements found in the list, which can 
; pair to the variables in the rule list. It then traverses through the list and returns the list
; of matched elements for variables.
(defun MATCH (lst clause varlist)	
	(cond ((and (null lst) (null clause)) varlist)
		((and (null lst) (not (null clause))) varlist)
		((and (not (null lst)) (null clause)) nil)
		(t (cond ((atom (car clause))
				(cond ((ISVARIABLE (car clause) VARIABLES)
					(MATCH (cdr lst) (cdr clause) (append varlist (list (car lst)))))
				(t (cond ((not (equal (car lst) (car clause))) nil)
					(t (MATCH (cdr lst) (cdr clause) varlist))
				))
				)) 
			(t (cond ((atom (car lst)) nil)
				(t (let ((result (MATCH (car lst) (car clause) '() )))
					(cond ((null result) nil)
					(t (MATCH (cdr lst) (cdr clause) (append varlist result)))
					))
				))
			))
		)
	)
)

; HELPER takes in three arguments. The first one is a list representing the current state in the
; working memory, and the second one is a list representing the left hand side or the ruls in 
; the production list. The last one is used to store the part of list that can match with the 
; rule list. It then returns that part if it is found, or Nil if it is not found.
(defun HELPER (lst lhs matched)
	(cond ((null lst) matched)
	(t (cond ((atom (car lst))
			(cond ((equal (car lst) (car lhs))
				(let ((findvar (MATCH lst lhs '() )))
				(cond ((null findvar)
					(HELPER (cdr lst) lhs '() ))
				(t lst))
				))
			(t (HELPER (cdr lst) lhs matched))))
		(t (let ((result (HELPER (car lst) lhs '() )))
			(let ((findvar (MATCH result lhs '() )))
			(cond ((null findvar)
				(HELPER (cdr lst) lhs '() ))
			(t result))
			)))
		))
	)
)

; MERGEEXP takes in four arguments. The first one is a lisp representatino of a working
; memory. The second one is the part of that lisp expression which matches with the rules
; in the production list. The third one is a list representing the new expression that
; stands for the updated version of that part. The last one is used to store the merged lisp
; expression which merge the unchanged part of the old state and the updated part together.
; It then returns the merged expression representing the new state in working memory.
(defun MERGEEXP (exp partexp newexp mergedexp)
	(cond ((null exp) mergedexp)
	(t (cond ((equal (car exp) partexp)
		(MERGEEXP (cdr exp) partexp newexp (append mergedexp (list newexp))))
		(t (cond ((atom (car exp))
				(MERGEEXP (cdr exp) partexp newexp (append mergedexp (list (car exp)))))
			(t (let ((result (MERGEEXP (car exp) partexp newexp '() )))
				(MERGEEXP (cdr exp) partexp newexp (append mergedexp (list result))))
			)))
		))
	)
)

; INTERPRETAUX takes in three arguments. The first one is a lisp representatino of a working
; memory. The second one is a list representing a list of productions, and the last one is 
; used to store the original version of the production list such that all states after actions 
; will be checked against this original list. It then returns the new working memory formed
; after experiencing all the actions in the production list.
(defun INTERPRETAUX (EXP PRODS ORIGINPRODS)
	(cond ((null PRODS) EXP)
	(t (let ((production (car PRODS)))
		(let ((partexp (HELPER EXP (car production) '() )))
		(cond ((null partexp) 
			(INTERPRETAUX EXP (cdr PRODS) ORIGINPRODS))
		(t (let ((matchresult (MATCHCHECK (MATCH partexp (car production) '() ) (car production) )))
			(cond ((null matchresult)
				(INTERPRETAUX EXP (cdr PRODS) ORIGINPRODS))
			(t (let ((newexp (PLUGIN matchresult (cadr production) '() )))
				(cond ((equal partexp EXP)
					(INTERPRETAUX newexp ORIGINPRODS ORIGINPRODS))
				(t (INTERPRETAUX (MERGEEXP EXP partexp newexp '() ) ORIGINPRODS ORIGINPRODS)))
				)
			)
			))
		))
		))
	)
	)
)

; INTERPRET takes in two arguments. The first one is a lisp representatino of a working memory. 
; The second one is a list representing a list of productions. It then starts running the production
; system interpreter by calling INTERPRETAUX, and returns the new working memory formed after going 
; through all the actions in the production list.
(defun INTERPRET (EXP PRODS)
	(INTERPRETAUX EXP PRODS PRODS)
)
