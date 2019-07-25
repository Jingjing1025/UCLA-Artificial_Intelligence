; Programming Assignment 8
; Jingjing Nie, 304567417

; CONTAIN takes in two arguments, a list representing the literal or clause, and a list 
; representing the clause or resolution. It then returns T if the lst cotains the literal
; or clause, or NIL if it does not contain the literal or clause.
(defun CONTAIN (c1 lst)
	(cond ((null lst) nil)
	(t (let ((c2 (car lst)))
		(cond ((equal c1 c2) T)
		(t (CONTAIN c1 (cdr lst))))
		))
	)
)

; FINDPROPOSITION takes two arguments, a list representing the proposition, and a list representing
; the resolution. It then checks if the resolution has the negation of the proposition, and
; returns 'POS if the negation found is a proposition, or 'NEG if the negation found is a
; negated proposition.
(defun FINDPROPOSITION (proposition resolution)
	(cond ((null resolution) NIL)
	(t (cond ((atom (car resolution))
		(cond ((equal proposition (car resolution)) '(pos) )
			((and (equal (car resolution) 'not) (equal proposition (cadr resolution))) '(neg) )
			(t (FINDPROPOSITION proposition (cdr resolution)))))
		(t (let ((result (FINDPROPOSITION proposition (car resolution))))
			(cond ((null result) (FINDPROPOSITION proposition (cdr resolution)))
			(t 
				(append result (FINDPROPOSITION proposition (cdr resolution)))))))
	)))
)

; REMOVELITERAL takes in three arguments. The first one is a list representing the literal, 
; and the second one is a list representing the clause. The third one is a list used to record
; the current solution, which is the clause with the passed-in literal removed. It then finds
; the literal in the clause and removes it from the clause.
(defun REMOVELITERAL (literal clause solution)
	(cond ((null clause) solution)
	(t (cond ((not (equal literal (car clause)))
		(REMOVELITERAL literal (cdr clause) (append solution (list (car clause)))))
		(t (REMOVELITERAL literal (cdr clause) solution)))
	))
)

; HASCOMPLEMENT takes in two arguments. The first one is a list representing the literal, 
; and the second one is a list representing the clause. It then returns either T, when the
; clause contains the negation of the literal, or NIL, when it does not contain the negation
; of the literal. When there is the negation of the literal in the clause, the negated literal
; is also returned together with the atom T.
(defun HASCOMPLEMENT (literal clause)
	(cond ((null clause) nil)
	(t
		(cond ((and (not (atom literal))
				(CONTAIN 'pos (FINDPROPOSITION (cadr literal) clause)))
				(list T (cadr literal)))
			((and (atom literal)
				(CONTAIN 'neg (FINDPROPOSITION literal clause))) 
				(list T (list 'not literal)))
			(t (HASCOMPLEMENT literal (cdr clause))	
		))
	))
)

; REMOVEDUP takes in two arguments, a list that will have its duplicates removed, and a list
; used to store the new unique lists. It then traverses through the list and returns the new
; list with no duplicate elements in it.
(defun REMOVEDUP (lst unique)
	(cond ((null lst) unique)
	(t (cond ((REMOVEDUPAUX (car lst) unique)
			(REMOVEDUP (cdr lst) unique))
		(t (REMOVEDUP (cdr lst) (append unique (list (car lst))) ))
	))
	)
)

; REMOVEDUPAUX takes in two arguments, an element and a list. It then searches through the
; to check if the element is present in the list, and returns T if yes, and Nil if not.
(defun REMOVEDUPAUX (a lst)
	(cond ((null lst) nil)
	(t (cond ((equal a (car lst)) T)
		(t (REMOVEDUPAUX a (cdr lst)))))
	)
)

; GETCOMPLEMENT takes in three arguments. The first one is a list representing the clause.
; The second one is a list representing the resolution, and the last one is a list used to
; store the current list of clauses found which contain the literal. It then checks through
; the resolution and returns the list of clauses that contain the literal
(defun GETCOMPLEMENT (clause lst complist)
	(cond ((null clause) (REMOVEDUP complist '()))
	(t (let ((result (HASCOMPLEMENT (car clause) lst)))
		(cond ((equal (car result) T)
			(let ((tmp (cadr result)))
			(let ((comp (GETCOMPLEMENTAUX tmp lst)))
			(GETCOMPLEMENT (cdr clause) lst (append complist comp))
			)))
		(t (GETCOMPLEMENT (cdr clause) lst complist))
		)
		))
	)
)

; GETCOMPLEMENTAUX takes in two arguments, a list representing the literal, and a list that
; represents the resolution. It then checks whether the resolution contians clauses that has
; the literal, and then returns all the clauses that has the literal.
(defun GETCOMPLEMENTAUX (literal lst)
	(cond ((null lst) nil)
	(t (let ((clause (car lst)))
		(cond ((CONTAIN literal clause) 
			(append (GETCOMPLEMENTAUX literal (cdr lst)) (list clause) ))
		(t (GETCOMPLEMENTAUX literal (cdr lst)))
		)
		))
	)
)

; UNIFYRESULTS takes in two arguments, a list representing the new result found, and a list 
; representing the previously found resolution with its leaves. It then checks whether they
; have the same results, and then merge these two lists together.
(defun UNIFYRESULTS (result resolution)
	(cond ((null resolution)
		(append result resolution))
	(t (let ((clause1 (second result)))
		(let ((clause2 (third result)))
		(cond ((CONTAIN (car clause1) resolution) 
			(let ((newresult (REMOVELITERAL clause1 result '() )))
				(append newresult (list resolution))
			))
			((CONTAIN (car clause2) resolution) 
			(let ((newresult (REMOVELITERAL clause2 result '() )))
				(append newresult (list resolution))
			))
		(t (append result (list resolution)))
		)))
	))
)

; FINDSINGLELITERAL takes in two arguments, a list representing the clauses, and a list that
; represents the original list of clauses. It then checks whether the clause has complementary
; pairs in the original list, and then reutrns either T if there is, or Nil, if there isn't.
(defun FINDSINGLELITERAL (clause lst)
	(cond ((null clause) nil)
	(t (cond ((null (HASCOMPLEMENT (car clause) lst)) (car clause) T)
		(t (FINDSINGLELITERAL (cdr clause) lst )))
	))
)

; REMOVESINGLELITERAL takes in three arguments. The first one is a list of clauses, and the
; second one is the original list of clauses. The last one is used to store the new lists of
; clauses found with clauses which do not have complementary pairs removed. It then traverses
; through the original list and removes all clauses that do not have complementary pairs, and
; returns that new list of clauses.
(defun REMOVESINGLELITERAL (lst originlst newlst)
	(cond ((null lst) newlst)
	(t (let ((clause (car lst)))
		(cond ((FINDSINGLELITERAL clause originlst)
			(let ((currlist (REMOVELITERAL clause newlst '())))
			(REMOVESINGLELITERAL (cdr lst) originlst currlist)
			))
		(t (REMOVESINGLELITERAL (cdr lst) originlst newlst)))
		)
	))
)

; REFUTE takes in a single argument, a list representing the list of clauses. It then calls
; REFUTEAUX with an empty initial result to obtain the proof of the list of clauses. It then
; returns either the results found, or 'FAIL, if the proof cannot be found.
(defun REFUTE (lst)
	(REFUTEAUX (REMOVESINGLELITERAL lst lst lst) '())
)

; REFUTEAUX takes in two arguments, the first one is a list of clauses, and the second one is
; a list used to store the currently found result together with the children that lead to the 
; result. It then checks whether a proof has been found, and returns the final result if its 
; element is Nil, or keeps traversing through the clause list to find new pairs.
(defun REFUTEAUX (lst resolution)
	(cond ((null lst) 'fail)
	(t (let ((clause1 (car lst)))
		(let ((comp (GETCOMPLEMENT clause1 (cdr lst) '() )))
		(let ((result (REFUTEAUXAUX clause1 comp)))
		(cond ((null result) (REFUTEAUX (cdr lst) resolution))
		(t (cond ((null (car result)) 
			(UNIFYRESULTS result resolution))
			(t (REFUTEAUX (append (list (car result)) (cdr lst) ) (UNIFYRESULTS result resolution) )) )
			))
		)))
	))
)

; REFUTEAUXAUX takes in two arguments, a list representing the clause, and a list representing
; the list of its complementary clauses. It then calls REFUTEHELPER to find out the results.
(defun REFUTEAUXAUX (clause1 lst)
	(cond ((null lst) nil)
	(t (let ((clause2 (car lst)))
		(let ((result (REFUTEHELPER clause1 clause1 clause2)))
		(cond ((null result)
			(REFUTEAUXAUX clause1 (cdr lst)))
		(t result))
		))
	))
)

; REFUTEHELPER takes in three arguments. The first one is a list representing the original clause,
; the second one is a list used for traversals through clauses, and the last one is a list that
; represents another clause from its complementary pair that we are going to check with it. It then
; unifies the two clauses with complementary literals, and returnd the result together with the two
; children that lead to the result.
(defun REFUTEHELPER (clause1 tmpclause1 clause2)
	(cond ((null tmpclause1) nil)
	(t (let ((literal1 (car tmpclause1)))
		(let ((comp (HASCOMPLEMENT literal1 clause2)))
		(cond ((equal (car comp) T)
			(let ((literal2 (cadr comp)))
			(let ((resolution (append (REMOVELITERAL literal1 clause1 '())
				(REMOVELITERAL literal2 clause2 '()))))
			(list resolution (list clause1) (list clause2))
			)))
		(t (REFUTEHELPER clause1 (cdr tmpclause1) clause2))
			))
		))
	)
)
