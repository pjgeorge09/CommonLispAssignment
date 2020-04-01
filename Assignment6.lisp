; Peter George
; CMSC403 ASGN6
; Created : 03/30/2020

; Write a Common Lisp function named myList which creates the following list and returns it
; (4 (7 22) "art" ("math" (8) 99) 100)
(defun myList () "Makes the particular list"
    (list 4 '(7 22) "art" '("math" (8) 99) 100)
)

; Write a Common Lisp function named leapYear which takes no parameters and returns an ordered list containing all leap years from 1800 
; though 2020. The list of leapyears must be calculated, no points will be given for a hard-coded list.
;; Optional @Param low = starting year.       Optional @Param L is the list being recursively created
(defun leapYear (&optional (low 1800) (L '()))
    (cond ((<= 2021 low) L)
	((and (= (mod low 4) 0)  (= (mod low 100) 0)   (= (mod low 400) 0)   )  (leapYear (+ 4 low) (append L (list low))))
	((and (= (mod low 4) 0)  (= (mod low 100) 0)   (/= (mod low 400) 0)   )  (leapYear (+ 4 low) L))
	((and (= (mod low 4) 0)  (/= (mod low 100) 0)   (/= (mod low 400) 0)   )  (leapYear (+ 4 low) (append L (list low))))
	(T (leapYear (+ 4 low) L))
	)
)

; Write a Common Lisp function named union- which takes two list parameters. union- returns a single list which contains the separate 
; unique entities from both lists, with no duplication. You are not allowed to use the predefined union function for this function.  
(defun union- (a b)
	;; Remove duplicated method is in common-lisp, utilized here. union is just removing duplicates
	(remove-duplicates (append a b))
)

;; Write a Common Lisp function named avg with a single parameter named aList. avg returns the average of all elements in aList. 
;; Assume all elements in aList are numbers. If given an empty list, avg should return NIL. The avg function must be tail recursive.
;; Optional @Param result is the running sum.    @Param counter is how many loops/recursions, therefore length.
(defun avg (a &optional (result 0) (counter 0 ))
	;; If the list length is 0
	(if (= (list-length a) 0 )
		(/ result counter)									;; return the sum divided by the count on true
		(avg (cdr a) (+ result (car a) )  (+ counter 1)   ) ;; tail-recursively call avg
	)
)

;; Write a Common Lisp function named isType which takes a single parameter named dataType. isType will return an anonymous 
;; function which takes a single parameter and returns true if the parameters data type is the data type specified in dataType. Otherwise the 
;; anonymous function returns false. 
(defun isType (dataType)
	;; For a given "dataType", return true if it's datatype matches.
	(lambda (x) (typep x dataType))
)

;; Write a Common Lisp function named taxCalculator with three parameters: limit, rate, and values. limit and rate will be numbers, 
;; values will be a list. taxCalculator returns a list with the same elements and ordering of the values parameter EXCEPT every element which is 
;; greater than limit is multiplied by rate. Assume that all elements of the values list are numbers. 
;; BONUS: Make taxCalculator tail recursive +5 points
(defun taxCalculator (limit rate values &optional (L '())   )
	(cond 
		;; if empty list, return list (base-case)
		(  (= (list-length values) 0) L)
		;; If the first value is above the limit, recursively call TaxCalc with modified first value (Rate x value)
		(  (> (first values) limit)  (taxCalculator limit rate (cdr values) (append L(list (* (first values) rate)  ) ) )  )
		;; If the first value is not above limit, recursively call TaxCalc with unmodified first value
		( T  (taxCalculator limit rate (cdr values)  (append L(list (first values) ))    )      )	
	)
)

;; Write a Common Lisp function named clean which takes two parameters: aFunc and aList. aFunc will be a function and aList a list. 
;; aFunc is expected to be a function which takes a single parameter and returns a boolean value. clean will return a list which contains all values 
;; in aList which, when passed to aFunc, return true. if aList contains sublists, clean should create a new sublist with only the values which return 
;; true when passed to aFunc
(defun clean (aFunc aList &optional (L '()))
	(cond 
		;; If the end of the list, return the list (Base Case)
		((= (list-length aList) 0)         L)
		;; If the list is a sublist, recursively call clean on the sublist, and call clean on everything after the sublist
		((listp (first aList) )            (append L      (list(clean aFunc (first aList)))     (clean aFunc (cdr aList))   ))
		;; If the first item passes aFunc , call clean on the rest of the list with the first item appended to L
		((funcall aFunc (first aList))     (clean aFunc (cdr aList) (append L(list (first aList) ) ) )  )
		;; If the item fails aFunc, just skip it.
		(T                                 (clean aFunc (cdr aList) L)   )      	
	)
)

;; Define a Common Lisp macro named threeWayBranch which takes three parameters, x y and toExecute. x and y will be numbers and 
;; toExecute a list with three sublists. The threeWayBranch macro will execute statements in toExecute’s first sublist if x < y, the second sublist 
;; if x > y, and the third sublist if x = y. Assume each of toExecute’s sublists contain an arbitrary number of statements. 

(defmacro threeWayBranch (x y toExecute)
	;; Create a condition that upon that condition, creates and executes it's own "submacro"
	(cond
		;; if x<y then use progn on the first sublist, evaluated.
		((< x y) `(if ,(< x y) (progn ,@(first toExecute))))
		;; if x>y then use progn on the second sublist, evaluated.
		((> x y) `(if ,(> x y) (progn ,@(second toExecute))))
		;; if x=y then use progn on the third sublist, evaluated.
		(T `(if ,(= x y) (progn ,@(third toExecute))))
	)
	;; Heavily relied on PP19 (Some quotes below)
	;; The ,@ causes the subexpression to be evaluated and the resulting list value spliced into the expression
	;; The ` operator behaves as the ' operator except you can unquote particular subexpressions by preceding them with a comma. 
)
;; For copy-pasting
;; (load "Assignment6Test.lisp")