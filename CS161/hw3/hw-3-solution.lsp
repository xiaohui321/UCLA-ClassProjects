; CS 161 Spring 2013: HW3 skeleton

(load "hw-1-solution.lsp")

; FUNCTION: UNIFRAME
; PURPOSE:  Perform unification over the two inputs, based on the pseudocode
;           from Fig 9.1, p.328 of the textbook. Description also available
;           online at http://aima.cs.berkeley.edu/algorithms.pdf, page 23.
;           Each input is either a variable [(V var) list], frame, or list of
;           frames. As noted in the homework PDF and discussion #6, semantics
;           of frame unification are slightly different, so your code will not
;           match the pseudocode exactly. 
; OUTPUT:   A binding-list, of the form
;           (T (atom_1 binding_1) (atom_2 binding_2) ...),
;           where each atom_i is a *variable name* (as in, the second part of a
;           filler that looks like "(V name)") and each binding is a frame, 
;           (variable value) pair, or gap.
; INPUTS:   XX: variable (V var), frame, or list of frames
;           YY: variable (V var), frame, or list of frames
;           feta: a list of bindings, same format as output
;           (where (T) represents an initial empty binding list)

(defun UNIFRAME (XX YY feta)
	(cond
		; failure detection
		((null feta) nil)
		; check if XX and YY are already equal; if frames, use frame-compare, else ordinary
		((robust-equal XX YY) feta)
		; check if either arg is a variable
		((variable? XX) (UNIVAR XX YY feta))
		((variable? YY) (UNIVAR YY XX feta))
		; if both frames, then check predicates and unify slot-fillers
		((and (frame? XX) (frame? YY)) (UNISF (rest XX) YY (UNIFRAME (first XX) (first YY) feta)))
		; if lists, see if first element of XX can unify with anything in YY
		((and (frame-list? XX) (frame-list? YY))
			(if (null XX) feta ; BASE CASE: we checked everything in the list XX already
				(let* ((result (UNILIST (first XX) YY feta)))
					; result is the list (new-feta, YY-frame), where YY-frame is the frame that
					; was used for the binding to XX; remove it for the recursive call
					(UNIFRAME (rest XX) (remove (second result) YY) (first result))
				)
			)
		)
		; fall-through: failure
		(t nil)
	)
)

; helper function for UNIFRAME; attempt to unify each slot-filler in the two frames, although
; it is OK if fr2 has extra frames
; (first argument is a slot-filler list; second is a frame)
(defun UNISF (sf1 fr2 feta)
	(cond
		; failed somewhere, so terminate
		((null feta) nil)
		; ran out of slots to check from sf1, so we're good!
		((null sf1) feta)
		; try to unify first slot in slot-filler list
		(t (let*  ((slot (first (first sf1))) 
				   (firstfiller (second (first sf1)))
				   ; actually attempt the unification
				   (binding (UNIFRAME firstfiller (filler slot fr2) feta))
				  )
				; now recurse on the rest of the sf list
				(UNISF (rest sf1) (rm-slot slot fr2) binding)
			)
		)
	)
)

; helper function for UNIFRAME; attempt to unify a single frame with a list of frames YY
; (any element of YY will do). Returns the list (new-feta, YY-frame), where YY-frame is the frame that
;  was used for the binding to XX;
(defun UNILIST (frame YY feta)
	(cond
		; ran out of frames; failure
		((null YY) nil)
		; else, check whether we can bind with the first frame
		(t (let* ((binding (UNIFRAME frame (first YY) feta)))
					(if (null binding)
						 ; We couldn't, but keep trying...
					     (UNILIST frame (rest YY) feta)
						 ; else, we did find a binding; return it!
						 (list binding (first YY))
					)
			)
		)
	)
)

(defun robust-equal (x y)
	(if (and (frame? x) (frame? y))
	    (COMPARE-FRMS x y) ; if frames, do frame comparison
	    (equal x y) ; else, ordinary comparison
	)
)

; Some type-checking utility functions
; Variable is a list starting with symbol V
(defun variable? (x)
	(if (listp x) (equal (first x) 'V) nil)
)

; Frame is a list starting with an atom that is not V
(defun frame? (x)
	(and (listp x) (atom (first x)) (not (equal (first x) 'V)))
)

; Frame list is a list starting with a frame
(defun frame-list? (x)
	(and (listp x) (frame? (first x)))
)

; -----------------------------------------------------------------------------

; FUNCTION: UNIVAR
; PURPOSE:  Helper function for UNIFRAME. See pseudo-code from the textbook
;           algorithm UNIFY-VAR, from the same page as UNIFY referenced in
;           UNIFRAME. You should not implement or call OCCUR-CHECK?
; OUTPUT:   A binding-list (feta)
; INPUTS:   var: a variable (list formatted as (V var))
;           x: frame, variable, or frame list, as in UNIFRAME
;           feta: a binding list, same format as in UNIFRAME

(defun UNIVAR (var x feta)
	(let* ((var-binding (get-binding (second var) feta))
	       (x-binding (get-binding (second x) feta))
		  )
		(cond
			(var-binding (UNIFRAME var-binding x feta))
			(x-binding (UNIFRAME var x-binding feta))
			(t (append feta (list (list (second var) x))))
		)
	)
)

; Helper for UNIVAR; check a feta to see if it has a binding for that var, and if so
; return it
(defun get-binding (x feta)
	(second (first (member x feta :test #'check-binding)))
)

; test function for get-binding
(defun check-binding (x y)
	(and (listp y) (equal x (first y)))
)

; -----------------------------------------------------------------------------

; FUNCTION: FFINFER
; PURPOSE:  Perform forward chaining, using a list of facts and rules (see INPUT).
;           Recursively build up a list of new facts derivable from this
;           knowledge base as NEW (see OUTPUT). See also FOL-FC-ASK from the text
;           (figure 9.3 on p332, online at http://aima.cs.berkeley.edu/algorithms.pdf, page 24)
;           although there are differences (see homework PDF)
; OUTPUT:   List of frames
; INPUTS:   FACTS: list of initial facts (list of instantiated frames, i.e.
;           frames containing no variables)
;           RULES: list of rules, with the following format:
;           ( (r_1-premise_1 r_1-premise_2 ... r_1-premise_n_1 ===> r_1-conclusion)
;             (r_2-premise_1 r_2-premise_2 ... r_2-premise_n_2 ===> r_2-conclusion)
;             ...
;           )
;           That is, for each rule, the second-to-last element is always a literal
;           "===>", signifying logical implication, the last element is the 
;           consequent of the implication, and the premises are the antecedants of
;           the implication (assumed to be conjoined). So eg:
;           ( (MONARCH (IS (V Q))) (FEMALE (IS (V Q))) ===> (QUEEN (IS (V Q))) ) is a
;           list of the single rule in first order logic
;           "forall Q (Monarch(Q) & Female(Q)) => Queen(Q)"
;           NEW: list of instantiated frames (containing no variables), to be
;           built up in recursive calls by applying premises to the current knowledge
;           base, and returned as output

; Most of the work done by FORWARD-CHAIN
(defun FFINFER (facts rules new)
	(only-new-facts facts (FORWARD-CHAIN facts (standardize-rules rules)))
)

; Helper for FFINFER to help it return only the new facts, by removing any facts that
; were already in the original fact list
(defun only-new-facts (original final)
	(loop for fact in final append
		; check if fact is in the original list (use "equal" test since default
		; can't handle lists as members)
		(if (member fact original :test #'equal) nil ; was in original, discard
			(list fact) ; else, keep it
		)
	)
)

; Helper for FFINFER to standardize all rules
(defun standardize-rules (rules)
	(loop for rule in rules append
		(list (standardize-vvars rule))
	)
)

; Main forward chaining function
(defun FORWARD-CHAIN (facts rules)
	(let* ((new (loop for rule in rules append
						(unify-rule facts rule)
					  )
		  ))
		  ; if nothing new, return full fact list
		(if (null new) facts
			; else, recurse with new
			(FORWARD-CHAIN (append facts new) rules)
		)
    )
)

; Construct a predicate filter (a function!) by extracting the list of predicates
; occuring in the premises list. The returned function will return true if it receives
; an element whose predicate occurs in that extracted list.
(defun pred-filter (premises)
	; extract predicate list from premises
	(let* ((preds (loop for p in premises append (subseq p 0 1))))
		; create a function that checks whether the input frame's predicate matches
		; something in the extracted list preds
		(lambda (frame) (member (first frame) preds))
	)
)

; Helper function to unify the fact list with the premises of a rule
(defun unify-rule (facts rule)
	(let* ((premises (subseq rule 0 (- (length rule) 2)))
	       (conclusion (first (last rule)))
		   ; get list of relevant facts, i.e. ones containing a predicate in premises
		   (relevant-facts (member-if (pred-filter premises) facts)) 
		   (feta (UNIFRAME premises relevant-facts '(T)))
		   (new (subst-feta conclusion feta))
		   (new-facts (cons new facts))
		  )
		(cond
			; Failed to unify
			((null feta) nil)
			; We did unify... but we didn't learn anything new
			((member new facts :test #'compare-frms) nil)
			; Else, make a note here: great success! See if we can't learn something
			; else new, by removing some of the relevant facts...
			(t (cons new (loop for rf in relevant-facts append
							; Try recursing with a single relevant fact removed, but remembering
							; the things we already derived (NB: assumes we won't have a conclusion
							; with the same predicate as one of the premises)
							(let* ((result (unify-rule (remove rf new-facts :test #'equal) rule)))
								; If we succeed, update facts list
								(if (null result) nil
									(progn (setf new-facts (append result new-facts))
										   result
									)
								)
							)
						)
				)
			)
		)
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: STANDARDIZE-VVARS
; PURPOSE:  Helper function for FFINFER. Takes a RULE formatted as in FFINFER
;           and replace all its variables with uniquely named ones. Eg,
;           (V X) should become (V X1234), or whatever the output of UNIQUE-GAP
;           happens to be).
; OUTPUT:   Rule with uniquely named variables
; INPUTS:   Rule to standardize, format is:
;           (premise-1 premise-2 ... premise-n ===> conclusion)

(defun UNIQUE-VAR (symName)
	
	; Increment gensym counter to ensure returned symbol will print with 
	; unique name
	(setq *gensym-counter* (1+ *gensym-counter*))
	
	; Generate the new symbol using gensym
	(intern (string (gensym (string symName))))
)

(defun STANDARDIZE-VVARS (rule)
	(let* ((newnames nil))
		; standardize each frame, but remember any new names we generate
		(loop for frame in rule append
			; wrap in a list to maintain rule format
			(if (equal frame '===>) '(===>)
					; return format of STANDARDIZE-FRAME is (standardized frame, newnames)
					(let* ((result (STANDARDIZE-FRAME frame newnames)))
						(setf newnames (second result))
						(list (first result))
					)
			)
		)
	)
)

(defun STANDARDIZE-SLOTS (sf newnames)
	(cond
		; Base case: got through them all
		((null sf) (list nil newnames))
		; Recursive case: dispatch STANDARDIZE-FRAME on our first filler
		(t (let* ((result (STANDARDIZE-FRAME (second (first sf)) newnames)) ; dispatch STANDARDIZE-FRAME on the filler
		          (new-sf (list (first (first sf)) (first result))) ; generate our new sf-pair
				  (recurse-sf (STANDARDIZE-SLOTS (rest sf) (second result))) ; recurse on rest of sf
				 )
				 ; return list (current-sf-list current-newnames)
			(list (cons new-sf (first recurse-sf)) (second recurse-sf))
		   )
	    )
	)
)

; returns (frame newnames) list
(defun STANDARDIZE-FRAME (frame newnames)
	(cond
		; check if binding exists (we have an entry for the variable name in the '(V name) list)
		((variable? frame) (let* ((binding (get-binding (second frame) newnames)))
								  ; if we already have a binding, just return it
								 (if binding (list (list 'V binding) newnames)
								 ; else, need a new one
									(let* ((new-var (UNIQUE-VAR (second frame))))
										; return ((V new-var) (old-var new-var)+newnames), i.e. add binding for old-var to newnames
										(list (list 'V new-var) (cons (list (second frame) new-var) newnames))
									)
								 )
							)
		)
		; Base case: empty, or single pred frame
		((<= (length frame) 1) (list frame newnames))
		; Main case: dispatch STANDARDIZE-SLOTS on our slot-filler list
		(t (let* ((result (STANDARDIZE-SLOTS (rest frame) newnames)))
				; return
		        (list (cons (first frame) (first result)) (second result))
		   )
	    )
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: SUBST-FETA
; PURPOSE:  Replace all variables in the given frame with their bindings given
;           in the binding-list feta.
; OUTPUT:   Instantiated frame, where variables have been replaced
; INPUTS:   frame: frame to replace variables in
;           feta: binding list, of the form from UNIFRAME
;           (T (var1 binding1) (var2 binding2) ...)

(defun SUBST-FETA (frame feta)
	(cond
		; Base case: got a variable, so replace it with its binding, OR just the frame if no binding
		((variable? frame) (or (get-binding (second frame) feta) frame))
		; Base case: empty, or single pred frame
		((<= (length frame) 1) frame)
		; Main case: dispatch SUBST-SLOTS on our slot-filler list
		(t (cons (first frame) (SUBST-SLOTS (rest frame) feta)))
	)
)

; Helper for SUBST-FETA, goes through the sf-list replacing variables with bindings from feta
(defun SUBST-SLOTS (sf feta)
	(cond
		; Base case: got through them all
		((null sf) nil)
		; Recursive case: dispatch SUBST-FETA on our first filler
		(t (cons (list (first (first sf))             ; rebuild our first slot-filler pair
		               (SUBST-FETA (second (first sf)) feta)) ; dispatch SUBST-FETA on the filler
		               (SUBST-SLOTS (rest sf) feta)))         ; recurse on rest of sf
	)
)
