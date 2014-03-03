;=============================
; CS 161 Spring 2013: HW4
; LAST  NAME: ZHOU
; FIRST NAME: XIAOHUI
;=============================

; FUNCTION: ANSWER
; PURPOSE:  Search through the passed-in episodic memory (list of frame atoms)
;           for a frame that can be UNIFRAME'd (homework #3) with the query
;           frame, by binding variables expressed in the query.
; OUTPUT:   If a frame that can be unified is found, substitute the corresponding
;           binding into the question frame, and return the result.
;           As a special case, if the predicate of the query is "ACT" (which corresponds
;           roughly to an English query "what did... do..."), then ANSWER should look
;           for frames in the episodic memory whose slots unify with the slots of
;           the query, with the exception of the "IS" slot in the query, which
;           can be ignored. It should return the result of unifying the found
;           frame with the query (leaving out the "IS" slot). For example, see
;           the Test-Case-2 in the homework PDF.
;           If no frame can be found, then return NIL.
; INPUTS:   QCON: query frame
;           EP-STMEM: list of frame atoms to search for answer in

(defun ANSWER (QCON EP-STMEM)
  (COND
     ((NULL EP-STMEM) NIL)
     ((CHECK QCON) (ANSWER-1 (REMAIN (REST QCON)) EP-STMEM))
     ((SETQ ANSFETA (UNIFRAME QCON (FIRST EP-STMEM) '(T)))  (SUBST-FETA (FIRST EP-STMEM) ANSFETA))
     (T (ANSWER QCON (REST EP-STMEM)))))

(defun ANSWER-1 (QCON EP-STMEM)
  (COND
     ((NULL EP-STMEM) NIL)
     ((SETQ ANSFETA (UNIFRAME (CONS (FIRST (FIRST EP-STMEM)) QCON) 
			      (FIRST EP-STMEM) '(T))) (SUBST-FETA (FIRST EP-STMEM) ANSFETA))
     (T (ANSWER-1 QCON (REST EP-STMEM)))))

(DEFUN REMAIN (QCON)
  (COND
   ((CHECK-IS+VAR (FIRST QCON)) (REST QCON))
   (T (CONS (FIRST QCON) (REMAIN (REST QCON))))))

(DEFUN CHECK (QCON)
  (COND 
     ((NULL QCON)                     NIL)
     ((AND (EQUAL (FIRST QCON)'ACT) 
	   (CHECKIS (REST QCON)))      T)
     (T                               NIL)))

(DEFUN CHECKIS (QCON)
  (COND 
     ((NULL QCON) NIL)
     ((CHECK-IS+VAR (FIRST QCON)) T)
     (T (CHECKIS (REST QCON)))))

(DEFUN CHECK-IS+VAR (QCON)
  (COND 
   ((NULL QCON) NIL)
   ((ATOM QCON) NIL)
   ((NOT (EQUAL 2 (LENGTH QCON)))   NIL)
   ((NOT (EQUAL (FIRST QCON) 'IS))  NIL)
   ((EQUAL (FIRST (SECOND QCON)) 'V) T )
   (T NIL)))
	 
; -----------------------------------------------------------------------------

; FUNCTION: C-GEN
; PURPOSE:  Converts a frame into an English sentence, using a list of "sentence
;           patterns" (ENG-PATS) and decision-trees to help make some more
;           natural sounding phrasings.
; OUTPUT:   List of atoms, readible as an English sentence. (Essentially, this
;           is the inverse of the "C-ANALYZE" operation from HW#2)
; INPUTS:   C-ANS: a frame to convert into a sentence
;           ENG-PATS: a list of English sentence patterns, of the form
;           ( (pred (element)* ) )
;           where element is either
;           -- An atom (interpreted as a slot name), in which case C-GEN should
;              insert the result of recursing on the FILLER of that slot
;           -- A list of the form (PHR (atom)+) (the literal symbol PHR followed by a list of
;              atoms, interpreted as words to append to the sentence)
;           -- A list of the form (DECIDE PRED), in which case the utility function 
;              DECIDE-PHR will be dispatched to use the entry for PRED in D-TREES to 
;              decide which words to add (see specs for DECIDE-PHR)
;           D-TREES: a list of decision trees, whose format is described in
;                    the documentation for DECIDE-PHR

(defun C-GEN (C-ANS ENG-PATS D-TREES)
   (COND
    ((NULL C-ANS) '1NIL)
    ((ATOM C-ANS) (LIST C-ANS))
    ((NOT (EQUAL 'NOTFIND (SETQ PAT (C-GEN-FINDPAT ENG-PATS (FIRST C-ANS))))) 
                                                      (C-GEN-1 C-ANS ENG-PATS D-TREES PAT))
    (T C-ANS)))

(DEFUN C-GEN-1 (C-ANS ENG-PATS D-TREES PAT)
  (COND 
   ((NULL PAT) NIL)
   ((AND (ATOM (FIRST PAT))  
	 (SETQ SLOT (C-FINDSLOT C-ANS (FIRST PAT))))
                                                      
                                                      (APPEND (C-GEN SLOT ENG-PATS D-TREES)
							      (C-GEN-1 C-ANS ENG-PATS D-TREES (REST PAT))))
  ((AND (ATOM (FIRST PAT))
	 (EQUAL NIL SLOT) )                           (C-GEN-1 C-ANS ENG-PATS D-TREES (REST PAT)))
   ((EQUAL 'DECIDE (FIRST (FIRST PAT)))               (APPEND (DECIDE-PHR (SECOND (FIRST PAT)) C-ANS D-TREES)
						            (C-GEN-1 C-ANS ENG-PATS D-TREES (REST PAT))))
   ((EQUAL 'PHR (FIRST (FIRST PAT)))                  (APPEND (SECOND (FIRST PAT))
						            (C-GEN-1 C-ANS ENG-PATS D-TREES (REST PAT))))
   (T NIL)))

(DEFUN C-FINDSLOT (C-ANS PRED)
  (COND
   ((NULL C-ANS) NIL)
   ((ATOM (FIRST C-ANS)) (C-FINDSLOT (REST C-ANS) PRED))
   ((EQUAL PRED (FIRST (FIRST C-ANS))) (SECOND (FIRST C-ANS)))
   (T (C-FINDSLOT (REST C-ANS) PRED))))

(DEFUN C-GEN-FINDPAT (ENG-PATS PRED)
  (COND 
   ((NULL ENG-PATS) 'NOTFIND)
   ((EQUAL (FIRST (FIRST ENG-PATS)) PRED) (SECOND (FIRST ENG-PATS)))
   (T (C-GEN-FINDPAT (REST ENG-PATS) PRED))))
; -----------------------------------------------------------------------------

; FUNCTION: DECIDE-PHR
; PURPOSE:  Locate in the list of D-TREES an entry indexed by the given predicate
;           DECIDE-PHR then traverses the decision tree, checking attributes
;           of the TOP-LEVEL SLOTS of the frame in the order of traversal. Leaf
;           nodes always are a list of the form (PHR (word)+), that is a list
;           starting with the literal element "PHR" and followed by a list of
;           atoms listing a phrase.
; OUTPUT:   A phrase (list of words), or NIL if not found
; INPUTS:   PRED: predicate to choose which tree out of our list of d-trees
;           FRAME: frame to check top-level slots of
;           D-TREES: list of decision trees, indexed by predicate; format is
;           ( (pred-1 (node (val-1-1 (leaf or node…))
;                … … 
;                       (val-1-n (leaf or node…)))
;             (pred-2 (node (val-2-1 (leaf or node...)) … )
;           For example, the D-TREES list:
;           ( (HUMAN
;               (NOBILITY
;                  (ROYAL (GENDER
;                            (FEMALE (PHR (QUEEN)))
;                            (MALE (PHR (KING)))
;                         )
;                  )
;                  (NOBLE (GENDER
;                            (MALE (PHR (LORD)))
;                            (FEMALE (PHR (LADY)))
;                         )
;                  )
;                  (COMMON (PHR (PEASANT)))
;               )
;             )
;           )
;           has a single tree, which is defined for frames whose predicate is HUMAN.
;           It first checks the TOP-LEVEL slot "NOBILITY" -- if it is "royal," then it
;           checks the TOP-LEVEL slot "GENDER" and outputs either (QUEEN) or (KING)
;           accordingly. (Similarly for "NOBLE"; but if NOBILITY is "COMMON" then
;           it doesn't check gender, it just outputs (PEASANT)).

(defun DECIDE-PHR (PRED FRAME D-TREES)
  (COND 
     ((SETQ DT (FIND-THE-TREE PRED D-TREES)) (DECIDE-PHR-1 FRAME (SECOND DT)))
     (T NIL)))

(DEFUN DECIDE-PHR-1 (FRAME DT)
  (COND
   ((PHR? DT) (SECOND DT))
   ((SETQ PRED (FINDNODE FRAME (FIRST DT))) (DECIDE-PHR-1 FRAME (FIND-SUBDT (REST DT) PRED)))
   (T NIL)))

(DEFUN PHR? (DT)
  (COND 
   ((NOT (EQUAL 2 (LENGTH DT))) NIL)
   ((EQUAL (FIRST DT) 'PHR) T)
   (T NIL)))

(DEFUN FIND-SUBDT (DT PRED)
  (COND 
    ((NULL DT) NIL)
    ((EQUAL PRED (FIRST (FIRST DT))) (SECOND (FIRST DT)))
    (T (FIND-SUBDT (REST DT) PRED))))

(DEFUN FINDNODE (FRAME NODE)
  (COND
   ((NULL FRAME) NIL)
   ((ATOM (FIRST FRAME)) (FINDNODE (REST FRAME) NODE))
   ((EQUAL NODE (FIRST (FIRST FRAME))) (FIRST (SECOND (FIRST FRAME))))
   (T (FINDNODE (REST FRAME) NODE))))
	   

(DEFUN FIND-THE-TREE (PRED D-TREES)
  (COND
   ((NULL D-TREES) NIL)
   ((EQUAL PRED (FIRST (FIRST D-TREES))) (FIRST D-TREES))
   (T (FIND-THE-TREE PRED (REST D-TREES)))))
;===============================================================================

;HW 1

; FUNCTION: pop-slot
; PURPOSE:  Return a copy of FRAME with its first slot removed
; INPUT:    FRAME
; OUTPUT:   FRAME (with first slot removed)
(defun pop-slot (frame)
	(cons (first frame ) (nthcdr 2 frame))
)

; FUNCTION: front-slot
; PURPOSE:  Return the name of the first SLOT in FRAME
; INPUT:    FRAME
; OUTPUT:   atom: name of first SLOT
(defun front-slot (frame)
	(first (second frame))
)

; FUNCTION: front-filler
; PURPOSE:  Return the FILLER of the first SLOT in FRAME
; INPUT:    FRAME
; OUTPUT:   FRAME/GAP: filler of first SLOT in FRAME
(defun front-filler (frame)
	(second (second frame))
)

; FUNCTION: rm-slot
; PURPOSE:  Return a copy of FRAME, but with a single slot removed
; INPUT:    FRAME -- frame to remove slot from
;           slot  -- slot to be removed
; OUTPUT:   FRAME
(defun rm-slot (slot frame)
	(cond
		; Base case: no slots left, so we're done
		((<= (length frame) 1) frame)
		; Base case: front slot matches, so just pop it
		((equal (front-slot frame) slot) (pop-slot frame))
		; Recursive case: front slot doesn't match, so keep looking
		(t (append (rm-slot slot (pop-slot frame)) (list (second frame))))
	)
)

; -----------------------------------------------------------------------------
							
; FUNCTION: FILLER
; PURPOSE:  Return the FILLER of the given SLOT in the given FRAME
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
; INPUTS:   slot: atom
;           frame: FRAME

(defun FILLER (slot frame)
	(cond
		; Base case: predicate with no slots (or empty frame)
		((<= (length frame) 1) NIL)
		; If first slot matches, return its filler. (first (second ... gets the
		; slot name, while (second (second ... gets its filler
		((equal slot (front-slot frame)) (front-filler frame))
		; Else, first slot does not match, so test the rest of the slots
		(t (FILLER slot (pop-slot frame)))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: PATH-SL
; PURPOSE:  Return a FILLER accessed by following a path of named SLOTS within
;           the given CONCEPT
; OUTPUT:   FILLER of the CONCEPT at the given path, or NIL if not present
; INPUTS:   slots: list (path of SLOTs to follow)
;           concept: FRAME

(defun PATH-SL (slots concept)
	(cond
		; Base case: got to the last slot, so stop recursing
		((null slots) concept)
		; Recursive case: continue following path on sub-frame matched by filler
		; of current path element
		(t (PATH-SL (rest slots) (FILLER (first slots) concept)))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: PATH-PS
; PURPOSE:  Return a FILLER accessed by following a path of predicate-slot
;           pairs, where each predicate in the concept must match the
;           predicates in the path. 
; OUTPUT:   FILLER of the CONCEPT at the given path. If any predicate fails to
;           match, return FAIL. If any SLOT in the path does not exist, return
;           NIL.
; INPUTS:   path: a list of alternating predicate names and slot names,
;           beginning with a PRED and ending with a SLOTh
;           concept: instantiated FRAME

(defun PATH-PS (path concept)
	(cond
		; Base case: got to last slot, so stop recursing
		((null path) concept)
		; Check that current predicate matches; if not, return FAIL
		((not (equal (first path) (first concept))) 'FAIL)
		; Recursive case: predicate matches, so recurse on the FILLER of the
		; current slot
		(t (PATH-PS (nthcdr 2 path) (FILLER (second path) concept)))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: GAPSLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching GAPVALS
;           on each filler.
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
; INPUTS:   sf: list of (slot filler) pairs

(defun GAPSLOTS (sf)
	(cond
		; Base case: got through them all
		((null sf) nil)
		; Recursive case: dispatch GAPVALS on our first filler
		(t (cons (list (first (first sf))             ; rebuild our first slot-filler pair
		               (GAPVALS (second (first sf)))) ; dispatch GAPVALS on the filler
		               (GAPSLOTS (rest sf))))         ; recurse on rest of sf
	)
)

; FUNCTION: GAPVALS
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, except that
;           any GAPs have been replaced by their values
; OUTPUT:   FRAME, with all GAPs replaced
; INPUTS:   frame: FRAME to evaluate GAPs in
; HELPERS:  GAPSLOTS, to go through all of the elements of our frame's slot-fillers

(defun GAPVALS (frame)
	(cond
		; Base case: we got a non-NIL atom, so evaluate it if bound
		((not (listp frame)) (if (boundp frame) (GAPVALS (eval frame)) frame))
		; Base case: empty, or single pred frame
		((<= (length frame) 1) frame)
		; Main case: dispatch GAPSLOTS on our slot-filler list
		(t (cons (first frame) (GAPSLOTS (rest frame))))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: REPLACE-SF
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; OUTPUT:   Copy of FRAME with the top-level SLOT slot filled with filler
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           frame: FRAME (frame to be modified)

(defun REPLACE-SF (slot filler frame)
	(cond
		; Base case: single predicate, so add the slot
		((<= (length frame) 1) (list (first frame) (list slot filler)))
		; Base case: If first slot is target, replace the filler, keep rest slots
		((equal slot (front-slot frame)) (cons (first frame)
		                                       (cons (list slot filler)
							     (nthcdr 2 frame)))
		)
		; Recursive case: First slot not target, so pop and recurse
		(t (append (REPLACE-SF slot filler (pop-slot frame)) (list (second frame))))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: REPLACE-PTH
; PURPOSE:  Return a FRAME which is a copy of the reference frame, with the
;           SLOT given in the designated PATH filled in with the given FILLER
;           (or added if that SLOT does not exist)
; OUTPUT:   Copy of FRAME with SLOT in path filled with filler
; INPUTS:   path: list (path of SLOTS to follow)
;           filler: FILLER (value to put in SLOT referenced by path)
;           frame: FRAME (frame to be modified) 

(defun REPLACE-PTH (path filler frame) 
	(cond
		; Base case: ran out of path, so done
		((null path) frame)
		; Base case: empty frame (path didn't match), so stop
		((null frame) nil)
		; Base case: reached end of path, so do the replacement
		((= (length path) 1) (REPLACE-SF (first path) filler frame))
		; Base case: path doesn't match, so don't try to modify
		((equal (FILLER (first path) frame) NIL) frame)
		; Recursive case: follow path
		(t (REPLACE-SF (first path) (REPLACE-PTH (rest path) filler (FILLER (first path) frame)) frame))
	)
)

; -----------------------------------------------------------------------------

; Function to retrieve predicate of a FILLER (or just the symbol if it's a symbol)
(defun f-pred (frame)
	(cond
		((listp frame) (first frame))
		(t frame)
	)
)

; Function to safely check length of FILLER in case it isn't a list
(defun f-length (frame)
	(cond
		((listp frame) (length frame))
		(t 1)
	)
)

; FUNCTION: COMPARE-FRMS
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)

(defun COMPARE-FRMS (frame1 frame2)
		(cond
		; Base case: empty frames match
		((and (null frame1) (null frame2)) t)
		; Base case: frames with different preds do not match
		((not (equal (f-pred frame1) (f-pred frame2))) NIL)
		; Base case: frames of different lengths do not match
		((not (= (f-length frame1) (f-length frame2))) NIL)
		; Base case: bare predicates (or matching symbols) match
		((<= (f-length frame1) 1) t)
		; Base case: variables match iff they have the same name
		((equal (first frame1) 'V) (equal frame1 frame2))
		
		; Recursive case: check frame1's front slot, then remove and recurse to
		; check the rest of the slots.
		(t (let ((front (front-slot frame1))) 
					(and (COMPARE-FRMS (FILLER front frame1) (FILLER front frame2))
		           (COMPARE-FRMS (pop-slot frame1) (rm-slot front frame2)))
				)
		)
	)
)
;===============================================================================
;HW 3
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
		(t (let* ((result (STANDARDIZE-FRAME (second (first sf)) newnames)) 
		; dispatch STANDARDIZE-FRAME on the filler
		          (new-sf (list (first (first sf)) (first result))) ; generate our new sf-pair
				  (recurse-sf (STANDARDIZE-SLOTS (rest sf) (second result))) 
                 ; recurse on rest of sf
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
			 ;return ((V new-var) (old-var new-var)+newnames), i.e. add binding for old-var to newnames
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
