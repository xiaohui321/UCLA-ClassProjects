; CS 161 Spring 2013: HW2 skeleton

; Use some utility functions from HW1 solution set
(load "solutions/hw-1-solution.lsp")

; File structure:
; Some convenience functions have been provided for you. They will be
; labeled GIVEN, to indicate you don't need to modify them as part of the
; assignment.
;
; Section 1: Utility functions:
;   -- CLEAR-GLOBALS (GIVEN): clears the global variables for the project
;   -- RELOAD (GIVEN): clears global variables and reloads your source file
;   -- UNIQUE-GAP (GIVEN): generates a unique symbol name from a base gap name
;   -- TOKENIZE (PROBLEM 3): replaces GAPs with unique values
;   -- SEARCH-WKM (PROBLEM 6): searches working memory for a predicate
;   -- ISA? (PROBLEM 7): checks the type of a predicate
;   -- BIND (PROBLEM 10): binds a GAP to a frame and updates global BOUND list
;   -- NUM-SLOTS (PROBLEM 20): counts a frame's number of slots at all levels
; Section 2: Main functions:
;   -- ADD-LEX (PROBLEM 1): adds information to the conceptual lexicon
;   -- RECALL-WPH (PROBLEM 2): finds a word/phrase in conceptual lexicon, returns
;                              associated frame and demons
;   -- EXT-WK-MEM (PROBLEM 4): adds an instantiated frame to working memory
;   -- SPAWN (PROBLEM 5): instantiates a list of demons and adds to ACTV-DEMONS
;   -- EXEC-DEMONS (PROBLEM 8): repeatedly calls demons in ACTV-DEMONS until
;                               either they all succeed or an entire round
;                               passes with no demon succeeding
;   -- TOP-CON (PROBLEM 21): looks through working memory to find the largest
;                            un-bound frame
;   -- C-ANALYZE (PROBLEM 22): top-level function to analyze a sentence
; Section 3: Demons:
;   -- D-LAST-NAME (PROBLEM 9)
;   -- D-FILL (PROBLEM 11)
;   -- D-AFT-PRED (PROBLEM 12)
;   -- D-SAME-BINDING (PROBLEM 13)
;   -- D-POSS-PRO-REF (PROBLEM 14)
;   -- D-IMMED-F-NAME (PROBLEM 15)
;   -- D-POSS-PRO (PROBLEM 16)
;   -- D-ATTACH-SF (PROBLEM 17)
;   -- D-PRO-REF (PROBLEM 18)
;   -- D-IMM-AFT (PROBLEM 19)

; ****** BEGIN SECTION 1: UTILITY FUNCTIONS ******

; UNIQUE-GAP takes the name of a symbol symName and generates a fresh, 
; unbound symbol with a unique name based upon symName. Note that the new
; symbol name may not have consecutive numbers and it may not start at
; 0001, but as long as they are unique this is fine for the homework.

; param symName - a symbol name to prefix the name of the symbol returned
; returns       - a fresh, unbound symbol with a unique name
;
; Examples:
; > (UNIQUE-GAP 'AGENT)
; #:AGENT1802
;
; > (boundp (UNIQUE-GAP 'AGENT))
; NIL
;
;
; Note: CLISP may print the characters "#:" before symbol names returned by 
; UNIQUE-GAP.  "#:" is the symbol's "package name," which is not important
; for this assignment.  To make Lisp stop printing the "#:" prefix for 
; symbols, type:
;  (setq *print-gensym* nil)
;
; Now, (UNIQUE-GAP 'AGENT) should return something like:
;  AGENT1806

(defun UNIQUE-GAP (symName)
	
	; Increment gensym counter to ensure returned symbol will print with 
	; unique name
	(setq *gensym-counter* (1+ *gensym-counter*))
	
	; Generate the new symbol using gensym
	(intern (string (gensym (string symName))))
)

; Resets the global variables used in the assignment. Loads default
; ontology.
(defun CLEAR-GLOBALS ()
	(setq C-LEX NIL)
	(setq ACTV-DEMONS NIL)
	(setq WK-MEM NIL)
	(setq BOUND NIL)
	(setq ONT '((ISA ACT CONCEPT) (ISA ST-CHANGE CONCEPT) 
	            (ISA STATE CONCEPT) (ISA C-CAUSE CAUSE)
	            (ISA CAUSE CONCEPT) (ISA MTRANS ACT)
	            (ISA STEAL ACT) (ISA HUMAN ANIMATE) (ISA CANINE ANIMATE)
	            (ISA ANIMATE PHYS-OBJ) (ISA VEHICLE PHYS-OBJ)
	            (ISA PAST TIME) (ISA D-LAST-NAME DEMON)))
)

; Resets global variables, THEN reloads your code. This means you can initialize the
; globals in your source file for testing purposes.
(defun RELOAD ()
	(clear-globals)
	(load "hw-2-examples.lsp") ; Replace with the name of this file
	; Feel free to load additional files for testing etc here
)

; ****** END GIVEN UTILITY FUNCTIONS ******

; ****** BEGIN PROBLEM SKELETONS ******

; -----------------------------------------------------------------------------

; PROBLEM 3: TOKENIZE

; FUNCTION: TOKENSLOTS (helper for TOKENIZE)
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching GAPVALS
;           on each filler.
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
; INPUTS:   sf: list of (slot filler) pairs

(defun TOKENSLOTS (sf)
	(cond
		; Base case: got through them all
		((null sf) nil)
		; Recursive case: dispatch TOKENIZE on our first filler
		(t (cons (list (first (first sf))             ; rebuild our first slot-filler pair
		               (TOKENIZE (second (first sf)))) ; dispatch GAPVALS on the filler
		               (TOKENSLOTS (rest sf))))         ; recurse on rest of sf
	)
)

; TOKENIZE should recursively go through a frame, replacing each gap it finds
; with a UNIQUE-GAP version of that gap. If called on a gap, it should replace
; the gap with a UNIQUE-GAP version.
; INPUTS: target (frame or gap)
; OUTPUT: frame instance (all gaps unique), or unique gap

(defun TOKENIZE (target)
	(cond
	; Base case: we got an atom, so tokenize it, set to NIL, and return it
	((not (listp target)) (let* ((gap (unique-gap target))) (set gap NIL) gap))
	; Base case: empty, or single pred frame
	((<= (length target) 1) target)
	; Main case: dispatch TOKENSLOTS on our slot-filler list
	(t (cons (first target) (TOKENSLOTS (rest target))))
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 6: SEARCH-WKM

; START-WKM is a helper for SEARCH-WKM (and several demons). It locates a particular
; CON-atom inside WKM, and returns the remainder of WKM in the given direction.
(defun START-WKM (wkm con dir)
	(cond
		; Why have two cases when we can have one? Just do a forward search in
		; the reversed list for a BEF search.
		((equal dir 'BEF) (START-WKM (reverse wkm) con 'AFT))
		; Base case: searched everything
		((null wkm) nil)
		; Base case: found what we were looking for, so just return the rest
		((equal (first wkm) con) (rest wkm))
		; Recursive case: no dice, so keep looking
		(t (START-WKM (rest wkm) con dir))
	)
)

; SEARCH-WKM searches through a working memory (wkm), which is structured as
; a list of atoms that evaluate to frames. It should start at the atom in wkm
; that matches con, moving either forward (if dir=AFT) or backward (if dir=BEF)
; in wkm looking for a frame whose top-level predicate matches pred.
; INPUTS: wkm: list of atoms that eval to frames
;         con: atom to start at
;         dir: direction to search (AFT -> forward, BEF -> backward)
;         pred: pred to search for
;         found (optional): was the starting CON found yet?
; OUTPUT: frame-reference atom if successful, NIL otherwise

(defun SEARCH-WKM (wkm con dir pred &optional found (test (lambda (p1 p2) (equal (first p1) p2))))
	(cond
		; As a first step, find our starting CON. Flag that we found it
		; by setting optional parameter "found"
		((not found) (SEARCH-WKM (START-WKM wkm con dir) con dir pred t test))
		
		; Base case: searched everything
		((null wkm) nil)

		; We already found the start atom, so start looking at predicates
		(t
			; See if pred matches
			(if (funcall test (eval (first wkm)) pred)
				; A match, so return it
				(first wkm)
				; Else, no match; remove and try again
				; (add optional argument "found" since we did find con)
				(SEARCH-WKM (rest wkm) con dir pred t test)
			)
		)
	)
)

; Wrapper to use "ISA?" inside SEARCH-WKM instead of "equal" (for D-FILL, prob 11)
(defun SEARCH-ISA (wkm con dir pred)
	(SEARCH-WKM wkm con dir pred nil (lambda (f p2) (ISA? (first f) p2 ONT)))
)

; Wrapper to use "ISA?" on a potential *list* of predicates (for D-AFT-PRED, prob 12)
(defun SEARCH-ISA-LIST (wkm con dir pred-list)
	; Our comparison ignores the "pred" argument to SEARCH-WKM and just uses our own list
	(SEARCH-WKM wkm con dir 'IGNORED nil
		(lambda (f p2)
			; Check everything in our pred-list; if the pred to test (pred of f) ISA? anything
			; in the pred-list, then return t. Otherwise NIL.
			(loop for p in pred-list do (if (ISA? (first f) p ONT) (return t) nil))
		)
	)
)

; Wrapper to look inside the frame at a particular path, and check for the pred there
; (D-POSS-PRO-REF, problem 14)
(defun SEARCH-PATH (wkm con dir path pred)
	(SEARCH-WKM wkm con dir pred nil
		(lambda (f p2)
			(equal (first (path-sl path f)) p2)
		)
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 7: ISA?

; ISA? takes an ontology, ONT, and checks to see whether pred1 ISA pred2.
; The ontology has the format (ISA P Q), which has the meaning that P is a Q.
; Note that this is transitive; if (ISA P Q) and (ISA Q R), then we ALSO say
; that P ISA R, even if this fact isn't explicitly stated in the ontology.
; INPUT: pred1 (atom): first pred to compare
;        pred2 (atom): second pred to compare
;        ont (list of lists): list of lists; format is
;                             ((ISA atom atom) (ISA atom atom) ...)
; OUTPUT: T, if pred1 ISA pred2, or if pred1=pred2. Note that this includes
; transitive ISA relations implied by the ontology, as described above.
; Otherwise, return NIL.

(defun ISA? (pred1 pred2 ont)
	(cond
		; Base case: same predicates
		((equal pred1 pred2) t)
		; Base case: "ISA pred1 pred2" is in the ONT
		((member (list 'ISA pred1 pred2) ont) t)
		
		; Recursive case: "ISA pred1 predi" where "ISA? predi pred2" for some i
		(t (let*
			; Find a parent of pred1 (ONT entry with second entry pred1)
			((parent (find-if (lambda (p) (equal (second p) pred1)) ont))
				
			 ; Need to remove that entry to avoid infinite loop
			 (next-ont (remove parent ont))
			)
			; If predi ISA? pred2, then success. Else look for a different predi
			; (predi is the third element of parent, since parent = (ISA pred1 predi))
				(cond 
					; If didn't find such a parent, we're done
					((null parent) nil)
					; Case 1: predi ISA pred2: success
					((ISA? (third parent) pred2 ont) t)
					; Case 2: pred1 ISA predi ... ISA pred2 for some different predi
					((ISA? pred1 pred2 next-ont) t)
					; Else: failure
					(t nil)
				)
			)
		)
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 10: BIND

; BIND takes a gap and a con-atom, and uses SETQ to set GAP = CON. It should
; also update the global variable BOUND, by adding the CON atom to its end.
; (BOUND is merely a list of CON-atoms on which BIND has been called)
; INPUT: gap (atom): gap to be bound
;        con (atom): CON to be bound
; OUTPUT: CON
; SIDE-EFFECT: binds GAP to CON, and adds CON to the global variable BOUND

(defun BIND (gap con)
	(set gap con)
	(setq BOUND (append BOUND (list con)))
	con
)

; -----------------------------------------------------------------------------

; PROBLEM 20: NUM-SLOTS

; COUNT-SLOTS is a helper for NUM-SLOTS. It takes a GAPVALS'd frame and counts
; how many slots appear at all levels.
(defun count-slots (frame)
	(cond
		; Base case: empty frame, or frame with no slots
		((<= (length frame) 1) 0)
		; Recursive case: pop a slot and recurse on the rest
		; 1 for the slot we pop
		; number of slots inside that slot
		; number of slots for rest of current frame
		(t (+ 1 (count-slots (front-filler frame)) (count-slots (pop-slot frame))))
	)
)
	

; NUM-SLOTS first gets the GAPVALS version of a FRAMI, then goes through each
; level counting the total number of slots.
; INPUT: frami (frame): frame to count
; OUTPUT: total number of slots in all levels of (GAPVALS frami)

(defun NUM-SLOTS (frami)
	(count-slots (gapvals frami))
)

; -----------------------------------------------------------------------------

; ****** END SECTION 1 ******

; ****** BEGIN SECTION 2: MAIN FUNCTIONS ******

;   PROBLEM 1: ADD-LEX
; ADD-LEX adds an entry to the global variable C-LEX, which is a representation
; of a conceptual lexicon. A conceptual lexicon is a list of lists, where each
; internal list has three parts: a list containing one or more words (WPH),
; a FRAME, and a list of zero or more DEMONs.
; INPUT: wph (list): list of one or more words (as atoms), which sentences will be
;                    matched against
;        frame (frame): frame to store in lexicon
;        demons (list): list of demons (function calls stored as data)
; OUTPUT: post-value of global variable C-LEX
; SIDE-EFFECT: Updates global C-LEX by appending the lexicon entry defined by the 
;              input arguments.
(defun ADD-LEX (wph frame demons)
	(setq C-LEX (append C-LEX (list (list wph frame demons))))
)

; -----------------------------------------------------------------------------

;   PROBLEM 2: RECALL-WPH

; MATCH-WPH: helper for RECALL-WPH.
; Returns length of lexical index (word/phrase) if a match with the start of s,
; 0 otherwise.

(defun MATCH-WPH (s lex)
	(let* ((len (length lex)))
		(cond
			; Not enough of s left, so no dice
			((< (length s) len) 0);
			; first len entries of s indeed match, so return len
			((equal (subseq s 0 len) lex) len)
			(t 0)
		)
	)
)

; RECALL-WPH searches the WPH parts of entries in the lexicon CLX for the longest
; phrase matching the front of S. It returns a list consisting of the associated
; frame and demons from the lexicon, with the remainder of S (after the part that
; matched with a phrase) removed.
; INPUT: sentence (list): list of words (as atoms)
;        clx: a conceptual lexicon (as defined above, and in the homework prompt)
; OUTPUT: a list of format (frame demons S-REM), where S-REM is the remainder of
;         S after removing the elements that were matched to an entry in the lexicon.
;         IF, however, no match is found in the lexicon, then it should return
;         the frame ((UNKNOWN (IS (w))) NIL S-REM), where w is the single word
;         at the front of S.

(defun RECALL-WPH (s clx)
	(cond
		((null s) nil)
		((null clx) nil)
		
		; got to end, so return the best match, or unknown
		((= (length clx) 1)
			(let* (
					(lex (first clx))
					(match-len (MATCH-WPH s (first lex)))
				  )
				(if (> match-len 0)
					; if match, build list of frame, demons, S-REM
					(list (second lex) (third lex) (nthcdr match-len s))
						
					; if no match, special return value
					(list
						; build the UNKNOWN frame
						(list 'UNKNOWN (list 'IS (list (first s))))
						; no demons
						NIL
						; remove first word
						(rest s)
					)
				)
			)
		)
		
		; Recursive case: find best match between first two entries. Keep
		; first entry if neither match.
		(t (let* (
		          (match1 (MATCH-WPH s (first (first clx))))
		          (match2 (MATCH-WPH s (first (second clx))))
				 )
					; remove the worst match of the first 2 entries
					(if (>= match1 match2)
						(RECALL-WPH s (cons (first clx) (nthcdr 2 clx)))
						(RECALL-WPH s (rest clx))
					)
			)
		)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 4: EXT-WK-MEM
; EXT-WK-MEM creates a new atom (using UNIQUE-GAP with a base name of CON),
; which it binds to the passed FRAMI. It adds the CON atom to the global list
; WK-MEM.
; INPUT: frami (frame): frame to be bind a new CON-atom to
; OUTPUT: new con-atom that got created
; SIDE-EFFECT: Adds generated CON atom to the end of the global list WK-MEM

(defun EXT-WK-MEM (frami)
	(let* ((mem (unique-gap 'CON)))
		; Bind to frame instance
		(set mem frami)
		; Add to working memory
		(setq WK-MEM (append WK-MEM (list mem)))
		; Return frame instance
		mem
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 5: SPAWN
; SPAWN creates a unique DEMI for each demon associated with a frame. It builds
; a function call (stored as a list) by inserting the CON atom as the first
; argument, in front of whatever else was passed to the demon in the lexicon.
; It also updates a global list ACTV-DEMONS
; INPUT: con (atom): CON-atom to set as first argument for each demon
;        demons (list): list of demons (partial function calls) from lexicon
; OUTPUT: list of demon instances
; SIDE-EFFECT: adds each demon instance created as above to the global list
;              ACTV-DEMONS.

(defun SPAWN (con demons)
	(cond
		((null demons) nil)
		(t
			(let* ( (dem (first demons))
					; construct new demon as ((first dem) con (rest dem))
					(newdemon (append (list (first dem)) (list con) (rest dem) ))
				  )
				(setq ACTV-DEMONS (append ACTV-DEMONS (list newdemon)))
			    (cons newdemon (SPAWN con (rest demons)))
			)
	    )
   )
)

; -----------------------------------------------------------------------------

;   PROBLEM 8: EXEC-DEMONS
; EXEC-DEMONS takes a list of demon-instances and a working memory (see problem 4)
; and repeatedly attempts to execute each demi. If a demi returns a non-NIL value
; then EXEC-DEMONS removes it from the demis list. EXEC-DEMONS should halt when
; either the demon list is empty, or it has progressed through a full round
; of invoking the demons with none of them returning non-NIL. (This condition
; is referred to in the specs as being "quiescent.") In this case it should return
; the list of demons that still remain.
; INPUT: demis (list): list of demon instances
;        wkm (list): list of CON-atoms (which evaluate to frames)
;        last-len (int, optional): length of demons list during previous call to exec
; OUTPUT: list of demons still remaining after quiescence is reached

(defun EXEC-DEMONS (demis wkm &optional last-len)
	(cond
		; Base case: nothing to do, or all demons successful
		((null demis) nil)
		; Length didn't change since last call => quiescence reached
		((equal (length demis) last-len) demis)
		; gather all unsuccessful demons
		(t (EXEC-DEMONS (loop for dem in demis append
							; call demon
							(if (apply (first dem) (rest dem))
							   nil ; if success, don't add back
							  (list dem) ; on failure, do add back
							)
					    )
						wkm
						(length demis) ; recursive call will check whether we
						               ; removed anything
			)
		)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 21: TOP-CON
; TOP-CON searches through a working memory list to look for CON-atoms which are
; not bound (i.e., do not appear in the global list BOUND). It returns the
; CON-atom with the greatest number of slots.
; INPUT: wkm (list): working memory to search

(defun TOP-CON (wkm)
	(cond
		; Base case: nothing to do
		((null wkm) nil)
		; Base case: first element is BOUND for the DANGER ZONE
		((member (first wkm) BOUND) (TOP-CON (rest wkm)))
		; Base case: last element standing is the TOP-CON
		((= (length wkm) 1) (first wkm))
		; Base case: *second* element is BOUND, so it loses to first element
		; (remove second element)
		((member (second wkm) BOUND) (TOP-CON (cons (first wkm) (nthcdr 2 wkm))))
		; Recursive case: both first and second elements are not BOUND. So keep
		; whichever one has more slots
		(t (let* ((count1 (NUM-SLOTS (first wkm))) (count2 (NUM-SLOTS (second wkm))))
			(if (>= count1 count2)
				; First one greater; keep it
				(TOP-CON (cons (first wkm) (nthcdr 2 wkm)))
				; Else, second one greater; keep it
				(TOP-CON (rest wkm))
			)
			)
		)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 22: C-ANALYZE
; C-ANALYZE is the top-most level function, that will return a frame representation
; of a sentence represented as a sequence of words. It loops through attempting
; to match the words of SENT to words or phrases in the lexicon, LEX, using
; RECALL-WPH. For each recognized phrase, it TOKENIZEs (problem 3) the associated
; frame, adds the CON atom of that frame to working memory with EXT-WK-MEM (problem 4),
; SPAWNs (problem 5) the associated demon list, and invokes EXEC-DEMONS (problem 8) on
; the global list ACTV-DEMONS, which it should update with the list EXEC-DEMONS returns
; (since some demons may have been removed). This process repeats until SENT is empty.
; C-ANALYZE then returns the GAPVALS of the TOP-CON of working memory as it stands
; after loading in the complete sentence.
; INPUT: sent (list): list of words (as atoms) representing a sentence
;        lex (list): a conceptual lexicon (see problem 1)


(defun C-ANALYZE (sent lex)
	(let* ((wph (recall-wph sent lex))
	       (frame (first wph))
	       (demons (second wph))
	       (nextsent (third wph))
		  )
		  ; When we've exhausted the sentence, return the gapvals of TOP-CON
		(if (null sent) (gapvals (TOP-CON WK-MEM))
			; Else, process frame/demons, then recurse
			(progn
				(SPAWN (EXT-WK-MEM (tokenize frame)) demons)
				; Update demons list in case we removed some
				(setq ACTV-DEMONS (EXEC-DEMONS ACTV-DEMONS wk-mem))
				; Finally, recurse: next part of sentence after removing matched wph
				(C-ANALYZE nextsent lex)
			)
		)
    )
)

; -----------------------------------------------------------------------------

; ****** END SECTION 2 ******

; ****** BEGIN SECTION 3: DEMONS ******

; Demons are short functions to BIND gaps in different frames of working memory
; to each other. Any given invocation of a demon may be successful (it found
; the things it was looking for and BIND'd them), or if it failed to find what
; it was looking for, it does nothing, waiting for later invocations. (It might
; succeed later when more of the sentence has been loaded into working memory.)
;
; Every demon gets as its first argument the CON it is working for (a particular
; element of working memory), which anchors searches for other frames, and will
; typically have gaps that get bound by the demon. Other arguments vary depending
; on the demon's function.
;
; The return value of a demon indicates whether it successfully
; did its operation: non-NIL value on success (eg, it could just return the
; result of the BIND call it had to make anyway), or NIL on failure.

;   PROBLEM 9: D-LAST-NAME

; D-LAST-NAME looks at the CON after its MYCON in working memory. If the frame
; there has a predicate of UNKNOWN, then BIND (problem 10) MYSLOT in MYCON frame
; to the IS slot of the "UNKNOWN" frame.
; INPUT: mycon(atom) - demon's frame in working memory
;        myslot(atom) - slot in MYCON to bind the "IS" of an UNKNOWN frame to
; Returns success if it found a frame with UNKNOWN

(defun D-LAST-NAME (mycon myslot)
	(let* ( (myframe (eval mycon))
	        (mygap (filler myslot myframe))
			(nextframe (eval (first (START-WKM WK-MEM mycon 'AFT)))))
		; is the frame an "UKNOWN?"
		(if (equal (first nextframe) 'UNKNOWN)
			; Set the "myslot" gap to the "IS" of the next frame; progn is
			; just machinery to always return "t" in case something weird 
			; like the filler being NIL happens
			(progn (set mygap (filler 'IS nextframe)) t)
			nil
		)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 11: D-FILL (see helper: SEARCH-ISA)

; D-FILL searches in DIR starting from MYCON looking for a frame that has
; predicate which ISA PRED. If found, BIND the GAP in
; MYCON located at PATH to the CON-atom of the found frame.
; INPUT: mycon(atom) - demon's frame in working memory
;        path(list) - location of gap to be bound, as list of slots
;        dir(AFT|BEF) - direction to search for PRED
;        pred(atom) - predicate to search for
; Returns success if it found PRED

(defun D-FILL (mycon path dir pred)
	(let* ( (myframe (eval mycon))
	        (mygap (path-sl path myframe))
			(target (search-isa wk-mem mycon dir pred))
		  )
		(if target (BIND mygap target) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 12: D-AFT-PRED (see helpers: SEARCH-WKM, SEARCH-ISA-LIST)
; D-AFT-PRED first searches AFTer its MYCON for a frame whose predicate is PRED.
; Starting from *that* frame, it searches AFT again until finding a frame whose
; predicate matches any one of the CONSTRAINTs. A predicate P is considered to 
; match if, for any X in the list CONSTRAINTs, P IS A X, checked using the
; utility function ISA? (see problem 7). If it finds a matching frame, it binds
; the gap located under MYPATH in MYCON, to the CON atom of the matching frame.
; INPUT: mycon(atom)  - demon's frame in working memory
;        mypath(list) - location of gap to be bound, as list of slots
;        pred(atom)   - predicate to look for in first part of search (see above)
;        constraints(list) - list of ATOMs to match ISA? with in second search
; Returns success if a frame is found following the above procedure.

(defun D-AFT-PRED (mycon mypath pred constraints)
	(let* ( (myframe (eval mycon))
	        (mygap (path-sl mypath myframe))
			(after (search-wkm wk-mem mycon 'AFT pred))
			(target (if after (search-isa-list wk-mem after 'AFT constraints) nil))
		  )
		(if target (BIND mygap target) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 13: D-SAME-BINDING
; D-SAME-BINDING checks to see if the gap at path1 in MYCON is NIL. If it is,
; it does nothing. If the gap is non-NIL, then it BINDs the gap at path2 in MYCON
; to the path1 gap. (This demon only modifies its own frame.)
; INPUT: mycon(atom) - demon's frame in working memory
;        path1(list) - path (list of slots) to check if non-NIL
;        path2(list) - path (list of slots) to bind to gap in path1
; Returns success if the gap at path1 was non-NIL

(defun D-SAME-BINDING (mycon path1 path2)
	(let* ( (myframe (eval mycon))
	        (mygap1 (path-sl path1 myframe))
			(mygap2 (path-sl path2 myframe))
		  )
		(if (eval mygap1) (set mygap2 (eval mygap1)) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 14: D-POSS-PRO-REF (see helper: SEARCH-PATH)
; D-POSS-PRO-REF looks in direction DIR for a frame which has a slot located
; at PATH whose filler's predicate is PRED. If found, then bind the gap at
; MYSLOT inside MYCON to the CON-atom of the frame that matched the above
; condition.
; INPUT: mycon(atom)  - demon's frame in working memory
;        myslot(atom) - top-level slot whose gap should be bound to the referent
;                       of the pronoun
;        dir(BEF|AFT) - direction to search
;        path(list)   - path to follow in each frame being searched
;        pred         - pred that must be present in the filler of PATH in
;                       frame being searched
; Returns success if a frame matching the above description is found

(defun D-POSS-PRO-REF (mycon myslot dir path pred)
	(let* ((myframe (eval mycon))
	       (mygap (filler myslot myframe))
	       (target (search-path wk-mem mycon dir path pred))
	      )
		  (if target (BIND mygap target) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 15: D-IMMED-F-NAME
; D-IMMED-F-NAME looks in the frame immediately after MYCON. If that frame
; has a top-level slot F-NAME, then bind the gap of the F-NAME slot in MYCON
; to the filler of F-NAME in the neighboring frame.
; INPUT: mycon(atom) - demon's frame in working memory
; Returns success if the next frame has an F-NAME slot

; **NOTE: not required by assignment, as never used in lexicon**


(defun D-IMMED-F-NAME (mycon)
	(let* ((myframe (eval mycon))
	       (mygap (filler 'F-NAME myframe))
		   (nextframe (eval (first (START-WKM WK-MEM mycon 'AFT))))
		   (nextfiller (filler 'F-NAME nextframe))
		  )
		
		(if nextfiller (set mygap nextfiller) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 16: D-POSS-PRO
; D-POSS-PRO looks in direction DIR for a frame whose predicate is PRONOUN,
; and furthermore has a TYPE slot filled by (POSS). If found, the demon binds
; the gap located at PATH in MYCON to the filler of the REF slot in the found frame.
; INPUT: mycon (atom) - demon's frame in working memory
;        dir (BEF|AFT) - direction to search
;        path (list) - path of slots in MYCON, which should be bound
; Returns success if it finds the pronoun frame

(defun D-POSS-PRO (mycon dir path)
	(let* ((myframe (eval mycon))
	       (mygap (path-sl path myframe))
		   ; Test function for search is whether frame's filler of "TYPE" is (POSS)
		   ; Also predicate must be "pronoun"
		   (target (search-wkm wk-mem mycon dir 'PRONOUN nil 
						(lambda (f p2) (and (equal (filler 'TYPE f) '(POSS))
					                        (equal (first f) p2)
										)
						)
					)
		    )
		  )
		; 
		(if target (set mygap (filler 'REF (eval target))) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 17: D-ATTACH-SF
; D-ATTACH-SF searches in direction DIR for a frame with predicate PRED. If
; found, it inserts in that frame the slot-filler pair (SLOT FILLER).
; INPUT: mycon (atom)       - demon's frame in working memory
;        dir (BEF|AFT)      - direction to search
;        pred (atom)        - predicate to look for
;        slot (atom)        - slot to add to found frame
;        filler (frame|gap) - filler for added slot
; Returns success if frame with predicate is found

(defun D-ATTACH-SF (mycon dir pred slot filler)
	(let* ((target (search-isa wk-mem mycon dir pred)))
		; If target found, get frame and insert the slot with filler
		(if target (set target (replace-sf slot filler (eval target))) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 18: D-PRO-REF
; D-PRO-REF searches in direction DIR for a frame whose filler of SLOT1 has a pred
; matching the parameter FILLER. If found, then bind the gap of SLOT2 in MYCON
; to the found frame.
; INPUT: mycon (atom) - demon's frame in working memory
;        dir (BEF|AFT) - direction to search
;        slot1 (atom) - slot to check while searching
;        filler (frame|gap) - filler of searched frame's SLOT1 must match this
;        slot2 (atom) - slot in MYCON to be bound to a found frame
; Returns success if a frame matching the above conditions is found

(defun D-PRO-REF (mycon dir slot1 filler slot2)
	(let* ((myframe (eval mycon))
	       (mygap (filler slot2 myframe))
		   (target (search-wkm wk-mem mycon dir 'IGNORED nil
						(lambda (f p2)
							(let* ((fil (filler slot1 f)))
							      (and (listp fil) (equal (first fil) filler))
							)
						)
				   )
		   )
	      )
		(if target (set mygap (filler slot2 (eval target))) nil)
	)
)

; -----------------------------------------------------------------------------

;   PROBLEM 19: D-IMM-AFT
; D-IMM-AFT looks at the frame immediately after MYCON. If there is a non-NIL
; filler of SLOT in that frame, then bind the gap of the same SLOT in mycon with
; that filler.
; INPUT: mycon (atom) - demon's frame in working memory
;        slot (atom) - slot to check, and fill in mycon

(defun D-IMM-AFT (mycon slot)
	(let* ((myframe (eval mycon))
	       (mygap (filler slot myframe))
		   (nextframe (eval (first (START-WKM WK-MEM mycon 'AFT))))
		   (nextfiller (filler slot nextframe))
		  )
		
		(if nextfiller (set mygap nextfiller) nil)
	)
)

; -----------------------------------------------------------------------------

; ****** END SECTION 3 ******
