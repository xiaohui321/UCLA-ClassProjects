; CS 161 Spring 2013: HW1 solutions

; -----------------------------------------------------------------------------

; Helper functions to make the main functions more readable

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

; -----------------------------------------------------------------------------

; mark as included for examples script
; (this is not necessary for solution correctness, but avoids some warnings)

(setq HW-1-SOLN-INCLUDED t)