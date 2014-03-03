; CS 161 Spring 2013: HW1 test cases from assignment

(load "hw-1-solution.lsp")

; C1: "A pretty young girl tells John that she ate the big apple."
(setq C1 '(MTRANS (AGENT (HUMAN (GENDER (FEMALE)) 
                         (APPEARANCE (>NORM)) 
                         (AGE (<NORM)) 
                         (REF (INDEF))))
                  (RECIP (HUMAN (GENDER (MALE)) 
                                (F-NAME (JOHN)))) 
                  (OBJECT (INGEST (AGENT (HUMAN (GENDER (FEMALE)))) 
                                  (OBJECT (FOOD (TYPE (APPLE)) 
                                  (SIZE (>NORM)) 
                                  (REF (DEFINITE)))) 
                                  (TIME (PAST))))))

; C2: "Fred makes Betty ecstatic by telling Betty that she's pretty."
(setq C2 '(CAUSE (ANTE (INFORM (AGENT (HUMAN (F-NAME (FRED))  
                                             (GENDER (MALE)))) 
                               (RECIP (HUMAN (GENDER (FEMALE)) 
                                             (F-NAME (BETTY)))) 
                               (OBJECT (STATE (VALUE (>NORM)) 
                                              (TYPE  (APPEARANCE))  
                                              (AGENT (HUMAN  
                                                       (GENDER (FEMALE))))))))
                 (CONSEQ (STATE (AGENT (HUMAN (GENDER (FEMALE)) 
                                              (F-NAME (BETTY)))) 
                                (VALUE (>>NORM)) 
                                (TYPE (HAPPY))))))

; C3: "Frank walks to Frank's car"
(setq C3 '(PTRANS (AGENT AGENT0)
                  (OBJECT OBJECT0)
									(SRC LOC0)
									(DEST LOC1)
									(INSTRU (MOVE (AGENT AGENT0)
									              (B-PART (LEGS))))))
(setq AGENT0 'HUM1)
(setq HUM1 '(HUMAN (F-NAME F-NAME0) (GENDER (MALE))))
(setq F-NAME0 '(FRANK))
(setq OBJECT0 'HUM1)
(setq LOC0 NIL)
(setq LOC1 '(LOC (PROX (AUTO (OWNER AGENT1)))))
(setq AGENT1 'HUM1)

; Helper functions to make the main functions more readable
; (cond statement is just to not redefine if solution was included,
;  so we can avoid some annoying warning messages)

(cond ((boundp 'HW-1-SOLN-INCLUDED) nil)
(t

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
	
)) ; end if solution included

; Function to check if frames are equal
(defun fr-equal (fr1 fr2)
	(cond
		; Base case: empty frames match
		((and (null fr1) (null fr2)) t)
		; Base case: frames with different preds do not match
		((not (equal (f-pred fr1) (f-pred fr2))) NIL)
		; Base case: frames of different lengths do not match
		((not (= (f-length fr1) (f-length fr2))) NIL)
		; Base case: bare predicates (or matching symbols) match
		((<= (f-length fr1) 1) t)
		; Recursive case: check fr1's front slot, then remove and recurse
		; Recursive case: pop fr1's front slot and continue
		(t (and (fr-equal (FILLER (front-slot fr1) fr1) (FILLER (front-slot fr1) fr2))
		        (fr-equal (pop-slot fr1) (rm-slot (front-slot fr1) fr2))))
	)
)

; Test function; make sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
	(cond
		((equal actual expected) (format t "~A: success~%" case-name))
		(t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
	)
)

; Test function, with equality replaced with frame-equality
(defun test-case-fr (actual expected case-name)
	(cond
		((not (and (listp actual) (listp expected))) t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
		((fr-equal actual expected) (format t "~A: success~%" case-name))
		(t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
	)
)

; Test cases: Function 1 (FILLER)
(format t "Testing FILLER...~%")

(test-case-fr (FILLER 'AGENT C1) '(HUMAN (GENDER (FEMALE)) 
                                     (APPEARANCE (>NORM)) 
                                     (AGE (<NORM)) 
                                     (REF (INDEF))) "filler_ex_1")
(test-case (FILLER 'SRC C1) NIL "filler_ex_2")
(test-case-fr (FILLER 'OBJECT C1) '(INGEST (AGENT (HUMAN (GENDER (FEMALE)))) 
                                            (OBJECT (FOOD (TYPE (APPLE)) 
                                                          (SIZE (>NORM)) 
                                                          (REF (DEFINITE)))) 
                                            (TIME (PAST))) "filler_ex_3")
(test-case (FILLER 'DEST C3) 'LOC1 "filler_ex_4")
(test-case (FILLER 'OWNER LOC1) NIL "filler_ex_5")
(test-case-fr (FILLER 'PROX LOC1) '(AUTO (OWNER AGENT1)) "filler_ex_6")

(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 2 (PATH-SL)
(format t "Testing PATH-SL...~%")

(test-case-fr (PATH-SL '(ANTE AGENT) C2) '(HUMAN (F-NAME (FRED)) (GENDER (MALE))) "path-sl_ex_1")
(test-case-fr (PATH-SL '(CONSEQ AGENT F-NAME) C2) '(BETTY) "path-sl_ex_2")
(test-case (PATH-SL '(OBJECT AGENT F-NAME) C1) NIL "path-sl_ex_3")
(test-case (PATH-SL '(DEST) C3) 'LOC1 "path_sl_ex_4")
(test-case (PATH-SL '(F-NAME) HUM1) 'F-NAME0 "path_sl_ex_5")

(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 3 (PATH-PS)
(format t "Testing PATH-PS...~%")

(test-case-fr (PATH-PS '(CAUSE CONSEQ STATE VALUE) C2) '(>>NORM) "path-ps_ex_1")
(test-case (PATH-PS '(CAUSE TIME) C2) NIL "path-ps_ex_2")
(test-case (PATH-PS '(INFORM ANTE) C2) 'FAIL "path-ps_ex_3")

(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 4 (GAPVALS)
(format t "Testing GAPVALS...~%")

(test-case-fr (GAPVALS 'F-NAME0) '(FRANK) "gapvals_ex_1")
(test-case-fr (GAPVALS 'AGENT0) '(HUMAN (F-NAME (FRANK)) (GENDER (MALE))) "gapvals_ex_2")
(test-case-fr (GAPVALS C3) '(PTRANS (AGENT (HUMAN (F-NAME (FRANK)) 
                         (GENDER (MALE)))) 
          (OBJECT (HUMAN (F-NAME (FRANK)) 
                         (GENDER (MALE)))) 
        (SRC ( )) 
        (DEST (LOC (PROX  
                    (AUTO (OWNER  
                          (HUMAN  
                            (F-NAME (FRANK)) 
                               (GENDER (MALE)))))))) 
        (INSTRU (MOVE (AGENT (HUMAN (F-NAME (FRANK)) 
                                     (GENDER (MALE)))) 
                      (B-PART (LEGS))))) "gapvals_ex_3")
(test-case-fr (GAPVALS C1) '(MTRANS 
    (AGENT (HUMAN (GENDER (FEMALE)) 
                (APPEARANCE (>NORM)) 
                (AGE (<NORM)) 
                (REF (INDEF)))) 
    (RECIP (HUMAN (GENDER (MALE)) 
                  (F-NAME (JOHN)))) 
    (OBJECT (INGEST (AGENT (HUMAN (GENDER (FEMALE)))) 
                (OBJECT (FOOD (TYPE (APPLE)) 
                           (SIZE (>NORM)) 
                            (REF (DEFINITE)))) 
              (TIME (PAST))))) "gapvals_ex_4")
							
(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 5 (REPLACE-SF)
(format t "Testing REPLACE-SF...~%")
(test-case-fr (REPLACE-SF 'RECIP '(HUMAN) C1) '(MTRANS 
															(AGENT (HUMAN (GENDER (FEMALE)) 
																					(APPEARANCE (>NORM)) 
																					(AGE (<NORM)) 
																					(REF (INDEF)))) 
															(RECIP (HUMAN)) 
															(OBJECT (INGEST (AGENT (HUMAN (GENDER (FEMALE)))) 
																					(OBJECT (FOOD (TYPE (APPLE)) 
																										 (SIZE (>NORM)) 
																											(REF (DEFINITE)))) 
																				(TIME (PAST))))) "replace-sf_ex_1")

(test-case-fr (REPLACE-SF 'TIME '(PAST) C3) '(PTRANS  (AGENT AGENT0) 
          (OBJECT OBJECT0)   
          (SRC  LOC0) 
          (DEST  LOC1) 
          (INSTRU (MOVE (AGENT AGENT0) 
                      (B-PART (LEGS)))) 
          (TIME (PAST))) "replace-sf_ex_2")
																				
(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 6 (REPLACE-PTH)
(format t "Testing REPLACE-PTH...~%")

(test-case-fr (REPLACE-PTH '(CONSEQ AGENT) '(HUMAN) C2) '(CAUSE 
  (ANTE (INFORM (AGENT (HUMAN (F-NAME (FRED))  
                           (GENDER (MALE)))) 
             (RECIP (HUMAN (GENDER (FEMALE)) 
                        (F-NAME (BETTY)))) 
            (OBJECT (STATE (VALUE (>NORM)) 
                        (TYPE (APPEARANCE))  
               (AGENT (HUMAN  
                      (GENDER (FEMALE))))))))
											(CONSEQ (STATE (AGENT (HUMAN)) 
              (VALUE (>>NORM)) 
              (TYPE (HAPPY))))) "replace-pth_ex_1")

(format t "-------------------------~%")

; -----------------------------------------------------------------------------

; Test cases: Function 7 (COMPARE-FRMS)
(format t "Testing COMPARE-FRMS...~%")

(test-case (COMPARE-FRMS C1 '(MTRANS 
    (RECIP (HUMAN (GENDER (MALE)) 
                  (F-NAME (JOHN)))) 
    (AGENT (HUMAN (GENDER (FEMALE)) 
                (APPEARANCE (>NORM)) 
                (AGE (<NORM)) 
                (REF (INDEF)))) 
    (OBJECT (INGEST (AGENT (HUMAN  
                              (GENDER (FEMALE)))) 
                  (OBJECT (FOOD (TYPE (APPLE)) 
                             (SIZE (>NORM)) 
                              (REF (DEFINITE)))) 
                (TIME (PAST)))))) t "compare-frms_ex_1")

(format t "-------------------------~%")