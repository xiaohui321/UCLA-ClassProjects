; ============================ 
;   CS 161 Spring 2013 HW-3
;   LASTNAME:   XIAOHUI
;   FIRSTNAME:  ZHOU
;   ID: 104-014-248
; ============================

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
  (COND
   ((EQUAL NIL XX) FETA)
   ((EQUAL NIL FETA) NIL)
   ((EQUAL XX  YY)   FETA)
   ((UNIFRAME-VARIABLE? XX)           (UNIVAR XX YY FETA))
   ((UNIFRAME-VARIABLE? YY)           (UNIVAR YY XX FETA))
   ((AND (EQUAL (FIRST XX) (FIRST YY))
	 (UNIFRAME-FRAME? XX) 
	 (UNIFRAME-FRAME? YY))        (UNIFRAME-frame XX YY FETA))
   ((AND (UNIFRAME-LISTOFFRAMES? XX)
	 (UNIFRAME-LISTOFFRAMES? YY)
         (UNIFRAME-FINDFRAME XX YY))
    (UNIFRAME (REST XX) 
	      (APPEND (SUBSEQ YY 0 (UNIFRAME-FINDFRAME XX YY))
		      (REST (SUBSEQ YY (UNIFRAME-FINDFRAME XX YY) (LENGTH YY))))
	      (UNIFRAME (FIRST XX) (NTH (UNIFRAME-FINDFRAME XX YY) YY) FETA)))
   (T  NIL)))

(DEFUN UNIFRAME-FINDFRAME (XX YY)
  (COND 
   ((EQUAL NIL YY) NIL)
   ((EQUAL (FIRST (FIRST XX)) (FIRST (FIRST YY))) 0)
   ((UNIFRAME-FINDFRAME XX (REST YY)) (+ 1 (UNIFRAME-FINDFRAME XX (REST YY))))
   (t NIL)))

(DEFUN UNIFRAME-frame (XX YY FETA)
  (COND
   ((EQUAL NIL FETA) NIL)
   ((EQUAL XX  YY)   FETA)
   ((UNIFRAME-VARIABLE? XX)               (UNIVAR XX YY FETA))
   ((UNIFRAME-VARIABLE? YY)               (UNIVAR YY XX FETA))
   ((EQUAL NIL (UNIFRAME-CONTAINS XX YY))  NIL)
   ((EQUAL (LENGTH XX) 1) FETA)
   (T  (UNIFRAME-FRAME (APPEND (LIST (FIRST XX))
			       (REST (REST XX))) 
		       (APPEND (SUBSEQ  YY 0 (UNIFRAME-CONTAINS XX YY))
			       (REST (SUBSEQ  YY (UNIFRAME-CONTAINS XX YY) (LENGTH YY))))
		       (UNIFRAME-FRAME (SECOND (SECOND XX))
				       (SECOND (NTH (UNIFRAME-CONTAINS XX YY) YY))
				       FETA)))))

(defun UNIFRAME-CONTAINS(XX YY)
  (cond 
     ((EQUAL NIL YY)  NIL)
     ((EQUAL (FIRST (SECOND XX))  (FIRST (SECOND YY))) 1)
     ((UNIFRAME-CONTAINS XX (REST YY)) (+ 1 (UNIFRAME-CONTAINS XX (REST YY))))
     (t NIL)))

(DEFUN UNIFRAME-VARIABLE? (XX)
  (COND 
   ((EQUAL XX NIL) NIL)
   ((ATOM XX)      NIL)
   ((NOT (EQUAL (LENGTH XX) 2)) NIL)
   ((EQUAL (FIRST XX) 'V) (SECOND XX))
   (T NIL)))
   
(DEFUN UNIFRAME-FRAME?(XX)
  (COND 
   ((EQUAL NIL XX)    NIL)
   ((ATOM XX)         NIL)
   ((AND (ATOM  (FIRST  XX))
	 (LISTP (SECOND XX))) T)
   (T                 NIL)))
 
(DEFUN UNIFRAME-LISTOFFRAMES? (XX)
  (COND 
   ((EQUAL NIL XX)    NIL)
   ((ATOM XX)         NIL)
   ((ATOM (FIRST XX)) NIL)
   (T                 T)))
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
  (cond
   ; X IS NOT A VAR AND X IS DIFFENT FROM VAR 
   ((AND (NOT (UNIFRAME-VARIABLE? X)) 
	 (NOT (EQUAL NIL (UNIVAR-CONTAINS VAR FETA) )) 
	 (NOT (EQUAL X (UNIVAR-CONTAINS VAR FETA))))                     NIL)
   ; X AND VAR ARE BOTH VAR AND THEIR CONTENTS ARE DIFFERENT 
   ((AND (UNIFRAME-VARIABLE? X) (NOT (EQUAL (UNIVAR-CONTAINS VAR FETA) (UNIVAR-CONTAINS X FETA)))) NIL)
   ((UNIVAR-CONTAINS VAR FETA) (UNIFRAME     (UNIVAR-CONTAINS VAR FETA) X  FETA))
   ((UNIVAR-CONTAINS X   FETA) (UNIFRAME VAR (UNIVAR-CONTAINS X   FETA)    FETA))
   (T (APPEND FETA (LIST (LIST (SECOND VAR) X))))))

(defun UNIVAR-CONTAINS(var feta)
  (cond 
     ((EQUAL (LENGTH FETA) 1)  NIL)
     ((equal (first (second feta)) (SECOND var)) (second (second feta)))
     (t (UNIVAR-CONTAINS VAR (REST FETA)))))

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
;           ( (MONARCH (IS (V Q))) (FEMALE (IS (V Q))) ===> (ROYAL (IS (V Q))) ) is a
;           list of the single rule in first order logic
;           "forall Q (Monarch(Q) & Female(Q)) => Queen(Q)"
;           NEW: list of instantiated frames (containing no variables), to be
;           built up in recursive calls by applying premises to the current knowledge
;           base, and returned as output

(defun FFINFER (facts rules new)
  (LET* ()
	(SETQ NEW-NEWS (FFINFER-REMAIN (FFINFER-1 FACTS RULES NIL) NEW))
	(COND 
	 ((EQUAL NIL NEW-NEWS) NEW)
	 (T (FFINFER (append NEW-NEWS FACTS) RULES (APPEND NEW-NEWS NEW))))))
	      
(DEFUN FFINFER-REMAIN (NEW-NEWS NEW)
  (COND
   ((NULL NEW-NEWS) NIL)
   ((FFINFER-CONTAIN (FIRST NEW-NEWS) NEW) (FFINFER-REMAIN (REST NEW-NEWS) NEW))
   (T      (APPEND (LIST (FIRST NEW-NEWS)) (FFINFER-REMAIN (REST NEW-NEWS) NEW)))))

(DEFUN FFINFER-CONTAIN (NEW-NEW NEW)
  (COND 
     ((NULL NEW) NIL)
     ((EQUAL NEW-NEW (FIRST NEW)) T)
     (T (FFINFER-CONTAIN NEW-NEW (REST NEW)))))



(defun FFINFER-1 (facts rules new)
  (COND 
   ((NULL RULES) NEW)
   (T (LET* ()
	    (SETQ FIRSTRULE (STANDARDIZE-VVARS (FIRST RULES)))
	    (COND 
	       ((SETQ FETA (UNIFRAME (BUTLAST FIRSTRULE 2) FACTS '(T)))
		(FFINFER-1 FACTS (REST RULES) 
			   (CONS (SUBST-FETA (FIRST (LAST FIRSTRULE)) FETA) NEW)))
	       (T (FFINFER-1 FACTS (REST RULES) NEW)))))))
	    
; -----------------------------------------------------------------------------

; FUNCTION: STANDARDIZE-VVARS
; PURPOSE:  Helper function for FFINFER. Takes a RULE formatted as in FFINFER
;           and replace all its variables with uniquely named ones. Eg,
;           (V X) should become (V X1234), or whatever the output of UNIQUE-GAP
;           happens to be).
; OUTPUT:   Rule with uniquely named variables
; INPUTS:   Rule to standardize, format is:
;           (premise-1 premise-2 ... premise-n ===> conclusion)
(defun STANDARDIZE-VVARS (rule)
  (LET* ()
	;(setq *gensym-counter* (1+ *gensym-counter*))
	(STANDARDIZE-VVARS-0 RULE)))

(defun STANDARDIZE-VVARS-0 (rule)
  (COND 
   ((NULL RULE) NIL)
   ((EQUAL '===> (FIRST RULE)) 
    (CONS '===> (STANDARDIZE-VVARS (REST RULE))))
   (T (APPEND (LIST (STANDARDIZE-VVARS-1 (FIRST RULE)))
	      (STANDARDIZE-VVARS (REST RULE))))))

(defun STANDARDIZE-VVARS-1 (PREMISE)
  (COND 
   ((NULL PREMISE) NIL)
   ((EQUAL 1 (LENGTH PREMISE)) PREMISE)
   ((UNIFRAME-VARIABLE? (SECOND (FIRST (LAST PREMISE))))
    (APPEND (STANDARDIZE-VVARS-1 (BUTLAST PREMISE))
	    (LIST (LIST (FIRST (FIRST (LAST PREMISE))) 
			(LIST 'V (UNIQUE-GAP (SECOND (SECOND (FIRST (LAST PREMISE))))))))))
   ((UNIFRAME-FRAME? (SECOND (FIRST (LAST PREMISE))))
    (APPEND (STANDARDIZE-VVARS-1 (BUTLAST PREMISE))
    (LIST (CONS (FIRST (FIRST (LAST PREMISE))) (LIST (STANDARDIZE-VVARS-1 (SECOND (FIRST (LAST PREMISE)))))))))
   (T NIL)
))
(defun UNIQUE-GAP (symName)
  ; Increment gensym counter to ensure returned symbol will print with 
  ; unique name
  (setq *gensym-counter* (1- *gensym-counter*))
  ; Generate the new symbol using gensym
  (gensym (string symName)))

; -----------------------------------------------------------------------------

; FUNCTION: SUBST-FETA
; PURPOSE:  Replace all variables in the given frame with their bindings given
;           in the binding-list feta.
; OUTPUT:   Instantiated frame, where variables have been replaced
; INPUTS:   frame: frame to replace variables in
;           feta: binding list, of the form from UNIFRAME
;           (T (var1 binding1) (var2 binding2) ...)

(defun SUBST-FETA (frame feta)
  (COND 
   ((OR (NULL FRAME) (EQUAL (LENGTH FRAME)1)) FRAME)
   ((APPEND (SUBST-FETA (BUTLAST FRAME) FETA)
	    (LIST (SUBST-FETA-1 (FIRST (LAST FRAME)) FETA))))))

(defun SUBST-FETA-1 (SLOT feta)
  (cond 
   ((UNIFRAME-VARIABLE? (SECOND SLOT))   (LIST (FIRST SLOT) (SUBST-FETA-2 (SECOND (SECOND SLOT)) FETA)))
   ((UNIFRAME-FRAME?    (SECOND SLOT))   (LIST (FIRST SLOT) (SUBST-FETA (SECOND SLOT) FETA)))
   (T SLOT)))

(DEFUN SUBST-FETA-2 (VAR FETA)
  (COND
    ((<=(LENGTH FETA) 1) 'AANIL)
    ((STRING= (FIRST (SECOND FETA)) VAR) (SECOND (SECOND FETA)))
    (T (SUBST-FETA-2 VAR (REST FETA)))))