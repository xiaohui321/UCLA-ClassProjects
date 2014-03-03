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
	'UNIMPLEMENTED
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
  (cond
     (
)

(defun UNIVAR-CONTAINS(var feta)
  (cond 
     ((EQUAL (LENGTH FETA) 1))  NIL)
     ((equal (first (second feta)) var) (second (second feta)))
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
	'UNIMPLEMENTED
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

(defun STANDARDIZE-VVARS (rule)
	'UNIMPLEMENTED
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
	'UNIMPLMENTED
)
