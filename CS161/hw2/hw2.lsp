; ============================ 
;   CS 161 Spring 2013 HW-2
;   LASTNAME:   XIAOHUI
;   FIRSTNAME:  ZHOU
;   ID: 104-014-248
; ============================

; File structure:
; Some convenience functions have been provided for you. They will be
; labeled GIVEN, to indicate you don't need to modify them as part of the
; assignment.
;
; Section 1: Utility functions:
;   ++ CLEAR-GLOBALS (GIVEN): clears the global variables for the project
;   ++ RELOAD (GIVEN): clears global variables and reloads your source file
;   ++ UNIQUE-GAP (GIVEN): generates a unique symbol name from a base gap name
;   ++ TOKENIZE (PROBLEM 3): replaces GAPs with unique values
;   ++ SEARCH-WKM (PROBLEM 6): searches working memory for a predicate
;   ++ ISA? (PROBLEM 7): checks the type of a predicate
;   ++ BIND (PROBLEM 10): binds a GAP to a frame and updates global BOUND list
;   ++ NUM-SLOTS (PROBLEM 20): counts a frame's number of slots at all levels

; Section 2: Main functions:
;   ++ ADD-LEX (PROBLEM 1): adds information to the conceptual lexicon
;   ++  RECALL-WPH (PROBLEM 2): finds a word/phrase in conceptual lexicon, returns
;                              associated frame and demons
;   ++ EXT-WK-MEM (PROBLEM 4): adds an instantiated frame to working memory
;   ++ SPAWN (PROBLEM 5): instantiates a list of demons and adds to ACTV-DEMONS
;   ++ EXEC-DEMONS (PROBLEM 8): repeatedly calls demons in ACTV-DEMONS until
;                               either they all succeed or an entire round
;                               passes with no demon succeeding
;   ++ TOP-CON (PROBLEM 21): looks through working memory to find the largest
;                            un-bound frame
;   -- C-ANALYZE (PROBLEM 22): top-level function to analyze a sentence

; Section 3: Demons:
;   ++ D-LAST-NAME (PROBLEM 9)
;   ++ D-FILL (PROBLEM 11)
;   ++ D-AFT-PRED (PROBLEM 12)
;   ++ D-SAME-BINDING (PROBLEM 13)
;   ++ D-POSS-PRO-REF (PROBLEM 14)
;   ++ D-IMMED-F-NAME (PROBLEM 15)
;   ++ D-POSS-PRO (PROBLEM 16)
;   ++ D-ATTACH-SF (PROBLEM 17)
;   ++ D-PRO-REF (PROBLEM 18)
;   ++ D-IMM-AFT (PROBLEM 19)

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
	(load "hw2.lsp") ; Replace with the name of this file
	; Feel free to load additional files for testing etc here
)

; ****** END GIVEN UTILITY FUNCTIONS ******

; ****** BEGIN PROBLEM SKELETONS ******

; -----------------------------------------------------------------------------

; PROBLEM 3: TOKENIZE

; TOKENIZE should recursively go through a frame, replacing each gap it finds
; with a UNIQUE-GAP version of that gap. If called on a gap, it should replace
; the gap with a UNIQUE-GAP version.
; INPUTS: target (frame or gap)
; OUTPUT: frame instance (all gaps unique), or unique gap

(defun TOKENIZE (target)
  (COND 
       ((NULL TARGET) NIL)
       ((ATOM TARGET) 
	(let () 
	  (SETQ a (UNIQUE-GAP TARGET))
	  (SET  a NIL)
	  A ))
       ((= (LENGTH TARGET) 1) (LIST (FIRST TARGET)))
       (T (APPEND 
	   (LIST (FIRST TARGET))
	   (TOKENIZE-1 (REST TARGET))))))

(defun TOKENIZE-1 (target)
  (COND 
      ((NULL TARGET) ())
      ((= (LENGTH TARGET) 1) (LIST (TOKENIZE (FIRST TARGET))))
      (T (APPEND 
	   (LIST (TOKENIZE (FIRST TARGET)))
	   (TOKENIZE-1 (REST TARGET))))))
; -----------------------------------------------------------------------------

; PROBLEM 6: SEARCH-WKM

; SEARCH-WKM searches through a working memory (wkm), which is structured as
; a list of atoms that evaluate to frames. It should start at the atom in wkm
; that matches con, moving either forward (if dir=AFT) or backward (if dir=BEF)
; in wkm looking for a frame whose top-level predicate matches pred.
; INPUTS: wkm: list of atoms that eval to frames
;         con: atom to start at
;         dir: direction to search (AFT -> forward, BEF -> backward)
;         pred: pred to search for
; OUTPUT: frame-reference atom if successful, NIL otherwise

(defun SEARCH-WKM (wkm con dir pred)
   (LET* ()
	 (COND 
	    ((EQUAL 'AFT DIR)  (SEARCH-WKM-AFTER WKM (+ 1 (SEARCH-WKM-FIND-POSITION WKM CON)) PRED)) 
	    ((EQUAL 'BEF DIR)  (SEARCH-WKM-BEFORE WKM (- (SEARCH-WKM-FIND-POSITION WKM CON) 1) PRED))
	    (T 'INVALID_DIR))))

(DEFUN SEARCH-WKM-FIND-POSITION (WKM CON)
  (COND 
   ((NULL WKM) 0)
   ((EQUAL CON (FIRST WKM)) 0)
   (T (+ 1 (SEARCH-WKM-FIND-POSITION (REST WKM) CON)))))

(DEFUN SEARCH-WKM-AFTER (WKM NUM PRED)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((ISA? (FIRST (EVAL (NTH NUM WKM))) PRED ONT)  (NTH NUM WKM))
     (T (SEARCH-WKM-AFTER WKM (+ 1 NUM) PRED))))

(DEFUN SEARCH-WKM-BEFORE (WKM NUM PRED)
  (COND 
     ((null wkm) NIL)
     ((> 0 num) NIL)
     ((ISA?  (FIRST (EVAL (NTH NUM WKM))) PRED ONT)  (NTH NUM WKM))
     (T (SEARCH-WKM-BEFORE WKM (- NUM 1) PRED))))

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
   (COND 
        ((EQUAL PRED1 PRED2) T)   
        ((EQUAL PRED1 NIL) NIL)
       	(T (ISA? (ISA-FIND PRED1 ONT) PRED2 ONT))))

(DEFUN ISA-FIND (PRED1 ONT)
  (COND 
       ((NULL ONT) NIL)
       ((EQUAL PRED1 (SECOND (FIRST ONT))) (THIRD (FIRST ONT)))
       (T (ISA-FIND PRED1 (REST ONT)))))

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
	  (SETQ BOUND (APPEND BOUND (LIST CON))) 
	  (SET GAP CON))

; -----------------------------------------------------------------------------

; PROBLEM 20: NUM-SLOTS

; NUM-SLOTS first gets the GAPVALS version of a FRAMI, then goes through each
; level counting the total number of slots.
; INPUT: frami (frame): frame to count
; OUTPUT: total number of slots in all levels of (GAPVALS frami)

(defun NUM-SLOTS (frami)
  (SLOTS-COUNT (REST (GAPVALS FRAMI))))

(DEFUN SLOTS-COUNT (FRAMI)
  (COND
   ((NULL FRAMI) 0)
   ((> (LENGTH FRAMI) 1) 
    (COND
     ((ATOM (FIRST FRAMI))
      (+ 1 (SLOTS-COUNT (REST FRAMI))))
     (T (+ (SLOTS-COUNT-1 (FIRST FRAMI)) (SLOTS-COUNT (REST FRAMI))))))
   ((ATOM (FIRST FRAMI)) 1)
   (T (SLOTS-COUNT-1 (FIRST FRAMI)))))

(DEFUN SLOTS-COUNT-1 (FRAMI)
  (COND
   ((EQUAL NIL FRAMI) 0)
   ((ATOM (SECOND FRAMI))
    (COND
     ((NULL (REST FRAMI)) 0)
     (T (+ 1 (SLOTS-COUNT-1 (REST FRAMI))))))
   (T (+ (SLOTS-COUNT (SECOND FRAMI)) (SLOTS-COUNT-1 (REST FRAMI))))))
; -----------------------------------------------------------------------------

; ****** END SECTION 1 ******

; ****** BEGIN SECTION 2: MAIN FUNCTIONS ******

; finished
; PROBLEM 1: ADD-LEX 
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
	(SETQ C-LEX (CONS 
		     (LIST WPH FRAME DEMONS) C-LEX)))
; -----------------------------------------------------------------------------

;   PROBLEM 2: RECALL-WPH
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
  (LET* ()
	(SETQ TEMP-CLX (COPY-LIST CLX))
	(SORT TEMP-CLX  #'SORT-CLX>)
	(RECALL-WPH-1 S TEMP-CLX)))

(DEFUN RECALL-WPH-1 (S CLX)
  (cond 
   ((null clx) (cons 
		(cons 'UNKNOWN (cons  (list 'IS (cons (first s) nil)) nil))
		(list () (rest s))))
   ((AND (>= (LENGTH S) (LENGTH (FIRST (FIRST CLX)))) 
         (EQUAL (FIRST (FIRST CLX)) (SUBSEQ S 0 (LENGTH (FIRST (FIRST CLX))))))
    (APPEND (REST (FIRST CLX)) (CONS (SUBSEQ S (LENGTH (FIRST (FIRST CLX)))) NIL)))		
   (T (RECALL-WPH-1 S (REST CLX)))))

(DEFUN SORT-CLX> (A B)
  (COND 
        ((NULL A) (NOT (NULL B)))
	((NULL B) NIL)
	((> (LENGTH (FIRST A)) (LENGTH (FIRST B))))))

; -----------------------------------------------------------------------------

;   PROBLEM 4: EXT-WK-MEM
; EXT-WK-MEM creates a new atom (using UNIQUE-GAP with a base name of CON),
; which it binds to the passed FRAMI. It adds the CON atom to the global list
; WK-MEM.
; INPUT: frami (frame): frame to be bind a new CON-atom to
; OUTPUT: frami
; SIDE-EFFECT: Adds generated CON atom to the end of the global list WK-MEM

(defun EXT-WK-MEM (frami)
  (LET* ()
    (SETQ WK-MEM-TMP (UNIQUE-GAP 'CON))
    (SET WK-MEM-TMP FRAMI)
    (COND 
     ((NULL WK-MEM) (SETQ WK-MEM (LIST WK-MEM-TMP)))
      (T (SETQ WK-MEM (APPEND WK-MEM (LIST WK-MEM-TMP)))))
    (FIRST (LAST WK-MEM))))

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
  (LET*()  
     (COND 
	((NULL DEMONS) NIL)
	((= 1 (LENGTH DEMONS)) (SPAWN-ADD CON (FIRST DEMONS)))
	(T (APPEND (SPAWN-ADD CON (FIRST DEMONS)) (SPAWN CON (REST DEMONS)))))))

(DEFUN SPAWN-ADD (CON DEMON)
  (LET* ()
	(SETQ SPAWN-DEMI (CONS (FIRST DEMON) (CONS CON (REST DEMON))))
	(COND 
	   ((NULL ACTV-DEMONS) (SETQ ACTV-DEMONS (LIST SPAWN-DEMI)))
	   (T (SETQ ACTV-DEMONS (APPEND ACTV-DEMONS (LIST SPAWN-DEMI)))))
	(LAST ACTV-DEMONS)))
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
; OUTPUT: list of demons still remaining after quiescence is reached

(defun EXEC-DEMONS (demis wkm)
  (COND 
    ((NULL DEMIS) NIL);EMPTY
    ((EQUAL NIL (SETQ EXEC-DEMONS-TMP (EXEC-DEMONS-1 DEMIS)))
     DEMIS); ALL NIL
    (T (EXEC-DEMONS (APPEND (BUTLAST DEMIS  EXEC-DEMONS-TMP)
			    (LAST DEMIS    (- EXEC-DEMONS-TMP 1))) 
		    WKM))))

(defun EXEC-DEMONS-1 (demis)
  (COND
   ((null DEMIS) 0)
   ((apply (first (FIRST DEMIS)) (rest (first demis))) (LENGTH DEMIS))
   ((EQUAL 1 (LENGTH DEMIS)) NIL)
   (T (EXEC-DEMONS-1 (REST DEMIS)))))

; -----------------------------------------------------------------------------

;   PROBLEM 21: TOP-CON
; TOP-CON searches through a working memory list to look for CON-atoms which are
; not bound (i.e., do not appear in the global list BOUND). It returns the
; CON-atom with the greatest number of slots.
; INPUT: wkm (list): working memory to search

(defun TOP-CON (wkm)
  (SETQ TOP-CON-TMP-LIST (TOP-CON-LIST WKM))
  (SETQ TOP-CON-TMP-VALUE (LOOP FOR I IN TOP-CON-TMP-LIST  MAXIMIZE (NUM-SLOTS I)))
  (TOP-CON-WHICH TOP-CON-TMP-LIST TOP-CON-TMP-VALUE))
(DEFUN TOP-CON-WHICH (LIST VALUE)
  (COND 
    ((NULL LIST) NIL)
    ((EQUAL VALUE (NUM-SLOTS (FIRST LIST))) (FIRST LIST))
    (T (TOP-CON-WHICH (REST LIST) VALUE)))) 

(DEFUN TOP-CON-LIST (WKM)
  (COND
    ((NULL WKM) NIL)
    ((> (LENGTH WKM) 1) 
         (COND 
	  ((TOP-CON-IF-NOT-CONTAIN (FIRST WKM) BOUND)
	   (APPEND (LIST (FIRST WKM)) (TOP-CON-LIST (REST WKM))))
	  (T (TOP-CON-LIST (REST WKM)))))
    ((TOP-CON-IF-NOT-CONTAIN (FIRST WKM) BOUND) 
     (LIST (FIRST WKM)))
    (T NIL)))

(DEFUN TOP-CON-IF-NOT-CONTAIN (CON B);T IF BOUND DOES NOT CONTAIN CON
  (COND 
   ((NULL B) T)
   ((EQUAL (FIRST B) CON) NIL)
   (T (TOP-CON-IF-NOT-CONTAIN CON (REST B)))))
; -----------------------------------------------------------------------------

;   PROBLEM 22: C-ANALYZE
; C-ANALYZE is the top-most level function, that will return a frame representation
; of a sentence represented as a sequence of words. 
; It loops through attempting to match the words of SENT to words or phrases in the lexicon, LEX, using RECALL-WPH. 
;    For each recognized phrase, it TOKENIZEs (problem 3) the associated frame, 
;   adds the CON atom of that frame to working memory with EXT-WK-MEM (problem 4),
;    SPAWNs (problem 5) the associated demon list,
;     and invokes EXEC-DEMONS (problem 8) on the global list ACTV-DEMONS, which it should update with the list EXEC-DEMONS returns
; (since some demons may have been removed). This process repeats until SENT is empty.
; C-ANALYZE then returns the GAPVALS of the TOP-CON of working memory as it stands
; after loading in the complete sentence.
; INPUT: sent (list): list of words (as atoms) representing a sentence
;        lex (list): a conceptual lexicon (see problem 1)


(defun C-ANALYZE (sent lex)
   (LET* ()
	 (SETQ C-ALL (RECALL-WPH SENT LEX))
	 (SPAWN (EXT-WK-MEM  (TOKENIZE (FIRST C-ALL))) (SECOND C-ALL))
	 (EXEC-DEMONS ACTV-DEMONS WK-MEM)
	 (COND 
	  ((NULL  (THIRD C-ALL)) (gapvals (TOP-CON WK-MEM)))
	  (T (C-ANALYZE  (THIRD C-ALL) LEX)))))

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

; D-LAST-NAME looks at the CON after it's MYCON in working memory. If the frame
; there has a predicate of UNKNOWN, then BIND (problem 10) MYSLOT in MYCON frame
; to the IS slot of the "UNKNOWN" frame.
; INPUT: mycon(atom) - demon's frame in working memory
;        myslot(atom) - slot in MYCON to bind the "IS" of an UNKNOWN frame to
; Returns success if it found a frame with UNKNOWN

(defun D-LAST-NAME (mycon myslot)
   (COND 
	((EQUAL 'UNKNOWN (FIRST (EVAL (NTH (+ 1 (D-LAST-NAME-FIND-POSITION WK-MEM MYCON)) WK-MEM)))) 
	  (LET ()
	       (SET (FIND-SLOT (eval MYCON) MYSLOT) (SECOND (SECOND (EVAL (NTH (+ 1 (D-LAST-NAME-FIND-POSITION WK-MEM MYCON)) WK-MEM)))))
	       t))
       (T NIL)))

(DEFUN D-LAST-NAME-FIND-POSITION (WKM CON)
  (COND 
   ((NULL WKM) 0)
   ((EQUAL CON (FIRST WKM)) 0)
   (T (+ 1 (D-LAST-NAME-FIND-POSITION (REST WKM) CON)))))

; -----------------------------------------------------------------------------

;   PROBLEM 11: D-FILL

; D-FILL searches in DIR starting from MYCON looking for a frame that has
; predicate PRED (using SEARCH-WKM; see problem 6). If found, BIND the GAP in
; MYCON located at PATH to the CON-atom of the found frame.
; INPUT: mycon(atom) - demon's frame in working memory
;        path(list) - location of gap to be bound, as list of slots
;        dir(AFT|BEF) - direction to search for PRED
;        pred(atom) - predicate to search for
; Returns success if it found PRED

(defun D-FILL (mycon path dir pred)
   (cond
	 ((SETQ D-FILL-TMP (SEARCH-WKM WK-MEM MYCON DIR PRED))  
	  (BIND (PATH-SL PATH (EVAL MYCON)) D-FILL-TMP))
	 (T NIL)))
; -----------------------------------------------------------------------------

;   PROBLEM 12: D-AFT-PRED
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
  (LET ((A (SEARCH-WKM WK-MEM MYCON 'AFT PRED)))
       (COND
	 ((NULL CONSTRAINTS) NIL)
	 ((SETq D-AFT-PRED-TMP (SEARCH-WKM-AFTER-PREDS WK-MEM 
						       (+ 1 (SEARCH-WKM-FIND-POSITION WK-MEM MYCON)) 
							  CONSTRAINTS))
	  (BIND (PATH-SL MYPATH (EVAL MYCON)) D-AFT-PRED-TMP)))))

(DEFUN SEARCH-WKM-AFTER-PREDS (WKM NUM PREDS)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((SETQ SEARCH-WKM-AFTER-PREDS-TMP (SEARCH-WKM-AFTER-PREDS-1 WKM NUM PREDS)) SEARCH-WKM-AFTER-PREDS-TMP)
     (T (SEARCH-WKM-AFTER WKM (+ 1 NUM) PREDS))))

(DEFUN SEARCH-WKM-AFTER-PREDS-1 (WKM NUM PREDS)
 (COND 
     ((NULL PREDS) NIL)
     ((ISA? (FIRST (EVAL (NTH NUM WKM))) (FIRST PREDS) ONT) (NTH NUM WKM))
     (T (SEARCH-WKM-AFTER-PREDS-1 WKM NUM (REST PREDS)))))

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
      (COND 
         ((eval (PATH-SL PATH1 (EVAL MYCON))) 
	 (SET (PATH-SL PATH2 (EVAL MYCON)) 
	      (eval (PATH-SL PATH1 (EVAL MYCON)))))
	 (T NIL)))

; -----------------------------------------------------------------------------

;   PROBLEM 14: D-POSS-PRO-REF
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
  (cond 
   ((SETQ D-POSS-PRO-REF-TMP (SEARCH-WKM-PATH WK-MEM MYCON DIR PATH PRED))
    (BIND MYSLOT D-POSS-PRO-REF-TMP))
   (t NIL))
)

(defun SEARCH-WKM-path (wkm con dir PATH pred)
   (LET* ()
	 (COND 
	    ((EQUAL 'AFT DIR)  (SEARCH-WKM-AFTER-PATH WKM (+ 1 (SEARCH-WKM-FIND-POSITION WKM CON)) PATH PRED)) 
	    ((EQUAL 'BEF DIR)  (SEARCH-WKM-BEFORE-PATH WKM (- (SEARCH-WKM-FIND-POSITION WKM CON)1) PATH PRED))
	    (T 'INVALID_DIR))))

(DEFUN SEARCH-WKM-AFTER-path (WKM NUM PATH PRED)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((EQUAL (PATH-SL-WITH-PROTECTION PATH (EVAL (NTH NUM WKM))) PRED)  (NTH NUM WKM))
     (T (SEARCH-WKM-AFTER-PATH WKM (+ 1 NUM) PATH PRED))))

(DEFUN SEARCH-WKM-BEFORE-path (WKM NUM PATH PRED)
  (COND 
     ((null wkm) NIL)
     ((> 0 num) NIL)
     ((EQUAL (PATH-SL-WITH-PROTECTION PATH (EVAL (NTH NUM WKM))) PRED)  (NTH NUM WKM))
     (T (SEARCH-WKM-BEFORE-PATH WKM (- NUM 1) PATH PRED))))


(defun PATH-SL-WITH-PROTECTION (slots concept)
  (LET* ()
	(COND 
	 ((ATOM CONCEPT) NIL)
	 ((NULL (second concept)) NIL)
	 ((equal (first slots) (first (second concept))) 
	               (cond
			( (null (rest slots))  (second (second concept)))
			( t  (path-sl-WITH-PROTECTION (rest slots)  (second (second concept)) ))))
	 (t      (path-sl-WITH-PROTECTION slots (rest concept))))))
; -----------------------------------------------------------------------------

;   PROBLEM 15: D-IMMED-F-NAME
; D-IMMED-F-NAME looks in the frame immediately after MYCON. If that frame
; has a top-level slot F-NAME, then bind the gap of the F-NAME slot in MYCON
; to the filler of F-NAME in the neighboring frame.
; INPUT: mycon(atom) - demon's frame in working memory
; Returns success if the next frame has an F-NAME slot

(defun D-IMMED-F-NAME (mycon)
  (COND
	((SETQ D-IMMED-TMP (NTH (+ (SEARCH-WKM-FIND-POSITION WK-MEM MYCON) 1) WK-MEM))
	 (COND 
	  ((SETQ D-IMMED-TMP-2 (FILLER 'F-NAME (EVAL D-IMMED-TMP)))
	   (SETq (FIND-F-NAME (EVAL MYCON)) d-immed-tmp-2))
	  (T NIL)))
	(T NIL)))

(defun find-f-name (mycon)
  (COND 
     ((NULL MYCON) NIL)
     ((ATOM MYCON) NIL)
     ((EQUAL (FIRST MYCON) 'F-NAME) (SECOND MYCON))
     ( (FIND-F-NAME (FIRST MYCON))  (FIND-F-NAME (FIRST MYCON)))
     (T (FIND-F-NAME (REST MYCON)))))
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
  (COND 
   ((SETQ D-POSS-PRO-TMP (SEARCH-WKM-POSS-PRO WK-MEM MYCON DIR))
    (BIND (PATH-SL-WITH-PROTECTION PATH (EVAL MYCON)) D-POSS-PRO-TMP))
   (T NIL)))


(defun SEARCH-WKM-POSS-PRO (wkm con dir)
   (LET* ()
	 (COND 
	    ((EQUAL 'AFT DIR)  (SEARCH-WKM-AFTER-POSS-PRO WKM (+ 1 (SEARCH-WKM-FIND-POSITION WKM CON)))) 
	    ((EQUAL 'BEF DIR)  (SEARCH-WKM-BEFORE-POSS-PRO WKM (- (SEARCH-WKM-FIND-POSITION WKM CON) 1)))
	    (T 'INVALID_DIR))))

(DEFUN SEARCH-WKM-AFTER-POSS-PRO (WKM NUM)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((SEARCH-WKM-PRONOUN (EVAL (NTH NUM WKM))) (FIND-REF (EVAL (NTH NUM WKM))))
     (T (SEARCH-WKM-AFTER-POSS-PRO WKM (+ 1 NUM)))))

(DEFUN SEARCH-WKM-BEFORE-POSS-PRO (WKM NUM)
  (COND 
     ((null wkm) NIL)
     ((> 0 num) NIL)
     ((SEARCH-WKM-PRONOUN (EVAL (NTH NUM WKM))) (FIND-REF (EVAL (NTH NUM WKM))))
     (T (SEARCH-WKM-BEFORE-POSS-PRO WKM (- NUM 1)))))

(DEFUN SEARCH-WKM-PRONOUN (CON)
  (COND 
   ((ATOM CON) NIL)
   ((NOT (EQUAL 'PRONOUN (FIRST CON))) NIL)
   ((SEARCH-WKM-TYPE-POSS (REST CON)) T)
   (T NIL)))
    
(DEFUN SEARCH-WKM-TYPE-POSS (CON)
  (COND
     ((NULL CON) NIL)
     ((EQUAL (FIRST CON) '(TYPE (POSS))) T)
     (T (SEARCH-WKM-TYPE-POSS (REST CON)))))

(defun find-REF (mycon)
  (COND 
     ((NULL MYCON) NIL)
     ((ATOM MYCON) NIL)
     ((ATOM (FIRST MYCON)) (FIND-REF (REST MYCON)))
     ((EQUAL (FIRST (FIRST MYCON)) 'REF) (SECOND (FIRST MYCON)))
     (T (FIND-REF (REST MYCON)))))
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
  (COND 
   ((SETQ D-ATTACH-SF-TMP (SEARCH-WKM-EXACT WK-MEM MYCON DIR PRED))
    (SETQ  D-ATTACH-SF-TMP (APPEND (EVAL D-ATTACH-SF-TMP) (CONS (LIST SLOT FILLER) NIL))))
   (T NIL)))

(defun SEARCH-WKM-EXACT (wkm con dir pred)
   (LET* ()
	 (COND 
	    ((EQUAL 'AFT DIR)  (SEARCH-WKM-AFTER-EXACT  WKM (+ 1 (SEARCH-WKM-FIND-POSITION WKM CON)) PRED)) 
	    ((EQUAL 'BEF DIR)  (SEARCH-WKM-BEFORE-EXACT WKM (- (SEARCH-WKM-FIND-POSITION WKM CON) 1) PRED))
	    (T 'INVALID_DIR))))

(DEFUN SEARCH-WKM-AFTER-EXACT (WKM NUM PRED)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((EQUAL (FIRST (EVAL (NTH NUM WKM))) PRED)  (NTH NUM WKM))
     (T (SEARCH-WKM-AFTER-EXACT WKM (+ 1 NUM) PRED))))

(DEFUN SEARCH-WKM-BEFORE-EXACT (WKM NUM PRED)
  (COND 
     ((null wkm) NIL)
     ((> 0 num) NIL)
     ((EQUAL (FIRST (EVAL (NTH NUM WKM))) PRED)  (NTH NUM WKM))
     (T (SEARCH-WKM-BEFORE-EXACT WKM (- NUM 1) PRED))))

; -----------------------------------------------------------------------------

;   PROBLEM 18: D-PRO-REF
; D-PRO-REF searches in direction DIR for a frame whose filler of SLOT1
; matches the parameter FILLER. If found, then bid the gap of SLOT2 in MYCON
; to the found frame.
; INPUT: mycon (atom) - demon's frame in working memory
;        dir (BEF|AFT) - direction to search
;        slot1 (atom) - slot to check while searching
;        filler (frame|gap) - filler of searched frame's SLOT1 must match this
;        slot2 (atom) - slot in MYCON to be bound to a found frame
; Returns success if a frame matching the above conditions is found

(defun D-PRO-REF (mycon dir slot1 filler slot2)
  (COND 
    ((SETQ D-PRO-REF-TMP (SEARCH-WKM-PRO-REF WK-MEM MYCON DIR SLOT1 FILLER))
     (COND
      ((SETQ D-PRO-REF-TMP-2 (FIND-SLOT (EVAL  MYCON) SLOT2))
       (BIND (FIND-SLOT (EVAL MYCON) SLOT2)  D-PRO-REF-TMP))
      (T NIL)))
    (T NIL)))

(defun SEARCH-WKM-PRO-REF (wkm con dir SLOT FILLER)
   (LET* ()
	 (COND 
	    ((EQUAL 'AFT DIR)(SEARCH-WKM-AFTER-PRO-REF WKM (+ 1(SEARCH-WKM-FIND-POSITION WKM CON))   SLOT FILLER)) 
	    ((EQUAL 'BEF DIR)(SEARCH-WKM-BEFORE-PRO-REF WKM (-( SEARCH-WKM-FIND-POSITION WKM CON)1) SLOT FILLER))
	    (T 'INVALID_DIR))))

(DEFUN SEARCH-WKM-AFTER-PRO-REF (WKM NUM SLOT FILLER)
  (COND 
     ((null wkm) NIL)
     ((>= num (length wkm)) NIL)
     ((FIND-SLOT-FILLER (EVAL (NTH NUM WKM)) SLOT FILLER)  (NTH NUM WKM))   
     (T (SEARCH-WKM-AFTER-PRO-REF WKM (+ 1 NUM) SLOT FILLER))))

(DEFUN SEARCH-WKM-BEFORE-PRO-REF (WKM NUM SLOT FILLER)
  (COND 
     ((null wkm) NIL)
     ((> 0 num) NIL)
     ((FIND-SLOT-FILLER (EVAL (NTH NUM WKM)) SLOT FILLER)  (NTH NUM WKM))   
     (T (SEARCH-WKM-BEFORE-PRO-REF WKM (- NUM 1) SLOT FILLER))))


(defun find-SLOT-FILLER (mycon SLOT FILLER)
  (COND 
     ((NULL MYCON) NIL)
     ((ATOM MYCON) NIL)
     ((EQUAL (FIRST MYCON) SLOT)
      (COND
          ((ATOM (SECOND MYCON)) NIL)
          ((EQUAL (FIRST (SECOND MYCON)) FILLER) T)
	  (T NIL)))
     ((FIND-SLOT-FILLER (FIRST MYCON) SLOT FILLER) (FIND-SLOT-FILLER (FIRST MYCON) SLOT FILLER))
     (T (FIND-SLOT-FILLER (REST MYCON) SLOT FILLER))))

(defun find-SLOT (mycon SLOT)
  (COND 
     ((NULL MYCON) NIL)
     ((ATOM MYCON) NIL)
     ((EQUAL (FIRST MYCON) SLOT) (SECOND MYCON))
     ((FIND-SLOT (FIRST MYCON) SLOT) (FIND-SLOT (FIRST MYCON) SLOT))
     (T (FIND-SLOT (REST MYCON) SLOT ))))
; -----------------------------------------------------------------------------

;   PROBLEM 19: D-IMM-AFT
; D-IMM-AFT looks at the frame immediately after MYCON. If there is a non-NIL
; filler of SLOT in that frame, then bind the gap of the same SLOT in mycon with
; that filler.
; INPUT: mycon (atom) - demon's frame in working memory
;        slot (atom) - slot to check, and fill in mycon

(defun D-IMM-AFT (mycon slot)
  (COND 
    ((SETQ D-IMM-AFT-TMP  
	  (FIND-SLOT (eval ( NTH (+ 1 (SEARCH-WKM-FIND-POSITION WK-MEM myCON)) wk-mem )) SLOT))
     (SET (FIND-SLOT (EVAL MYCON) SLOT) D-IMM-AFT-TMP))
    (T NIL)))

; -----------------------------------------------------------------------------

; ****** END SECTION 3 ******

;================================================================================
;================================================================================

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