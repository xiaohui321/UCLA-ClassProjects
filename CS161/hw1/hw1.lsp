;==========================
; CS 161 Spring 2013
; HW1 
;==========================
; LAST  NAME: XIAOHUI
; FIRST NAME: ZHOU
; STUDENT ID: 104-014-248
;==========================

;------------------------------------------------------------------------------
; Finished
; FUNCTION: FILLER
; PURPOSE:  Return the FILLER of the given SLOT in the given FRAME
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
; INPUTS:   slot: atom
;           frame: FRAME

(defun FILLER (slot frame)
  (let* ( (a (second frame)) )
           (cond
	         ((NULL a)                 NIL)
		 ((equal slot (first a))   (second a))
		 (t                        (filler slot (rest frame))))))
; -----------------------------------------------------------------------------
; Finished
; FUNCTION: PATH-SL
; PURPOSE:  Return a FILLER accessed by following a path of named SLOTS within
;           the given CONCEPT
; OUTPUT:   FILLER of the CONCEPT at the given path, or NIL if not present
; INPUTS:   slots: list (path of SLOTs to follow)
;           concept: FRAME

(defun PATH-SL (slots concept)
  (LET* ((a (second concept)))
	(COND 
	 ((NULL a) NIL)
	 ((equal (first slots) (first a)) 
	               (cond
			( (null (rest slots))  (second a))
			( t  (path-sl (rest slots)  (second a) ))))
	 (t      (path-sl slots (rest concept))))))
; -----------------------------------------------------------------------------
; Finished
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
  (let* ()
    (cond
     ( (not (equal (first path) (first concept)))'FAIL) 
     ( t   (path-ps-a (rest path) concept)))))

(defun path-ps-a (path concept)
  (let* ((a (second concept)))
    (cond
         ((Null a) NIL)
         ((equal (first path) (first a))
	    (cond 
	       ( (null (rest path)) (second a))
	       (t (path-ps (rest path) (second a)))))
	 (t (path-ps-a path (rest concept))))))
; -----------------------------------------------------------------------------
; Finished
; FUNCTION: GAPVALS
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, except that
;           any GAPs have been replaced by their values
; OUTPUT:   FRAME, with all GAPs replaced
; INPUTS:   frame: FRAME to evaluate GAPs in

(defun GAPVALS (frame)
  (let* ()
    (cond
     ((null frame) ())
     ((atom frame) 
        (cond
	    ((symbolp frame) (GAPVALS (eval frame)))        ; symbol
	    (t frame)))                                     ; value
     ((= (length frame) 1) (list (first frame)))            ; list of 1 element
     (t                                                     ; list of elements 
	  (append
	   (list (first frame))
	   (GAPVALS-1 (rest frame)))))))

(defun GAPVALS-1 (frame)
  (let* ()
    (cond 
     ((null frame) ())                                     ; null list
     ( (= (length frame) 1) (list (GAPVALS (first frame)))) ; list of 1 element
     ( t                                                    ; list of elements
      (append
	 (list (GAPVALS (first frame)))
	 (GAPVALS-1 (rest frame)))))))
; -----------------------------------------------------------------------------
; Finished
; FUNCTION: REPLACE-SF
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; OUTPUT:   Copy of FRAME with the top-level SLOT slot filled with filler
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           frame: FRAME (frame to be modified)

(defun REPLACE-SF (slot filler frame)
  (let* ()
    (cond
     ( (null slot) NIL)
      ((replace-sf-occur slot frame) 
          (append 
	         (butlast frame (- (length frame) (replace-sf-position slot frame)))  
		 (cons (list slot filler) nil)
		 (nthcdr (+ (replace-sf-position slot frame) 1) frame)))
      ( t (append frame (cons (list slot filler) nil))))))

(defun REPLACE-SF-occur (slot frame)
  (let* ((a (second frame)))
    (cond
      ((null a) NIL)
      ((equal slot (first a)) t)
      (t (replace-sf-occur slot (rest frame))))))

(defun REPLACE-SF-position (slot frame)
  (let* ()
    (cond 
     ((equal slot (first (second frame))) 1)
     (t (+ 1 (replace-sf-position slot (rest frame)))))))
; -----------------------------------------------------------------------------
; Finished 
; FUNCTION: REPLACE-PTH
; PURPOSE:  Return a FRAME which is a copy of the reference frame, with the
;           SLOT given in the designated PATH filled in with the given FILLER
;           (or added if that SLOT does not exist)
; OUTPUT:   Copy of FRAME with SLOT in path filled with filler
; INPUTS:   path: list (path of SLOTS to follow)
;           filler: FILLER (value to put in SLOT referenced by path)
;           frame: FRAME (frame to be modified) 

(defun REPLACE-PTH (path filler frame) 
   (let* ((slot (first path))(nn (replace-sf-position slot frame)))
     (cond 
      ((null path) NIL)
      ((replace-sf-occur slot  frame) 
       (cond
           ( (= 1 (length path))  (append 
				   (butlast frame (- (length frame) nn ))  
				   (cons (list slot filler) nil)
				   (nthcdr (+ nn  1) frame)))
	   (t (append 
	       (butlast frame (- (length frame) nn ))  
	       (cons (list (first (nth nn frame))
			   (replace-pth (rest path) filler (first ( rest (nth nn frame))))) nil)
	       (nthcdr (+ nn 1) frame)))))
      (t  (append frame (cons (list slot filler) nil))))))
; -----------------------------------------------------------------------------
; Finished
; FUNCTION: COMPARE-FRMS
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)

(defun COMPARE-FRMS (frame1 frame2)
  (let* ()
    (cond
     ((not (= (length frame1) (length frame2))) NIL)
     (t (compare-frms-1 frame1 frame2)))))

(defun COMPARE-FRMS-1 (frame1 frame2)
  (let* ()
    (cond
       ((compare-frms-not-contain (second frame1) frame2) NIL)
       ((NULL (rest (rest frame1))) T)
       (t (compare-frms-1 (rest frame1) frame2)))))

(defun compare-frms-not-contain (slot frame)
  (let* ()
    (cond 
       ((null frame) T)
       ((equal slot (first frame)) NIL)
       (t (compare-frms-not-contain slot (rest frame))))))
;-------------------------------------------------------------------------------