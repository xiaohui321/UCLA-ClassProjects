
; Section 1: Utility functions:
;   -- CLEAR-GLOBALS (GIVEN): clears the global variables for the project
;   -- RELOAD (GIVEN): clears global variables and reloads your source file
;   -- UNIQUE-GAP (GIVEN): generates a unique symbol name from a base gap name
;   -- TOKENIZE (PROBLEM 3): replaces GAPs with unique values
;   -- SEARCH-WKM (PROBLEM 6): searches working memory for a predicate
;   -- ISA? (PROBLEM 7): checks the type of a predicate
;   -- BIND (PROBLEM 10): binds a GAP to a frame and updates global BOUND list
;   -- NUM-SLOTS (PROBLEM 20): counts a frame's number of slots at all levels
==================================================================================
11111111111111111111111111111111
;
(ADD-LEX '(SALLY) '(HUMAN (GENDER (FEMALE)) (F-NAME (SALLY)) (L-NAME L-NAME)) '((D-LAST-NAME L-NAME)))

(ADD-LEX '(FREDDY)
'(HUMAN (GENDER (MALE))
(F-NAME (FREDDY))
(L-NAME L-NAME))
'((D-LAST-NAME L-NAME)))


=====================================================================================
(setq C-LX-1 
'(((THE) ( )
((D-ATTACH-SF AFT PHYS-OBJ REF (DEF))))
((BIG) (SIZE (VAL (>NORM)))
((D-ATTACH-SF AFT PHYS-OBJ SIZE (<NORM))))
((APPLE) (FRUIT (TYPE (APPLE))) ( ) )
((APPLE PIE) (DESSERT) ( ) )
((THE BIG APPLE) (NYC) ( ) )
))

(setq APS-1  '(THE APPLE WAS FOUND IN THE BIG APPLE))
(setq APS-2  '(APPLE WAS FOUND))
(setq APS-3  '(APPLE PIE IS TASTY))
(setq APS-4  '(THE BIG APPLE IS AN EXCITING PLACE))
(setq APS-5  '(CRAB APPLE))


===============================================================================================
(TOKENIZE '(ST-CHANGE (AGENT AGENT) (FROM (ACQUAINT)) (TO (FRIEND (OF OF)))))
(TOKENIZE '(MTRANS (AGENT AGENT) (RECIP RECIP) (OBJECT (C-CAUSE (ANTE ANTE) (CONSEQ (GOAL-FAILURE (AGENT AGENT) (TIME (FUTURE)))) (JUSTIF JUSTIF)))))

================================================================================================
  (SET  a NIL)
	  (SETQ a (UNIQUE-GAP TARGET))))
============================
(STEAL (AGENT AGENT88) (OBJECT OBJECT99) (TIME (PAST)))

==================================

(SETQ ONT-1 '(
(ISA ACT CONCEPT) (ISA ST-CHANGE CONCEPT)
(ISA STATE CONCEPT) (ISA C-CAUSE CAUSE)
(ISA CAUSE CONCEPT) (ISA MTRANS ACT)
(ISA STEAL ACT) (ISA HUMAN ANIMATE) (ISA CANINE ANIMATE)
(ISA ANIMATE PHYS-OBJ) (ISA VEHICLE PHYS-OBJ)
(ISA PAST TIME) (ISA D-LAST-NAME DEMON)
))
















