;;;=============================================================================
;;; Auto Art Maker
;;;  - Leif Rogers
;;;  - or, rather, a system for declaring readymades
;;;  - according to my process - randomized
;;;
;;;
;;;
;;;=============================================================================

(deftemplate person
  (slot firstname)
  (slot lastname)
  (slot connection)
  (slot relationship)
  (multislot association))

(deftemplate mood
  (slot typeof)
  (slot association))

(deftemplate mood-strength
  (slot strength))

(deftemplate senses
  (slot typeof)
  (slot influence))

(deftemplate time-of-day
  (slot time)
  (multislot association))

(deftemplate influence-person
  (slot firstname)
  (slot lastname)
  (slot typeof))

(deftemplate influence-other
  (slot typeof)
  (multislot comments))

(deftemplate general-locations
  (slot location)
  (multislot association))

(deftemplate detailed-locations
  (slot general-location)
  (slot detailed-location)
  (slot foreground)
  (slot background)
  (multislot details)
  (multislot association))

(deftemplate global-subject
  (slot subject))

(deftemplate detailed-subject
  (slot subject)
  (slot location)
  (slot foreground)
  (slot background)
  (slot palette)
  (multislot accessories))

(deftemplate global-medium
  (slot medium))

(deftemplate detailed-medium
  (slot typeof)
  (slot medium))

(deftemplate memory
  (slot description)
  (multislot association))

(deftemplate chosen
  (slot thing)
  (multislot selection))

; more template options tk - senses, motivation, palettes, etc.

;;;=============================================================================
;;; functions
;;;=============================================================================

(deffunction ask-question (?question $?allowed-values)
  (printout t ?question crlf)
  (printout t $?allowed-values crlf)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question crlf)
      (printout t $?allowed-values crlf)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)


; takes a list of facts (specifically from a template) and groups the chosen
; slot into one consolidated fact for ease of randomness and choosing, etc.

(deffunction consolidate-facts (?template ?factslot)
  (bind ?facts (create$))
  (if (eq (deftemplate-slot-existp ?template ?factslot) TRUE) then
								(do-for-all-facts ((?fct ?template)) TRUE
	                (bind ?innerfact (fact-slot-value ?fct ?factslot))
	                (bind ?facts (insert$ ?facts 1 ?innerfact)))
	   (assert (consolidated ?facts))
   else (printout t "slot doesn't exist")
  	(printout t (deftemplate-slot-names ?template))))


; counts facts from any given template and returns the number
; used by consolidate-facts for the max limit of random

(deffunction count-facts (?template)
  (bind ?count 0)
  (do-for-all-facts ((?fct ?template)) TRUE
    (bind ?count (+ ?count 1))))


; gets a random fact from the given template
; i'm sure there's a better way to filter out the different facts
; and assign the chosen easier but that's for another time

(deffunction get-random-fact (?template)
  (bind ?max (count-facts ?template))
  (bind ?selection (random 0 ?max))
  (bind ?count 0)
  (do-for-all-facts ((?fct ?template)) TRUE
    (if (= ?count ?selection) then
      (if (= (str-compare "person" ?template) 0) then 
        (printout t ?fct:firstname) (assert
								   (chosen
								    (thing
								     person)(selection 
								    ?fct:firstname 
								    ?fct:lastname))))
      (if (= (str-compare "global-medium" ?template) 0) then
        (printout t ?fct:medium) (assert
								  (chosen (thing
									   media)
									  (selection
									   ?fct:medium))))
      (if (= (str-compare "mood" ?template) 0) then
        (printout t ?fct:typeof) (assert
								  (chosen (thing
									   mood)
									  (selection
									   ?fct:typeof))))
      (if (= (str-compare "mood-strength" ?template) 0) then
        (printout t ?fct:strength) (assert
								  (chosen (thing
									   mood-strength)
									  (selection
									   ?fct:strength))))
      (if (= (str-compare "time-of-day" ?template) 0) then
        (printout t ?fct:time) (assert
								  (chosen (thing
									   time-of-day)
									  (selection
									   ?fct:time))))
      (if (= (str-compare "global-subject" ?template) 0) then
        (printout t ?fct:subject) (assert
								  (chosen (thing
									   global-subject)
									  (selection
									   ?fct:subject)))))                						
    (bind ?count (+ ?count 1))
  ))


(deffunction get-detailed-medium (?template)
   (do-for-all-facts ((?p detailed-medium)) (= (str-compare ?p:typeof ?template) 0)
     (println ?p:medium))
   )

(deffunction find-last-name (?template)
   (do-for-all-facts ((?p person)) (= (str-compare ?p:lastname ?template) 0)
     (println ?p:firstname " " ?p:lastname))
   )

;;;=============================================================================
;;; rules
;;;
;;; mood before time, time before senses, senses before motivation
;;; mood and time affect medium.
;;; motivation, generally, is unimportant
;;;=============================================================================

(defrule startup
 =>
  (assert (chosen
	   (thing creation)
	   (selection
	    (ask-question "Choose creation method:" manual
			  random)))))

; defines mood
; influencial for color choices and subjects
(defrule moodiness
 (chosen (thing creation)(selection ?x))
 =>
 (if (eq "manual" ?x) then 
  (bind ?choice (consolidate-facts mood typeof))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen
	   (thing mood)
	   (selection
	    (ask-question "What's your mood" $?choices))))
  (retract ?choice)
  else (get-random-fact mood))
)

; how strong the mood is - not fully implemented yet
(defrule mood-strength
  (chosen (thing mood)(selection ?x))
 =>
 (if (eq "manual" ?x) then 
  (bind ?choice (consolidate-facts mood-strength strength))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen (thing mood-strength)(selection (ask-question "How strong is your mood?"
							      $?choices))))
  (retract ?choice)
    else
    (get-random-fact mood-strength)))

(defrule get-time
  (chosen (thing creation)(selection ?x))
  (chosen (thing mood)(selection ?y))
  (chosen (thing mood-strength)(selection ?z))
 =>
 (if (eq "manual" ?x) then 
  (bind ?choice (consolidate-facts time-of-day time))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen (thing time-of-day)(selection (ask-question "What time is it?"
							      $?choices))))
  (retract ?choice)
  else
    (get-random-fact time-of-day)))

(defrule get-media
  (chosen (thing creation)(selection ?x))
  (chosen (thing time-of-day)(selection ?y))
 =>
 (if (eq "manual" ?x) then 
  (bind ?choice (consolidate-facts time-of-day time))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen (thing media) (selection (ask-question "Pick media: " $?choices))))
  (retract ?choice)
  else
    (get-random-fact global-medium)))

(defrule get-subject
  (chosen (thing creation)(selection ?x))
  (chosen (thing media) (selection ?y))
 =>
  (if (eq "manual" ?x) then 
    (bind ?choice (consolidate-facts global-subject subject))
    (bind $?choices (fact-slot-value ?choice implied))

    (assert (chosen (thing subject)
	  	  (selection
		     (ask-question "Select a subject "
			  	 $?choices ))))
    (retract ?choice)
    else (get-random-fact global-subject)))

(defrule find-landscape
  (chosen (thing creation)(selection ?x))
  (chosen (thing subject) (selection landscape))
  =>
 (if (eq "random" ?x) then 
  (printout t "this is random")
  else (printout t "this is not random"))
)


(defrule get-random-portrait
  (chosen (thing creation) (selection random))
  (chosen (thing subject) (selection portrait))
  =>
  (get-random-fact person))



;;;=============================================================================
;;; facts
;;;=============================================================================

(deffacts all-the-knowledge

  (mood (typeof depressed)(association 1))
  (mood (typeof blue)(association 2))
  (mood (typeof melancholy)(association 3))
  (mood (typeof bored)(association 4))
  (mood (typeof reflective)(association 5))
  (mood (typeof nostalgic)(association 6))
  (mood (typeof happy)(association 7))
  (mood (typeof curious)(association 8))
  (mood (typeof dreamy)(association 9))
  (mood (typeof mad)(association 10))
  (mood (typeof sad)(association 11))
  (mood (typeof angry)(association 12))
  (mood (typeof indifferent)(association 13))
  (mood (typeof disassociated)(association 14))

  (mood-strength (strength strong))
  (mood-strength (strength weak))
  (mood-strength (strength medium))

  (time-of-day (time morning)(association 6))
  (time-of-day (time noon)(association 6))
  (time-of-day (time afternoon)(association 6))
  (time-of-day (time evening)(association 6))
  (time-of-day (time night)(association 6))
  (time-of-day (time midnight)(association 6))
  (time-of-day (time 3am)(association 6))
  
  (global-medium (medium painting))
  (global-medium (medium drawing))
  (global-medium (medium digital))

  (detailed-medium (typeof digital)(medium code))
  (detailed-medium (typeof digital)(medium glitch))
  (detailed-medium (typeof digital)(medium photoshop))

  (detailed-medium (typeof painting)(medium oil))
  (detailed-medium (typeof painting)(medium acrylic))
  (detailed-medium (typeof painting)(medium watercolor))

  (detailed-medium (typeof drawing)(medium graphite))
  (detailed-medium (typeof drawing)(medium charcoal))
  (detailed-medium (typeof drawing)(medium conte))
  (detailed-medium (typeof drawing)(medium colored-pencil))
  (detailed-medium (typeof drawing)(medium oil-pastel))
  
  (global-subject (subject landscape))
  (global-subject (subject portrait))
  (global-subject (subject memory))
  (global-subject (subject genre))
  (global-subject (subject figure))


;;;==========================================================================
;;;Family Connections
;;;==========================================================================
  (person (firstname Leif)(lastname Rogers)(connection family)(relationship myself)(association 1))
  (person (firstname Julie)(lastname Rogers)(connection family)(relationship wife)(association 6))
  (person (firstname Luke)(lastname Rogers)(connection family)(relationship son)(association 7))
  (person (firstname Eric)(lastname Rogers)(connection family)(relationship brother)(association 6))
  (person (firstname Charles)(lastname Rogers)(connection family)(relationship nephew)(association 6))
  (person (firstname Larry)(lastname Rogers)(connection family)(relationship father)(association 6))
  (person (firstname Claire)(lastname Rogers)(connection family)(relationship mother)(association 6))
  (person (firstname Marie)(lastname Seyller)(connection family)(relationship grandmother)(association 6))
  (person (firstname Charles)(lastname Seyller)(connection family)(relationship grandfather)(association 6))
  (person (firstname Justin)(lastname Hastings)(connection neighborhood)(relationship friend)(association 6))
  

;;;===============================================================================
;;; Church connections
;;;===============================================================================
  (person (firstname Gary)(lastname Fehrenbach)(connection church)(relationship friend)(association 6))
  (person (firstname Bob)(lastname Kasperson)(connection church)(relationship pastor)(association 6))	
  (person (firstname Tom)(lastname Skogen)(connection church)(relationship friend)(association 6))	
  (person (firstname Vi)(lastname Skogen)(connection church)(relationship friend)(association 6))
  (person (firstname Jim)(lastname Steinwart)(connection church)(relationship friend)(association 6))	
  (person (firstname Barb)(lastname Steinwart)(connection church)(relationship friend)(association 6))
)