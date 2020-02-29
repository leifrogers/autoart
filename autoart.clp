;;;============================================================================
;;; Auto Art Maker
;;; or, rather, a system for declaring readymades
;;; according to
;;; my process - randomized
;;;
;;;
;;;
;;;===========================================================================

(deftemplate person
  (slot firstname)
  (slot lastname)
  (slot connection)
  (slot relationship))

(deftemplate mood
  (slot typeof))

(deftemplate senses
  (slot typeof)
  (slot influence))

(deftemplate time-of-day
  (slot time))


(deftemplate influence-person
  (slot firstname)
  (slot lastname)
  (slot typeof))

(deftemplate influence-other
  (slot typeof)
  (multislot comments))

(deftemplate general-locations
  (slot location))

(deftemplate detailed-locations
  (slot location)
  (slot meaning)
  (slot foreground)
  (slot background)
  (multislot details))

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

(deftemplate chosen
  (slot thing)
  (multislot selection))

; more template options - senses, mood, time of day, motivation

;;;=============================================================================
;;; functions
;;;=============================================================================

(deffunction ask-question (?question $?allowed-values)
  (printout t ?question)
  (printout t $?allowed-values)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

; takes a list of facts (specifically from a template) and groups the chosen
; slot into one consolidated fact
(deffunction consolidate-facts (?template ?factslot)
  (bind ?facts (create$))
  (if (eq (deftemplate-slot-existp ?template ?factslot) TRUE) then
								(do-for-all-facts ((?fct ?template)) TRUE
	     (bind ?innerfact (fact-slot-value ?fct ?factslot))
	     (printout t ?innerfact)
	     (bind ?facts (insert$ ?facts 1 ?innerfact)))
	   (assert (consolidated ?facts))
   else (printout t "slot doesn't exist")
	(printout t (deftemplate-slot-names ?template))))

(deffunction count-facts (?template)
  (bind ?count 0)
  (do-for-all-facts ((?fct ?template)) TRUE
    (bind ?count (+ ?count 1))))

(deffunction get-random-fact (?template)
  (bind ?max (count-facts ?template))
  (bind ?selection (random 0 ?max))
  (bind ?count 0)
  (do-for-all-facts ((?fct ?template)) TRUE
    (if (= (str-compare "person" ?template) 0) then 
      (if (= ?count ?selection) then (printout t ?fct:firstname) (assert
								   (chosen
								    (thing
								     person)(selection 
								    ?fct:firstname 
								    ?fct:lastname)))))
    (if (= (str-compare "global-medium" ?template) 0) then
        (if (= ?count ?selection) then (printout t ?fct:medium) (assert
								  (chosen (thing
									   medium)
									  (selection
									   ?fct:medium)))))
    (if (= (str-compare "mood" ?template) 0) then
        (if (= ?count ?selection) then (printout t ?fct:typeof) (assert
								  (chosen (thing
									   mood)
									  (selection
									   ?fct:typeof)))))							
  (bind ?count (+ ?count 1))
  ))


(deffunction find-last-name (?template)
   (do-for-all-facts ((?p person)) (= (str-compare ?p:lastname ?template) 0)
     (println ?p:firstname " " ?p:lastname))
   )

;;;===========================================================================
;;; rules
;;;
;;; mood before time, time before senses, senses before motivation
;;; mood and time affect medium.
;;; motivation, generally, is unimportant
;;;==========================================================================

(defrule startup
 =>
  (assert (chosen
	   (thing creation)
	   (selection
	    (ask-question "Choose creation method - manual or random? " manual
			  random)))))

(defrule moodiness
 (chosen (thing creation)(selection manual))
 =>
  (bind ?choice (consolidate-facts mood typeof))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen
	   (thing mood)
	   (selection
	    (ask-question "What's your mood" $?choices))))
  (retract ?choice)
)

(defrule mood-strength
  (chosen (thing mood)(selection ?x))
 =>
  (bind ?rando (random 1 3))
  (assert (chosen (thing mood-strength)(selection (nth$ ?rando (create$ strong
									medium weak))))))

(defrule get-time
  (chosen (thing creation)(selection manual))
  (chosen (thing mood)(selection ?x))
  (chosen (thing mood-strength)(selection ?y))
 =>
  (bind ?choice (consolidate-facts time-of-day time))
  (bind $?choices (fact-slot-value ?choice implied))
  (assert (chosen (thing time-of-day)(selection (ask-question "What time is it?"
							      $?choices))))
  (retract ?choice)
)

(defrule get-media
  (chosen
   (thing creation)
   (selection manual))
 (chosen (thing mood)(selection ?x))
 =>
  (assert (chosen
	   (thing media)
	   (selection
	    (ask-question "Pick media (physical/digital) " physical digital)))))

(defrule random-mode
  (chosen (thing creation) (selection random))
 =>
(get-random-fact global-medium)
)

(defrule get-subject
  (chosen (thing media) (selection physical))
 =>
  (bind ?choice (consolidate-facts global-subject subject))
  (bind $?choices (fact-slot-value ?choice implied))

  (assert (chosen (thing subject)
		  (selection
		   (ask-question "Select a subject "
				 $?choices ))))
  (retract ?choice)
)

(defrule get-random-portrait
  (chosen (thing creation) (selection random))
  (chosen (thing subject) (selection portrait))
  =>
  (get-random-fact person))



;;;==============================================================================
;;; facts
;;;==============================================================================

(deffacts all-the-knowledge

  (mood (typeof depressed))
  (mood (typeof blue))
  (mood (typeof melancholy))
  (mood (typeof bored))
  (mood (typeof reflective))
  (mood (typeof nostalgic))
  (mood (typeof happy))
  (mood (typeof curious))
  (mood (typeof dreamy))
  (mood (typeof mad))
  (mood (typeof sad))
  (mood (typeof angry))
  (mood (typeof indifferent))
  (mood (typeof disassociated))

  (time-of-day (time morning))
  (time-of-day (time noon))
  (time-of-day (time afternoon))
  (time-of-day (time evening))
  (time-of-day (time night))
  (time-of-day (time midnight))
  (time-of-day (time 3am))

  
  (global-medium (medium painting))
  (global-medium (medium drawing))
  (global-medium (medium digital))

  (detailed-medium (typeof digital) (medium code))
  (detailed-medium (typeof digital) (medium glitch))
  (detailed-medium (typeof digital) (medium photoshop))

  (detailed-medium (typeof painting) (medium oil))
  (detailed-medium (typeof painting) (medium acrylic))
  (detailed-medium (typeof painting) (medium watercolor))

  (detailed-medium (typeof drawing) (medium graphite))
  (detailed-medium (typeof drawing) (medium charcoal))
  (detailed-medium (typeof drawing) (medium conte))
  (detailed-medium (typeof drawing) (medium colored-pencil))
  (detailed-medium (typeof drawing) (medium oil-pastel))
  
  (global-subject (subject landscape))
  (global-subject (subject portrait))
  (global-subject (subject memory))
  (global-subject (subject genre))
  (global-subject (subject figure))


;;;==========================================================================
;;;Family Connections
;;;==========================================================================
  (person (firstname Leif)(lastname Rogers)(connection family)(relationship
							       myself))
  (person (firstname Julie)(lastname Rogers)(connection family)(relationship
								wife))
  (person (firstname Luke)(lastname Rogers)(connection family)(relationship son))
  
  (person (firstname Eric)(lastname Rogers)(connection family)(relationship
							       brother))
  (person (firstname Charles)(lastname Rogers)(connection family)(relationship
								 nephew))
  (person (firstname Larry)(lastname Rogers)(connection family)(relationship
								father))
  (person (firstname Claire)(lastname Rogers)(connection family)(relationship
								 mother))
  (person (firstname Marie)(lastname Seyller)(connection family)(relationship
								 grandmother))
  (person (firstname Charles)(lastname Seyller)(connection family)(relationship
								   grandfather))
  (person (firstname Justin)(lastname Hastings)(connection neighborhood)
	  (relationship friend))
  

;;;===============================================================================
;;; Church connections
;;;===============================================================================
  (person (firstname Gary)(lastname Fehrenbach)(connection church)(relationship
								   friend))
  (person (firstname Bob)(lastname Kasperson)(connection church)(relationship pastor))	
  (person (firstname Tom)(lastname Skogen)(connection church)(relationship friend))	
  (person (firstname Vi)(lastname Skogen)(connection church)(relationship friend))
  (person (firstname Jim)(lastname Steinwart)(connection church)(relationship friend))	
  (person (firstname Barb)(lastname Steinwart)(connection church)(relationship friend))
)