;;Base de règles
(defparameter *BR* NIL)

(setq *BR* '(
             (R1 ((equal caracteristiqueChien "compagnie") (>= exercice? 2) (= supporteSolitude 1)) (equal raceDeChien "Doberman"))
             (R2 ((equal caractereIndividu "autoritaire") (>= exercice? 2) (equal possedeJardin? "oui") (<= supporteSolitude 2)) (equal raceDeChien "Beauceron"))
             (R3 ((equal caracteristiqueChien "compagnie") (= supporteSolitude 1)) (equal raceDeChien "BichonFrise"))
             (R4 ((equal caracteristiqueChien "compagnie") (<= supporteSolitude 2)) (equal raceDeChien "Carlin"))
             (R5 ((equal caracteristiqueChien "compagnie") (<= supporteSolitude 3)) (equal raceDeChien "Pekinois"))
             (R6 ((equal caracteristiqueChien "compagnie") (= exercice? 3) (equal typeDeChien "exterieur") (<= supporteSolitude 3)) (equal raceDeChien "MalamuteDeLAlaska"))
             (R7 ((equal caractereIndividu "autoritaire") (equal caracteristiqueChien "compagnie") (>= exercice? 2) (<= supporteSolitude 2)) (equal raceDeChien "Chihuahua"))
             (R8 ((equal caractereIndividu "autoritaire") (equal caracteristiqueChien "compagnie") (>= exercice? 2) (<= supporteSolitude 2)) (equal raceDeChien "Leonberger"))
             (R9 ((equal caracteristiqueChien "guide") (>= exercice? 2) (= supporteSolitude 1)) (equal raceDeChien "Labrador"))
             (R10 ((equal caracteristiqueChien "guide") (equal caractereIndividu "autoritaire") (>= exercice? 2) (<= supporteSolitude 2)) (equal raceDeChien "BergerAllemand"))
             (R11 ((> frequenceSport 1)) (= sportif? 3))
             (R12 ((= frequenceSport 1)) (= sportif? 2))
             (R13 ((< frequenceSport 1)) (= sportif? 1))
             (R14 ((= sportif? 3)) (= exercice? 3))
             (R15 ((= sportif? 2)) (= exercice? 2))
             (R16 ((= sportif? 1)) (= exercice? 1))
             (R17 ((equal possedeJardin? "oui")) (equal typeDeChien "exterieur"))
             (R18 ((= exercice? 3) (equal possedeJardin? "non")) (equal typeDeChien "exterieur")) 
             (R19 ((equal tempsDeTravail "beaucoup") (> nbEnfants 3)) (equal avoirDuTemps? "non"))
             (R20 ((equal tempsDeTravail "beaucoup") (<= nbEnfants 3)) (equal avoirDuTemps? "moyen"))
             (R21 ((equal tempsDeTravail "moyen") (>= nbEnfants 3)) (equal avoirDuTemps? "moyen"))
             (R22 ((equal tempsDeTravail "moyen") (< nbEnfants 3)) (equal avoirDuTemps? "oui"))
             (R23 ((equal tempsDeTravail "faible") (> nbEnfants 3)) (equal avoirDuTemps? "moyen"))
             (R24 ((equal tempsDeTravail "faible") (<= nbEnfants 3)) (equal avoirDuTemps? "oui"))
             (R25 ((equal avoirDuTemps? "non"))  (= supporteSolitude 3))
             (R26 ((equal avoirDuTemps? "moyen"))  (= supporteSolitude 2))
             (R27 ((equal avoirDuTemps? "oui"))  (= supporteSolitude 1))
             (R28 ((> nbEnfants 3)) (equal caractereIndividu "autoritaire"))
             (R29 ((equal caracteristiqueIndividu "aveugle")) (equal caracteristiqueChien "guide"))
             (R30 ((equal caracteristiqueIndividu "age")) (equal caracteristiqueChien "compagnie"))
             (R31 ((equal caracteristiqueIndividu "age")) (equal caracteristiqueChien "guide"))
             (R32 ((equal caracteristiqueIndividu "aucune")) (equal caracteristiqueChien "compagnie"))
             (R33 ((equal caracteristiqueIndividu "aucune")) (equal caracteristiqueChien "guide"))
            )
)

;;Initialisation de la base de fait 
(defparameter *BF* NIL)

;;Renvoie une règle à partir de son étiquette
(defun getRegle (etiquette)
     (assoc etiquette *BR*)
)

;;Renvoie une liste des prémisses d’un règle à partir de son étiquette
(defun getPremisses (etiquette)
     (cadr (getRegle etiquette))
)

;;Renvoie une liste de la conclusion d’un règle à partir de son étiquette
(defun getConclusion (etiquette)
     (cddr (getRegle etiquette))
)

;;Supprime une règle de la base de règle à partir de son étiquette
(defun supprimeRegle (etiquette)
     (if (getRegle etiquette)
         (not (null (setq *BR* (remove (getRegle etiquette) *BR*))))
         nil
     )
)

;;Renvoie la valeur d'un objet dans de la base de fait
(defun getValeur (objet)
     (if (or (numberp objet) (stringp objet))
         objet
         (cadr (assoc objet *BF*))
     )
)

;;Permet de modifier la valeur d’un objet existant dans la base de faits 
;; par la valeur passé en paramètre
(defun setValeur (objet valeur)
     (if (assoc objet *BF*)
         (setf (cadr (assoc objet *BF*)) valeur)
     )
)

;;Permet d’ajouter un couple (objet valeur) à la base de faits. 
;;Si le couple existe déjà on modifie uniquement sa valeur
(defun ajoutDansBF (couple)
     (if (listp couple)
         (if (eq (length couple) 2)
	     (if (AND (assoc (car couple) *BF*) (NOT (equal 'raceDeChien (car couple))))
	         (not (null (setValeur (car couple) (cadr couple))))
	         (not (null (push couple *BF*)))
	     )
             (format T "Erreur, le couple n’est pas de la bonne forme.")
         )
         (format T "Erreur, le couple n’est pas une liste.")
     )
)

;;Permet de questionner l’utilisateur et d’ajouter les couples correspondant 
;;à ses réponses à la base de faits
(defun createBF ()
     (format T "A quel frequence pratiquez-vous du sport par semaine ? (en chiffre)  ~%")
     (let ((choice (parse-integer (read-line))))
          (ajoutDansBF (list 'frequenceSport choice))
     )
     (format T "Combien d’enfants avez-vous ? (en chiffre) ~%")
     (let ((choice (parse-integer (read-line))))
          (ajoutDansBF (list 'nbEnfants choice))
     )
     (format T "Possédez-vous un jardin ? (oui ou non) ~%")
     (let ((choice (read-line)))
          (ajoutDansBF (list 'possedeJardin? choice))
     )
     (format T "Travaillez-vous beaucoup (oui, moyen ou non) ~%")
     (let ((choice (read-line)))
          (ajoutDansBF (list 'tempsDeTravail choice))
     )
     (format T "Avez-vous une caractéristique particulière ? (aveugle, age ou aucune) ~%")
     (let ((choice (read-line)))
          (ajoutDansBF (list 'caracteristiqueIndividu choice))
     )
     (format T "Avez-vous un caractère particulier ? (autoritaire ou aucun) ~%")
     (let ((choice (read-line)))
          (ajoutDansBF (list 'caractereIndividu choice))
     )
)

;;Permet de vérifier si une prémisse est vrai par rapport à la base de faits
(defun checkPremisse (p)
     (let ((attribut (car p)) (objet (cadr p)) (valeur (caddr p)) (valeurActuel nil))
          (setq valeurActuel (getValeur objet))
          (if (not valeurActuel)
              (return-from checkPremisse NIL)
          )
          (if (equal attribut 'member)
              (member (car objet) (getValeur valeur))
              (eval (list attribut valeurActuel valeur))
          )
     )
)

;;Permet de vérifier si une règle est déclenchable, 
;;c’est-à-dire si elle peut être utilisée par le moteur d’inférence
(defun declenchable? (regle)
     (let ((premisses (getPremisses regle)) (OK T))
          (dolist (p premisses OK)
                  (setq OK (and OK (checkPremisse p)))
          )
     )
)

;;Permet de récupérer la liste des règles candidates, qui peuvent être déclencher
(defun reglesCandidates ()
     (let ((candidates nil))
          (dolist (regle *BR* (reverse candidates))
                  (if (declenchable? (car regle))
                      (push (car regle) candidates)
                  )
          )
     )
)

;;Permet de déclencher une règle
(defun declencher (regle)
     (cond 
         ((declenchable? regle)
          (ajoutDansBF (list (cadr(car (getConclusion regle))) (caddr(car (getConclusion regle)))))
          (supprimeRegle regle)
         )
     )
     (declenchable? regle)
)

;;Moteur d’inférence qui permet de renvoyer la liste des chiens qui conviennent
(defun moteur ()
     (let* ((a (createBF)) (candidates (reglesCandidates)) (regle (car candidates)) (liste NIL) (conclusion (getConclusion regle)))
       (loop 
            (if candidates
                (progn
                      (setq conclusion (getConclusion regle))
                      (declencher regle)
                      (if (equal 'raceDeChien (cadar conclusion))
                          (push (cadar *BF*) liste)
                      )
                      (setq candidates (reglesCandidates))
                      (setq regle (car candidates))
                )
                (if liste
                    (return-from nil liste)
                    (return-from nil "Aucune race de chien ne vous correspond.")
                )
             )
          )
     )
)

(moteur)