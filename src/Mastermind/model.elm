module Mastermind.Model exposing (..)

import List
import Random

type alias Parametre = 
    { nbr_ligne : Int
    , nbr_colonne : Int
    , nbr_couleur : Int 
    , gabarit : Ligne
    } 

type alias Colonne = Maybe Couleur

type alias Couleur = String

type alias Resultat = 
    { bien : Int 
    , mal : Int         
    }

type alias Ligne = 
    { colonnes : List Colonne 
    , resultat : Maybe Resultat        
    }

type StatusJeu 
    = EnCour 
    | Victoire 
    | Defaite  


type alias Jeu = 
    { parametre : Parametre
    , combinaison_secrete : List Colonne 
    , lignes_essai : List Ligne 
    , ligne_courante : Ligne
    , select_couleur : Maybe Couleur 
    , status : StatusJeu
    }

type alias Model =
    { parametre : Parametre
    , jeu : Maybe Jeu          
    }
    
type alias Position = 
    { x : Int
    , y : Int
    }

type Msg 
    = NbrColonne String 
    | NbrLigne String 
    | NbrCouleur String
    | Generer
    | Secret (List Couleur)
    | Select Int
    | SelectCouleur (Maybe Couleur)  
    | ValiderCourant 


couleurs : List Couleur 
couleurs = ["white","black","cyan","green","navy","silver","gray","olive","teal","blue","lime","purple","magenta","maroon","red","yellow"]



init : (Model, Cmd Msg)
init = 
    let
      model = initModel initParametre  
    in   
        (model , Random.generate Secret (couleurGenerator model.parametre))

initModel :Parametre -> Model 
initModel parametre = 
    { parametre = parametre
    , jeu = Just (generationJeu parametre)
    }

initParametre : Parametre 
initParametre = 
    { nbr_ligne = 10
    , nbr_colonne = 4
    , nbr_couleur = 6
    , gabarit = Ligne ( List.repeat 4 (Nothing)) Nothing      
    }




generationJeu : Parametre -> Jeu 
generationJeu parametre =
    let
       secret = List.repeat parametre.nbr_colonne ( Just (intToCouleur 1))
       essai = []
       gabarit = Ligne ( List.repeat parametre.nbr_colonne (Nothing)) Nothing
       param = {parametre | gabarit = gabarit}  
    in
        Jeu param secret essai gabarit Nothing EnCour


estValide : List Colonne -> Bool 
estValide colonnes = 
    case colonnes of
        [] ->
            True
        (elem :: reste) ->
            if elem == Nothing then 
                False
            else
                (estValide reste)

            





intToCouleur : Int -> Couleur  
intToCouleur i= 
    let 
        expression = List.drop i couleurs 
                    |>List.head
    in
        case expression of
            Just couleur ->
                couleur   
            _ ->
                "white"

-- 

couleurGenerator : Parametre -> Random.Generator (List Couleur)
couleurGenerator parametre = 
    Random.list parametre.nbr_colonne  ( Random.map intToCouleur (Random.int 0 (parametre.nbr_couleur - 1)))



