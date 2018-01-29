module Mastermind.Update exposing (update)

import Mastermind.Model as Model exposing (..)
import Random 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
    Generer ->
        ({model | jeu = Just (generationJeu model.parametre)} , Random.generate Secret (couleurGenerator model.parametre))
    Secret secretCode ->
        case model.jeu of
            Just jeu ->
                let 
                  combinaison = List.map (Just) secretCode
                in
                    ({model | jeu = Just {jeu | combinaison_secrete = combinaison }},Cmd.none)
            Nothing ->
                (model,Cmd.none)
    NbrColonne val ->
        let
            {parametre, jeu } = model  
            value = 
                case  (String.toInt val) of 
                Ok nbr ->     
                    if (nbr > 0) &&  (nbr < 100)  then
                        nbr 
                    else  
                    parametre.nbr_colonne
                _ -> 
                    parametre.nbr_colonne
        in
            ({model | parametre = {parametre | nbr_colonne =  value }} , Cmd.none)
    NbrLigne val ->
        let
            {parametre, jeu } = model  
            value = 
                case  (String.toInt val) of 
                Ok nbr ->     
                    if (nbr > 0) &&  (nbr < 100)  then
                        nbr 
                    else  
                    parametre.nbr_ligne
                _ -> 
                    parametre.nbr_ligne
        in
            ({model | parametre = {parametre | nbr_ligne =  value }} , Cmd.none)
    NbrCouleur val ->
        let
            {parametre, jeu } = model  
            value = 
                case  (String.toInt val) of 
                Ok nbr ->     
                    if (nbr > 1) &&  (nbr <= (List.length couleurs ))  then
                        nbr 
                    else  
                    parametre.nbr_couleur
                _ -> 
                    parametre.nbr_couleur
        in
            ({model | parametre = {parametre | nbr_couleur =  value }} , Cmd.none)
    Select num ->
        let
            {parametre, jeu } = model    
        in
            case jeu of
                Just jeu  ->
                let
                  {parametre,combinaison_secrete,lignes_essai,ligne_courante,select_couleur} = jeu
                in
                    ({model | jeu = Just {jeu | ligne_courante = {ligne_courante | colonnes = (replace num 0 select_couleur ligne_courante.colonnes   )  }}}, Cmd.none)   
                Nothing ->
                    (model,Cmd.none)  
    SelectCouleur couleur ->
        let
            {parametre, jeu } = model 
        in
            case jeu of
                Just jeu ->
                    ({model | jeu = Just {jeu | select_couleur = couleur }}, Cmd.none)   
                Nothing ->
                    (model,Cmd.none)   
    ValiderCourant ->
        let
            {parametre, jeu } = model  
        in
            case jeu of
                Just jeu ->
                    if (estValide jeu.ligne_courante.colonnes ) then
                        let
                            courant = jeu.ligne_courante
                            {bien,mal} = valider jeu.combinaison_secrete courant.colonnes
                            essais = ({courant |  resultat = Just (Resultat bien mal ) } :: jeu.lignes_essai)
                            status =
                                if bien == jeu.parametre.nbr_colonne then 
                                    Victoire
                                else
                                    if List.length essais >= jeu.parametre.nbr_ligne then 
                                        Defaite
                                    else
                                        EnCour
                        in
                            ({model | jeu = Just { jeu | ligne_courante = jeu.parametre.gabarit , lignes_essai = essais , status = status }},Cmd.none)   
                    else 
                        (model,Cmd.none)    
                Nothing ->
                    (model,Cmd.none)                
        

replace : Int-> Int -> a -> List a -> List a
replace index acc val lists = 
    case lists of
        [] ->
            []
        (elem::reste) ->
            if index == acc then
                val :: reste
            else
                elem :: (replace index (acc + 1) val reste )

valider : List a -> List a -> Resultat 
valider secret essai =
    let 
      (secretA ,essaiA , bien ) = (estBienPlace secret essai)
    in                    
        Resultat bien (estMalPlace secretA essaiA)     

estBienPlace : List a -> List a  ->(List a,List a,Int)
estBienPlace secret essai =
    case secret of
        [] ->
            (secret,essai,0)                     
        (a:: secretR) ->
            case essai of
                [] ->
                    (secret,essai,0)    
                (b:: essaiR) ->
                    let
                            (secretResult , essaiResult , bien) = (estBienPlace  secretR   essaiR)       
                    in
                        if a == b then
                            ( secretResult , essaiResult , bien + 1)                         
                        else 
                            ( a::secretResult , b::essaiResult , bien )

estMalPlace : List a -> List a -> Int
estMalPlace secret essai =
    let
        secretRes = retireLesMalPlaces (==) secret essai 
    in
        ((List.length secret) - (List.length secretRes))         

retireLesMalPlaces :  (a->a->Bool) -> List a -> List a -> List a 
retireLesMalPlaces equal vals liste =
    case vals of
        [] ->
            liste
        (elem :: reste) ->
            let
                listDrop = retireLesMalPlaces (equal) reste liste           
            in
                retireUnMalPlace (equal) elem listDrop



retireUnMalPlace : (a->a->Bool) -> a -> List a -> List a 
retireUnMalPlace equal val liste = 
    case liste of
        [] ->
            []
        (elem :: reste) ->
            if (equal val elem ) then 
                reste
            else
                (elem::retireUnMalPlace (equal) val reste)