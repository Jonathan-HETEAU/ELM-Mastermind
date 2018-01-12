module Mastermind exposing (Model, Msg, update, view, subscriptions, init)


import Html exposing (..)
import Html.Attributes as HtmlA exposing (..) 
import Html.Events as HtmlE exposing (onInput , onClick)
import List
import String
import Random 
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (onClick)



main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Type alias
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

-- Initialisation

init_parametre : Parametre 
init_parametre = 
    { nbr_ligne = 10
    , nbr_colonne = 4
    , nbr_couleur = 6
    , gabarit = Ligne ( List.repeat 4 (Nothing)) Nothing      
    }

init_model : Model 
init_model = 
    { parametre = init_parametre
    , jeu = Just (generation_jeu init_parametre)
    }

-- Constante 

couleurs : List Couleur 
couleurs = ["white","black","cyan","green","navy","silver","gray","olive","teal","blue","lime","purple","magenta","maroon","red","yellow"]

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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
    Generer ->
        ({model | jeu = Just (generation_jeu model.parametre)} , Random.generate Secret (couleurGenerator model.parametre))
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
                    if (est_valide jeu.ligne_courante.colonnes ) then
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
        
    

view : Model -> Html Msg
view model =
    let
        listCouleurs = 
            case model.jeu of
                Just jeu ->
                    List.take jeu.parametre.nbr_couleur couleurs 
                Nothing ->
                    []
                          
    in        
        div []
            [ formulaire model.parametre
            , draw_couleurs listCouleurs
            , plateau model.jeu
            ]


formulaire : Parametre -> Html Msg 
formulaire parametre =
    let
        nbrColonne = toString parametre.nbr_colonne
        nbrLigne = toString parametre.nbr_ligne
        nbrCouleur = toString parametre.nbr_couleur        
    in
        div []
            [ input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max "10", placeholder "Nombre de colonne", value nbrColonne , onInput NbrColonne ] []
            , input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max "100", placeholder "Nombre de ligne", value nbrLigne , onInput NbrLigne ] []
            , input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max (toString (List.length couleurs)) , value nbrCouleur , placeholder "Nombre de couleur", onInput NbrCouleur ] []
            , button [ HtmlE.onClick Generer ] [ Html.text "Générer" ]
            ]

generation_jeu : Parametre -> Jeu 
generation_jeu parametre =
    let
       secret = List.repeat parametre.nbr_colonne ( Just (intToCouleur 1))
       essai = []
       gabarit = Ligne ( List.repeat parametre.nbr_colonne (Nothing)) Nothing
       param = {parametre | gabarit = gabarit}  
    in
        Jeu param secret essai gabarit Nothing EnCour
    

plateau : Maybe Jeu -> Html Msg 
plateau jeu =
    case jeu of 
        Nothing -> svg[][]
        Just lejeu ->
            let
                width = (lejeu.parametre.nbr_colonne +2 ) * 50
                        |> toString 
                height= (lejeu.parametre.nbr_ligne +1 ) * 50
                        |> toString 
                svgJeu = (svg
                            [SvgA.width width ,SvgA.height height, viewBox ("0 0 "++ width ++" "++height)]
                            [draw_jeu lejeu 50 (Position 0 0) ])
                resultat = case lejeu.status of
                            Victoire ->
                                div[]
                                [ h1 [][ Html.text "Victoire"]
                                , svgJeu    
                                ]
                            Defaite ->
                                div[]
                                [ h1 [][ Html.text "Defaite"]    
                                , svgJeu
                                ]
                            EnCour ->
                                div[][svgJeu]
            in
                resultat
                
        
    

draw_jeu : Jeu -> Int -> Position -> Svg Msg 
draw_jeu jeu taille position =
    let
        nbrRest = jeu.parametre.nbr_ligne - (List.length jeu.lignes_essai) - 2
        secret = if jeu.status == EnCour then
                    g[] (draw_secret jeu.parametre.nbr_colonne taille position)
                else  
                    g[] (draw_colonne jeu.combinaison_secrete False 0 taille position)
        positionReste =  Position position.x (position.y + taille)
        reste =  g[] (draw_plateau_repeat jeu.parametre.gabarit False nbrRest taille positionReste )
        positionCourant = Position positionReste.x (positionReste.y + ((nbrRest + 1) * taille))
        courant = if jeu.status == EnCour then 
                     g[] (draw_plateau [jeu.ligne_courante]  True taille positionCourant)
                  else 
                    g[][]
        positionEssai =  Position positionCourant.x (positionCourant.y +  taille)
        essai = if (List.length jeu.lignes_essai) > 0 then 
                    g[] (draw_plateau jeu.lignes_essai False taille positionEssai)
                else 
                    g[][]
        
    in
        g[]
        [secret
        , reste
        , courant
        , essai
        ]


draw_plateau_repeat : Ligne ->Bool -> Int -> Int -> Position -> List (Svg Msg) 
draw_plateau_repeat ligne isCourant nbr taille position = 
    if nbr < 0 then 
        []
    else 
            let
               svgLigne =  draw_ligne ligne isCourant taille position 
            in
                svgLigne :: draw_plateau_repeat ligne isCourant (nbr - 1) taille {position | y = position.y + taille}


draw_plateau : List Ligne ->Bool -> Int -> Position -> List (Svg Msg) 
draw_plateau lignes isCourant taille position = 
    case lignes of 
        []-> []
        (ligne::reste)->
            let
               svgLigne =  draw_ligne ligne isCourant taille position 
            in
                svgLigne :: draw_plateau reste isCourant taille {position | y = position.y + taille}



        

draw_secret : Int -> Int -> Position -> List (Svg Msg)
draw_secret longueur taille position =
    if longueur <= 0 then 
        []
    else
        let
            secret = 
                rect
                [x (toString position.x), y (toString position.y), SvgA.width (toString taille), SvgA.height (toString taille), fill "brown" , stroke "black" ]
                []      
        in
            secret :: draw_secret (longueur - 1) taille {position | x = position.x + taille}


draw_ligne : Ligne -> Bool -> Int -> Position -> Svg Msg
draw_ligne ligne isCourant taille position =
    let
        lenght = List.length ligne.colonnes
        pos = {position | x = position.x + lenght * taille }
        valide = isCourant && (est_valide ligne.colonnes)
    in
        g []
        ((draw_colonne ligne.colonnes isCourant  0 taille position )
        |> (::) (
                if valide then
                    (draw_valider taille pos)
                else 
                    (draw_resultat ligne.resultat  taille pos)))  



draw_colonne : List Colonne -> Bool  -> Int -> Int -> Position -> List (Svg Msg)
draw_colonne colonnes isCourant  acc taille position =
        let
           attributes = (if isCourant then
                            [
                                SvgE.onClick (Select acc) 
                            ]
                            else
                            [])    
        in    
            case colonnes of
                (colonne :: []) ->
                    [draw_case colonne attributes taille position]            
                (colonne :: reste) ->
                    draw_colonne reste isCourant (acc + 1) taille {position | x = position.x + taille}
                    |> (::) (draw_case colonne attributes taille position)
                ([]) ->
                    []

draw_valider : Int -> Position -> Svg Msg 
draw_valider taille  position =
    g[SvgE.onClick ValiderCourant]
    [
     rect
            [x (toString position.x), y (toString position.y), SvgA.width (toString (taille*2)), SvgA.height (toString taille), fill "green" , stroke "white" ]
            []       
    ]


draw_resultat : Maybe Resultat -> Int -> Position -> Svg Msg 
draw_resultat result taille position = 
    let
        rayon = taille //2
        position1 = Position (position.x+ rayon) (position.y+ rayon)
        position2 = {position1 | x = position1.x + taille}
        balisesResult = 
            case result of 
                Just {bien , mal}  ->
                    [circle
                        [ cx (toString position1.x), cy (toString position1.y), r (toString rayon) , fill "green" ]
                        []
                    ,circle
                        [ cx (toString position2.x), cy (toString position2.y), r (toString rayon) , fill "red" ]
                        []
                    ,Svg.text_
                        [ x (toString position1.x), y (toString position1.y), fill "white", textAnchor "middle" , dy ".3em"]
                        [ Svg.text (toString bien)]
                    ,Svg.text_
                         [x (toString (position2.x)), y (toString position2.y), fill "white", textAnchor "middle" , dy ".3em" ]
                        [ Svg.text (toString mal)]
                    ]    
                Nothing -> 
                    []
    in
        g []
            balisesResult     


draw_case : Colonne -> List (Svg.Attribute Msg) -> Int -> Position -> Svg Msg
draw_case colonne attributes taille position = 
    let 
        pos =  Position (position.x+taille//2) (position.y+taille//2)
        contenu = case colonne of
                    Just couleur -> 
                        let
                            rayon = toFloat taille
                             |> (*) 0.4
                             |> round
                        in
                            draw_pion couleur rayon pos       
                    Nothing ->
                        let
                            rayon = toFloat taille
                             |> (*) 0.2
                             |> round
                        in
                            draw_pion "black" rayon pos
         
    in
        g
        attributes
        [ rect
            [x (toString position.x), y (toString position.y), SvgA.width (toString taille), SvgA.height (toString taille), fill "white" , stroke "black" ]
            []
        , contenu
        ]
       
        
draw_couleurs : List Couleur -> Html Msg         
draw_couleurs listCouleurs = 
    let 
        scgNothing =  svg[SvgE.onClick (SelectCouleur Nothing) ,SvgA.width "50" ,SvgA.height "50"][(draw_pion "black" 10 (Position 25 25))]   
        scgCouleur =  List.map (\n->svg[SvgE.onClick (SelectCouleur (Just n)) ,SvgA.width "50" ,SvgA.height "50"][(draw_pion n 20 (Position 25 25))] ) listCouleurs   
                     
    in
        div []
            (List.append [scgNothing] scgCouleur )

draw_pion : Couleur -> Int -> Position -> Svg Msg
draw_pion couleur rayon pos = 
    circle
        [ cx (toString pos.x), cy (toString pos.y), r (toString rayon) , fill couleur , stroke "black" , strokeWidth "4"]
        []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    let
      model = init_model  
    in   
        (model , Random.generate Secret (couleurGenerator model.parametre))


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



est_valide : List Colonne -> Bool 
est_valide colonnes = 
    case colonnes of
        [] ->
            True
        (elem :: reste) ->
            if elem == Nothing then 
                False
            else
                (est_valide reste)

est_mal_placer : List a -> List a -> Int
est_mal_placer secret essai =
    let
        secretRes = drop_mal_placers (==) secret essai 
    in
        ((List.length secret) - (List.length secretRes))         

drop_mal_placers :  (a->a->Bool) -> List a -> List a -> List a 
drop_mal_placers equal vals liste =
    case vals of
        [] ->
            liste
        (elem :: reste) ->
            let
                listDrop = drop_mal_placers (equal) reste liste           
            in
                drop_mal_placer (equal) elem listDrop



drop_mal_placer : (a->a->Bool) -> a -> List a -> List a 
drop_mal_placer equal val liste = 
    case liste of
        [] ->
            []
        (elem :: reste) ->
            if (equal val elem ) then 
                reste
            else
                (elem::drop_mal_placer (equal) val reste)
            


valider : List Colonne -> List Colonne -> Resultat 
valider secret essai =
    let 
      (secretA ,essaiA , bien ) = (est_bien_placer secret essai)
    in                    
        Resultat bien (est_mal_placer secretA essaiA)     


est_bien_placer : List a -> List a  ->(List a,List a,Int)
est_bien_placer secret essai =
    case secret of
        [] ->
            (secret,essai,0)                     
        (a:: secretR) ->
            case essai of
                [] ->
                    (secret,essai,0)    
                (b:: essaiR) ->
                    let
                            (secretResult , essaiResult , bien) = (est_bien_placer  secretR   essaiR)       
                    in
                        if a == b then
                            ( secretResult , essaiResult , bien + 1)                         
                        else 
                            ( a::secretResult , b::essaiResult , bien )
