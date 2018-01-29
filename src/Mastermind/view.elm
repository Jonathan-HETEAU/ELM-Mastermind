module Mastermind.View exposing (view)


import Html exposing (..)
import Html.Attributes as HtmlA exposing (..) 
import Html.Events as HtmlE exposing (onInput , onClick)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (onClick)


import Mastermind.Model as Model exposing (..)


taille : Int 
taille = 50

rayon : Int 
rayon = 25

svg : List (Svg.Attribute Msg) -> List ( Svg Msg) -> Svg Msg 
svg attributes children =
    Svg.node "svg" attributes children

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
            [ drawTitre "Mastermind" 
            , formulaire model.parametre
            , drawCouleurs listCouleurs
            , plateau model.jeu
            , drawCouleurs listCouleurs
            ]


drawTitre : String -> Html Msg 
drawTitre titre =
    section [ HtmlA.class "hero is-primary" ]
    [ div [ HtmlA.class "hero-body" ]
        [ div [ HtmlA.class "container" ]
            [ h1 [ HtmlA.class "title" ]
                [ Html.text titre ]
            ]
        ]
    ]

formulaire : Parametre -> Html Msg 
formulaire parametre =
    let
        nbrColonne = toString parametre.nbr_colonne
        nbrLigne = toString parametre.nbr_ligne
        nbrCouleur = toString parametre.nbr_couleur        
    in
        section [HtmlA.class "section"]
        [   div [HtmlA.class "container columns is-centered"]
            [ htmlInput "Nombre de colonne" (input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max "10", placeholder "Nombre de colonne", value nbrColonne , onInput NbrColonne ] [])
            , htmlInput "Nombre de ligne" (input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max "100", placeholder "Nombre de ligne", value nbrLigne , onInput NbrLigne ] [])
            , htmlInput "Nombre de couleur" (input [ HtmlA.type_ "number",HtmlA.min "1", HtmlA.max (toString (List.length couleurs)) , value nbrCouleur , placeholder "Nombre de couleur", onInput NbrCouleur ] [])
            , htmlButton "Générer" (HtmlE.onClick Generer)  
            ]
        ]
htmlInput : String -> Html Msg -> Html Msg 
htmlInput nom contenu = 
    div [ HtmlA.class "column"]
        [
            div [ HtmlA.class "field" ]
                [ label [ HtmlA.class "label" ]
                    [ Html.text nom ]
                , div [ HtmlA.class "control" ]
                    [ 
                        contenu
                    ]
                ]
        ]

htmlButton : String -> Html.Attribute Msg -> Html Msg
htmlButton nom contenu = 
    div [ HtmlA.class "column  field"] 
    [
        div [ HtmlA.class "control" ]
        [ Html.button [ contenu , HtmlA.class "button is-link" ]
            [ Html.text nom ]
        ]
    ]    


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
                            [SvgA.class "image" ,SvgA.width width ,SvgA.height height, viewBox ("0 0 "++ width ++" "++height)]
                            [drawJeu lejeu 50 (Position 0 0) ])
                resultat = case lejeu.status of
                            Victoire ->
                                div[HtmlA.class " is-narrow"]
                                [ p [HtmlA.class  "title is-success"]
                                        [ Html.text "Victoire"]
                                , svgJeu    
                                ]
                            Defaite ->
                                div[HtmlA.class " is-narrow"] 
                                [ p [HtmlA.class  "title is-danger"]
                                    [ Html.text "Defaite"]   
                                , svgJeu
                                ]
                            EnCour ->
                                div[] [svgJeu]
            in
                div[HtmlA.class "columns is-mobile is-multiline is-centered"][resultat]
                
        
    

drawJeu : Jeu -> Int -> Position -> Svg Msg 
drawJeu jeu taille position =
    let
        nbrRest = jeu.parametre.nbr_ligne - (List.length jeu.lignes_essai) - 2
        secret = if jeu.status == EnCour then
                    g[] (drawSecret jeu.parametre.nbr_colonne taille position)
                else  
                    g[] (drawColonne jeu.combinaison_secrete False 0 taille position)
        positionReste =  Position position.x (position.y + taille)
        reste =  g[] [(svgPlateau jeu.parametre positionReste)]
        positionCourant = Position positionReste.x (positionReste.y + ((nbrRest + 1) * taille))
        courant = if jeu.status == EnCour then 
                     g[] (drawPlateau [jeu.ligne_courante]  True taille positionCourant)
                  else 
                    g[][]
        positionEssai =  Position positionCourant.x (positionCourant.y +  taille)
        essai = if (List.length jeu.lignes_essai) > 0 then 
                    g[] (drawPlateau jeu.lignes_essai False taille positionEssai)
                else 
                    g[][]
        
    in
        g[]
        [secret
        , reste
        , courant
        , essai
        ]


drawPlateauRepeat : Ligne ->Bool -> Int -> Int -> Position -> List (Svg Msg) 
drawPlateauRepeat ligne isCourant nbr taille position = 
    if nbr < 0 then 
        []
    else 
            let
               svgLigne =  drawLigne ligne isCourant taille position 
            in
                svgLigne :: drawPlateauRepeat ligne isCourant (nbr - 1) taille {position | y = position.y + taille}


drawPlateau : List Ligne ->Bool -> Int -> Position -> List (Svg Msg) 
drawPlateau lignes isCourant taille position = 
    case lignes of 
        []-> []
        (ligne::reste)->
            let
               svgLigne =  drawLigne ligne isCourant taille position 
            in
                svgLigne :: drawPlateau reste isCourant taille {position | y = position.y + taille}



        

drawSecret : Int -> Int -> Position -> List (Svg Msg)
drawSecret longueur taille position =
    if longueur <= 0 then 
        []
    else
        let
            secret = 
                rect
                [x (toString position.x), y (toString position.y), SvgA.width (toString taille), SvgA.height (toString taille), fill "brown" , stroke "black" ]
                []      
        in
            secret :: drawSecret (longueur - 1) taille {position | x = position.x + taille}


drawLigne : Ligne -> Bool -> Int -> Position -> Svg Msg
drawLigne ligne isCourant taille position =
    let
        lenght = List.length ligne.colonnes
        pos = {position | x = position.x + lenght * taille }
        valide = isCourant && (estValide ligne.colonnes)
    in
        g []
        ((drawColonne ligne.colonnes isCourant  0 taille position )
        |> (::) (
                if valide then
                    (drawValider taille pos)
                else 
                    (drawResultat ligne.resultat  taille pos)))  



drawColonne : List Colonne -> Bool  -> Int -> Int -> Position -> List (Svg Msg)
drawColonne colonnes isCourant  acc taille position =
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
                    [drawCase colonne attributes taille position]            
                (colonne :: reste) ->
                    drawColonne reste isCourant (acc + 1) taille {position | x = position.x + taille}
                    |> (::) (drawCase colonne attributes taille position)
                ([]) ->
                    []

drawValider : Int -> Position -> Svg Msg 
drawValider taille  position =
    g[SvgE.onClick ValiderCourant]
    [
     rect
            [x (toString position.x), y (toString position.y), SvgA.width (toString (taille*2)), SvgA.height (toString taille), fill "green" , stroke "white" ]
            []       
    ]


drawResultat : Maybe Resultat -> Int -> Position -> Svg Msg 
drawResultat result taille position = 
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


drawCase : Colonne -> List (Svg.Attribute Msg) -> Int -> Position -> Svg Msg
drawCase colonne attributes taille position = 
    let 
        pos =  Position (position.x+taille//2) (position.y+taille//2)
        contenu = case colonne of
                    Just couleur -> 
                        let
                            rayon = toFloat taille
                             |> (*) 0.4
                             |> round
                        in
                            drawPion couleur rayon pos       
                    Nothing ->
                        let
                            rayon = toFloat taille
                             |> (*) 0.2
                             |> round
                        in
                            drawPion "black" rayon pos
         
    in
        g
        attributes
        [contenu
        ]
       
        
drawCouleurs : List Couleur -> Html Msg         
drawCouleurs listCouleurs = 
    let 
        scgNothing =  div[HtmlA.class "column is-narrow" ][svg[SvgE.onClick (SelectCouleur Nothing) ,SvgA.width "50" ,SvgA.height "50"][(drawPion "black" 10 (Position 25 25))]]   
        scgCouleur =  List.map (\n->div[HtmlA.class "column is-narrow" ][svg[SvgE.onClick (SelectCouleur (Just n)) ,SvgA.width "50" ,SvgA.height "50"][(drawPion n 20 (Position 25 25))]] ) listCouleurs   
                     
    in
        div [HtmlA.class "columns is-gapless is-multiline is-mobile  is-centered"]
            (List.append [scgNothing] scgCouleur )

drawPion : Couleur -> Int -> Position -> Svg Msg
drawPion couleur rayon pos = 
    circle
        [ cx (toString pos.x), cy (toString pos.y), r (toString rayon) , fill couleur , stroke "black" , strokeWidth "4"]
        []




svgPlateau : Parametre -> Position -> Svg Msg 
svgPlateau parametre position =
        svg [ x (toString position.x) ,  y (toString position.y) ]
         (
             grillePosition (parametre.nbr_colonne - 1 ,parametre.nbr_ligne ) 0 0
            |> List.map (svgCase)
         )
        


grillePosition : (Int,Int) ->Int -> Int -> List Position
grillePosition (oX , oY) x  y =
    if 0 < oX && y < oY then
        grillePosition (oX , oY) x ( y + 1)  
        |> (::)  (Position (x * 50) (y *50))  
    else 
        if x < oX then 
            grillePosition (oX , oY) ( x + 1)  0 
            |> (::)  (Position (x * 50) 0 )
        else 
            []

svgCase : Position -> Svg Msg 
svgCase position =
    svg [ x (toString position.x) ,  y (toString position.y) ]
    [ rect  [y "0" , SvgA.width "50", SvgA.height "50", fill "#501616"][]
    ,circle [ cx "25" , cy "25", r "15" , fill "#803300" , stroke "black" , strokeWidth ".23302"][]
    ,circle [ cx "25" , cy "25", r "10" , fill "#280b0b" , stroke "black" , strokeWidth ".15534"][]
    ]
