{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Cell = Hunter | Targett | Obstacle | Gateway | Empty deriving (Eq, Ord)


data Game = Game {
    rows :: Int,
    cols :: Int,
    cellVect :: [Cell],
    hunters :: Position,
    gateways :: [(Position, Position)],
    targets :: [Target]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

instance Show Cell 
    where 
        show Hunter = "!"
        show Targett = "*"
        show Obstacle = "@"
        show Gateway = "#"
        show Empty = " "

gameAsString :: Game -> String
gameAsString (Game r c cells hs gt targ) =  take (length cells + r - 1) (gameAsStringHelper (Game r c cells hs gt targ))

gameAsStringHelper :: Game -> String
gameAsStringHelper (Game r c cells hs gt targ)= if r == 0 then [] else col ++ "\n" ++ gameAsString (Game (r-1) c remains hs gt targ)
    where 
        col = concatMap show $ take c cells
        remains = drop c cells

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame n m  = addHunter (1,1) (Game n m (replicate m Obstacle ++ makeBody (n - 2) m ++ replicate m Obstacle) (1,1) [] [])

{- functie care returneaza tabla fara mariginile de sus si jos -}
makeBody :: Int -> Int -> [Cell]
makeBody n m = if n == 0 then [] else [Obstacle] ++ replicate (m - 2) Empty ++ [Obstacle] ++ makeBody (n - 1) m
   
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) (Game r c cells (x_h, y_h) gt targ ) 
    | x >= (r - 1) || y >= (c - 1) = Game r c cells (x_h, y_h) gt targ
    | y <= 0 || x <= 0 = Game r c cells (x_h, y_h) gt targ
    | poss == [Obstacle] = Game r c cells (x_h, y_h) gt targ
    | otherwise = Game r c (take offset body ++ [Hunter] ++ drop (offset + 1) body) (x, y) gt targ
        where 
           offset = c * x + y
           poss = drop offset $ take (offset + 1) cells
           body = deleteHunter (Game r c cells (x_h, y_h) gt targ)

{- functie care sterge hunterul inainte sa fie adaugat pe noua pozitie -}
deleteHunter :: Game -> [Cell]
deleteHunter game --(Game r c cells (x, y) gt targ ) 
    | poss /= [Hunter] = cellVect game
    | otherwise = take offset (cellVect game) ++ [Empty] ++ drop (offset + 1) (cellVect game)
        where 
           poss = drop offset $ take (offset + 1) (cellVect game)
           offset = cols game * fst (hunters game) + snd (hunters game)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behaviour (x, y) (Game r c cells (x_h, y_h) gt targ)
    | x == 0 || x >= (r - 1) = Game r c cells (x_h, y_h) gt targ
    | y == 0 || y >= (c - 1) = Game r c cells (x_h, y_h) gt targ
    | pos == [Obstacle] = Game r c cells (x_h, y_h) gt targ
    |otherwise = Game r c (take offset cells ++ [Targett] ++ drop (offset + 1) cells) (x_h, y_h) gt (Target {position = (x, y), behavior = behaviour} : targ)
        where 
           offset = c * x + y
           pos = drop offset $ take (offset + 1) cells

    
{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1@(x1, y1), pos2@(x2, y2)) (Game r c cells hs gt targ)  = Game r c body hs ((pos1, pos2) : gt) targ
    where
        body1 = addGate (x1, y1) cells c 
        body = addGate (x2, y2) body1 c 

{- functie care adauga o poarta in joc -}
addGate :: Position -> [Cell] -> Int -> [Cell]
addGate (x, y) cells c = take offset cells ++ [Gateway] ++ drop (offset + 1) cells
        where 
           offset = c * x + y

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

addObstacle :: Position -> Game -> Game
addObstacle (x, y) (Game r c cells hs gt targ) =  Game r c body hs gt targ
    where
        body = addObs (x, y) cells c

{- functie care adauga obstacolul pe tabla -}
addObs :: Position -> [Cell] -> Int -> [Cell]
addObs (x, y) cells c = take offset cells ++ [Obstacle] ++ drop (offset + 1) cells
        where 
           offset = c * x + y

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) game 
    |pos == [Empty] = Just (x, y)
    |pos == [Gateway] = Just $ findPair (x, y) (gateways game) 
    |pos == [Obstacle]  = Nothing 
    |otherwise = Nothing 
    where
        pos = drop offset $ take (offset + 1) (cellVect game)
        offset = cols game * x + y 

{- functie care intoarce pozitia perechii gateway-ului -}
findPair :: Position -> [(Position, Position)] -> Position
findPair pos@(x, y) gts
    | x == fst (fst (head gts)) && y == snd (fst (head gts))  = snd (head gts)
    | x == fst (snd (head gts)) && y == snd (snd (head gts)) = fst (head gts)
    | null gts = (-1, -1)
    |otherwise = findPair pos (tail gts)
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

{-
    functie generala care primeste pozitia intiala a targetului, displacement-ul si intoarce un target nou miscat in directia 
    corespunzatoare displacement-ului 
-}
iLikeToMoveItMoveIt :: Position -> Game -> (Int, Int) -> Behavior -> Target
iLikeToMoveItMoveIt (x,y) (Game r c cells hs gt targ) (a, b) behaviour 
    | x <= 0 || x >= (r - 1) = Target {position = (x, y), behavior = behaviour}
    | y <= 0 || y >= (c - 1) = Target {position = (x, y), behavior = behaviour}
    | isNothing (attemptMove (x + a, y + b) (Game r c cells hs gt targ)) = Target {position = (x, y), behavior = behaviour}
    | otherwise = Target {position = (x_uf, y_uf), behavior = behaviour}
        where
            (x_uf, y_uf) = fromJust $ attemptMove (x + a, y + b) (Game r c cells hs gt targ)

goEast :: Behavior
goEast (x, y) (Game r c cells hs gt targ) = iLikeToMoveItMoveIt (x, y) (Game r c cells hs gt targ) (0, 1) goEast
    
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) (Game r c cells hs gt targ)  = iLikeToMoveItMoveIt (x, y) (Game r c cells hs gt targ) (0, -1)  goWest

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) (Game r c cells hs gt targ)  = iLikeToMoveItMoveIt (x, y) (Game r c cells hs gt targ) (-1, 0) goNorth

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior 
goSouth (x, y) (Game r c cells hs gt targ) = iLikeToMoveItMoveIt (x, y) (Game r c cells hs gt targ) (1, 0) goSouth

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior 
bounce dir (x, y) (Game r c cells hs gt targ) 
    | isJust (attemptMove (x + dir, y) (Game r c cells hs gt targ))  = Target {position = pos, behavior = bounce dir}
    | otherwise = if isJust (attemptMove (x - dir, y) (Game r c cells hs gt targ)) then Target {position = pos_op, behavior = bounce (- dir)}
                else Target {position = (-2, -2), behavior = bounce 0} 
            --pe else nu se va intra dar aveam warning si era enervant
        where
            pos = fromJust $ attemptMove (x + dir, y) (Game r c cells hs gt targ)
            pos_op = fromJust $ attemptMove (x - dir, y) (Game r c cells hs gt targ)


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

{- sterge toate targeturile de pe tabla, dar nu din lista game-ului -}
deleteTargets :: Game -> [Cell] 
deleteTargets (Game r c cells hs gt targ )
    | null cells = []
    | head cells == Targett = Empty : deleteTargets (Game r c (tail cells) hs gt targ)
    | otherwise = head cells : deleteTargets (Game r c (tail cells) hs gt targ )

{- functie care aplica behaviour pe target -}
updateTargets :: Game -> Game
updateTargets game = game {targets = map (\(Target pos bhv) -> bhv pos game) (targets game)}

{-functie care adauga toate targeturile din lista game-ului pe tabla -}
addAllTargets :: [Target] -> Game -> Game
addAllTargets newTargets (Game r c cells hs gt targ ) 
    | null newTargets = Game r c cells hs gt targ 
    | otherwise  = addAllTargets (tail newTargets) (addTarget (behavior (head newTargets)) (position (head newTargets)) (Game r c cells hs gt targ ))
    

moveTargets :: Game -> Game
moveTargets game = addAllTargets (targets game1) game1 {targets = []}
    where 
        newCells = deleteTargets game 
        game1 = updateTargets game {cellVect = newCells}

            
{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x_h, y_h) target
    | x_h + 1 == fst (position target) && y_h == snd (position target) = True 
    | x_h - 1 == fst (position target) && y_h == snd (position target) = True
    | x_h == fst (position target) && y_h - 1 == snd (position target) = True
    | x_h == fst (position target) && y_h + 1 == snd (position target) = True
    | otherwise = False 


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
{- misca hunterul pe tabla -}
goHunter :: Direction -> Game -> Game
goHunter dir game 
    | dir == North = addHunter (x - 1, y) game
    | dir == South = addHunter (x + 1, y) game
    | dir == West = addHunter (x, y - 1) game
    | otherwise  = addHunter (x, y + 1) game
    where
        (x,y) = hunters game

{- sterget un target -}
deleteTarget :: Position -> Int -> [Cell] -> [Cell]
deleteTarget (x, y) c cells  
    | poss /= [Targett] = cells
    | otherwise = take offset cells ++ [Empty] ++ drop (offset + 1) cells
        where 
           poss = drop offset $ take (offset + 1) cells  
           offset = c * x + y

{- elimina targeturile din preajma hunterului -}
eliminateTargets :: [Target] -> Position -> Game -> Game
eliminateTargets newTargets (x,y) (Game r c cells hs gt targ ) 
    | null targ = Game r c cells hs gt newTargets
    | isTargetKilled (x,y) (head targ) = eliminateTargets newTargets (x,y) (Game r c (deleteTarget (position (head targ))c cells) hs gt (tail targ) ) 
    | otherwise = eliminateTargets (head targ : newTargets) (x,y) (Game r c cells hs gt (tail targ))


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game  
    | bool  = game4
    | otherwise = game1
     where
         game1 = goHunter dir game
         game2 = eliminateTargets [] (hunters game1) game1
         game3 = moveTargets game2
         game4 = eliminateTargets [] (hunters game1) game3


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = null (targets game)
{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

{- verifica daca exista targeturi de eliminat. La primul target care poate fi eliminat se opreste -}
isPossibleToEliminateTargets :: Position -> Game -> Bool
isPossibleToEliminateTargets (x,y) (Game r c cells hs gt targ ) 
    | null targ = False
    | isTargetKilled (x,y) (head targ) = True 
    | otherwise =  isPossibleToEliminateTargets (x,y) (Game r c cells hs gt (tail targ))

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, advanceGameState North False  game ), (South, advanceGameState South False  game ), (East, advanceGameState East False  game ), (West, advanceGameState West False game )]
       
    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    isGoal game = isPossibleToEliminateTargets (hunters game) game 

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = undefined

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int
------------------------------------------------------------------------------------------------------------
{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
