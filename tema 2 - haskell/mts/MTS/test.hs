data Cell = Hunter | Targett | Obstacle | Gateway | Empty deriving (Eq, Ord)
type Position = (Int, Int)
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

type Behavior = Position -> Game -> Target


data Game = Game {
    rows :: Int,
    cols :: Int,
    cells :: [Cell],
    hunters :: Position,
    gateways :: [(Position, Position)],
    targets :: [Target],
    obstacles :: [Position]
    
} deriving (Eq, Ord)

instance Show Cell 
    where 
        show Hunter = "!"
        show Targett = "*"
        show Obstacle = "@"
        show Gateway = "#"
        show Empty = " "

-- gameAsString :: Game -> String
-- gameAsString (Game r c cells) = if r == 0 then [] else col ++ "\n" ++ gameAsString (Game (r-1) c remains)
--     where 
--         col = concatMap show $ take c cells
--         remains = drop c cells
    

-- instance Show Game where
--     show = gameAsString


gameAsString :: Game -> String
gameAsString (Game r c cells hs gt targ obs) = take (length cells + r - 1) (gameAsStringHelper (Game r c cells hs gt targ obs))

gameAsStringHelper :: Game -> String
gameAsStringHelper (Game r c cells hs gt targ obs) = if r == 0 then [] else col ++ "\n" ++ gameAsString (Game (r-1) c remains hs gt targ obs)
    where 
        col = concatMap show $ take c cells
        remains = drop c cells

instance Show Game where
    show = gameAsString

emptyGame :: Int -> Int -> Game
emptyGame r c   =  addHunter (1,1) (Game r c (replicate c Obstacle ++ makeBody (r - 2) c ++ replicate c Obstacle) (1,1) [] [] [])
-- addHunter (1,1)
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
addHunter pos@(x, y) (Game r c cells (x_h, y_h) gt targ obs) 
    | x == 0 || x == (c - 1) = Game r c cells (x_h, y_h) gt targ obs
    | y == 0 || y == (r - 1) = Game r c cells (x_h, y_h) gt targ obs
    | y < 0 || x < 0 = Game r c cells (x_h, y_h) gt targ obs
    | otherwise = Game r c (take offset body ++ [Hunter] ++ drop (offset + 1) body) (x, y) gt targ obs
        where 
           offset = c * x + y
           body = deleteHunter (Game r c cells (x_h, y_h) gt targ obs)

deleteHunter :: Game -> [Cell]
deleteHunter (Game r c cells (x, y) gt targ obs) = take offset cells ++ [Empty] ++ drop (offset + 1) cells
        where 
           offset = c * x + y


findPair :: Position -> [(Position, Position)] -> Position
findPair pos@(x, y) gateways
    | x == fst (fst (head gateways)) && y == snd (fst (head gateways))  = snd (head gateways)
    | x == fst (snd (head gateways)) && y == snd (snd (head gateways)) = fst (head gateways)
    | null gateways = (-1, -1)
    |otherwise = findPair pos (tail gateways)

addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1@(x1, y1), pos2@(x2, y2)) (Game r c cells hs gt targ obs)  = Game r c body hs ((pos1, pos2) : gt) targ obs
    where
        body1 = addGate (x1, y1) cells c 
        body = addGate (x2, y2) body1 c 


addGate :: Position -> [Cell] -> Int -> [Cell]
addGate (x, y) cells c = take offset cells ++ [Gateway] ++ drop (offset + 1) cells
        where 
           offset = c * x + y


addTarget :: Behavior -> Position -> Game -> Game
addTarget behaviour (x, y) (Game r c cells (x_h, y_h) gt targ obs) = insertTarget behaviour (x, y) (Game r c cells (x_h, y_h) gt targ obs)
    

insertTarget :: Behavior -> Position -> Game  -> Game
insertTarget behaviour (x, y) (Game r c cells (x_h, y_h) gt targ obs)
    | x == 0 || x >= (r - 1) = Game r c cells (x_h, y_h) gt targ obs
    | y == 0 || y >= (c - 1) = Game r c cells (x_h, y_h) gt targ obs
    | pos == [Obstacle] = Game r c cells (x_h, y_h) gt targ obs
    |otherwise = Game r c (take offset cells ++ [Targett] ++ drop (offset + 1) cells) (x_h, y_h) gt (Target {position = (x, y), behavior = behaviour} : targ) obs
        where 
           offset = c * x + y
           pos = drop offset $ take (offset + 1) cells


goNorth :: Behavior
goNorth (x, y) (Game r c cells hs gt targ obs)  = undefined

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth = undefined