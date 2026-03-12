module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import System.Exit (exitSuccess)

-- ============================================================
-- Types
-- ============================================================

type Coord    = (Int, Int)
type Board    = [[Maybe PieceType]]

data PieceType = I | J | L | O | S | T | Z
  deriving (Eq, Show, Enum, Bounded)

data Piece = Piece
  { pieceType   :: PieceType
  , pieceBlocks :: [Coord]
  , pieceOrigin :: Coord
  } deriving (Show)

data GameState = GameState
  { gsBoard      :: Board
  , gsCurrent    :: Piece
  , gsNext       :: PieceType
  , gsScore      :: Int
  , gsLevel      :: Int
  , gsLines      :: Int
  , gsTickTimer  :: Float
  , gsGameOver   :: Bool
  , gsDropTrail  :: Float
  } deriving (Show)

boardW, boardH :: Int
boardW = 10
boardH = 20

cellSize :: Float
cellSize = 30

-- ============================================================
-- Pastel color palette (cute!)
-- ============================================================

pastelColor :: PieceType -> Color
pastelColor I = makeColorI 130 220 255 255   -- baby blue
pastelColor J = makeColorI 140 150 255 255   -- lavender
pastelColor L = makeColorI 255 190 130 255   -- peach
pastelColor O = makeColorI 255 240 140 255   -- butter yellow
pastelColor S = makeColorI 170 255 170 255   -- mint green
pastelColor T = makeColorI 240 170 240 255   -- pink lilac
pastelColor Z = makeColorI 255 150 160 255   -- rose

darkerColor :: PieceType -> Color
darkerColor I = makeColorI 90  180 220 255
darkerColor J = makeColorI 100 110 220 255
darkerColor L = makeColorI 220 150 90  255
darkerColor O = makeColorI 220 200 100 255
darkerColor S = makeColorI 130 215 130 255
darkerColor T = makeColorI 200 130 200 255
darkerColor Z = makeColorI 220 110 120 255

ghostColor :: PieceType -> Color
ghostColor pt = withAlpha 0.25 (pastelColor pt)

bgColor :: Color
bgColor = makeColorI 30 25 40 255

boardBgColor :: Color
boardBgColor = makeColorI 45 40 60 255

gridLineColor :: Color
gridLineColor = makeColorI 55 50 70 255

-- ============================================================
-- Piece definitions (spawn coordinates)
-- ============================================================

spawnPiece :: PieceType -> Piece
spawnPiece pt = Piece pt (map (\(r,c) -> (r + spawnRow, c + spawnCol)) coords) origin
  where
    spawnRow = boardH - 2
    spawnCol = 3
    origin   = (spawnRow + oR, spawnCol + oC)
    (coords, (oR, oC)) = pieceData pt

pieceData :: PieceType -> ([Coord], Coord)
pieceData I = ([(0,0),(0,1),(0,2),(0,3)], (0,1))
pieceData J = ([(1,0),(0,0),(0,1),(0,2)], (0,1))
pieceData L = ([(1,2),(0,0),(0,1),(0,2)], (0,1))
pieceData O = ([(0,0),(0,1),(1,0),(1,1)], (0,0))
pieceData S = ([(0,1),(0,2),(1,0),(1,1)], (0,1))
pieceData T = ([(1,1),(0,0),(0,1),(0,2)], (0,1))
pieceData Z = ([(0,0),(0,1),(1,1),(1,2)], (0,1))

-- ============================================================
-- Board helpers
-- ============================================================

emptyBoard :: Board
emptyBoard = replicate boardH (replicate boardW Nothing)

getCell :: Board -> Coord -> Maybe PieceType
getCell board (r, c)
  | r < 0 || r >= boardH || c < 0 || c >= boardW = Nothing
  | otherwise = (board !! r) !! c

setCell :: Board -> Coord -> Maybe PieceType -> Board
setCell board (r, c) val =
  take r board ++ [take c row ++ [val] ++ drop (c+1) row] ++ drop (r+1) board
  where row = board !! r

inBounds :: Coord -> Bool
inBounds (r, c) = r >= 0 && r < boardH && c >= 0 && c < boardW

isOccupied :: Board -> Coord -> Bool
isOccupied board coord
  | not (inBounds coord) = True
  | otherwise = getCell board coord /= Nothing

-- ============================================================
-- Collision & placement
-- ============================================================

collides :: Board -> [Coord] -> Bool
collides board = any (isOccupied board)

outOfBounds :: [Coord] -> Bool
outOfBounds = any (\(r,c) -> c < 0 || c >= boardW || r < 0)

canPlace :: Board -> [Coord] -> Bool
canPlace board coords = not (outOfBounds coords) && not (collides board coords)

placePiece :: Board -> Piece -> Board
placePiece board piece = foldl (\b coord -> setCell b coord (Just (pieceType piece))) board (pieceBlocks piece)

-- ============================================================
-- Row clearing
-- ============================================================

isRowFull :: [Maybe PieceType] -> Bool
isRowFull = all (/= Nothing)

clearRows :: Board -> (Board, Int)
clearRows board = (newBoard, cleared)
  where
    remaining = filter (not . isRowFull) board
    cleared   = boardH - length remaining
    newBoard  = replicate cleared (replicate boardW Nothing) ++ remaining

-- ============================================================
-- Rotation (matrix rotation around origin)
-- ============================================================

rotateCW :: Piece -> Piece
rotateCW piece
  | pieceType piece == O = piece
  | otherwise = piece { pieceBlocks = map rotate (pieceBlocks piece) }
  where
    (oR, oC) = pieceOrigin piece
    rotate (r, c) =
      let dr = r - oR
          dc = c - oC
          dr' = dc
          dc' = -dr
      in (oR + dr', oC + dc')

rotateCCW :: Piece -> Piece
rotateCCW = rotateCW . rotateCW . rotateCW

-- ============================================================
-- Movement
-- ============================================================

movePiece :: (Int, Int) -> Piece -> Piece
movePiece (dr, dc) piece = piece
  { pieceBlocks = map (\(r,c) -> (r+dr, c+dc)) (pieceBlocks piece)
  , pieceOrigin = let (oR, oC) = pieceOrigin piece in (oR+dr, oC+dc)
  }

hardDrop :: Board -> Piece -> Piece
hardDrop board piece
  | canPlace board (pieceBlocks moved) = hardDrop board moved
  | otherwise = piece
  where moved = movePiece (-1, 0) piece

ghostPiece :: Board -> Piece -> Piece
ghostPiece = hardDrop

-- ============================================================
-- Game logic
-- ============================================================

tickInterval :: Int -> Float
tickInterval lvl = max 0.05 (0.8 - fromIntegral lvl * 0.07)

scoreForLines :: Int -> Int -> Int
scoreForLines lvl n = case n of
  1 -> 100   * (lvl + 1)
  2 -> 300   * (lvl + 1)
  3 -> 500   * (lvl + 1)
  4 -> 800   * (lvl + 1)
  _ -> 0

tryMove :: Board -> Piece -> Piece -> Piece
tryMove board newP oldP
  | canPlace board (pieceBlocks newP) = newP
  | otherwise = oldP

lockAndSpawn :: GameState -> IO GameState
lockAndSpawn gs = do
  let board'  = placePiece (gsBoard gs) (gsCurrent gs)
      (board'', linesCleared) = clearRows board'
      newScore = gsScore gs + scoreForLines (gsLevel gs) linesCleared
      newLines = gsLines gs + linesCleared
      newLevel = newLines `div` 10
  nextPt <- randomPieceType
  let newPiece = spawnPiece (gsNext gs)
      gameOver = not (canPlace board'' (pieceBlocks newPiece))
  return gs
    { gsBoard     = board''
    , gsCurrent   = newPiece
    , gsNext      = nextPt
    , gsScore     = newScore
    , gsLevel     = newLevel
    , gsLines     = newLines
    , gsGameOver  = gameOver
    , gsDropTrail = 0
    }

randomPieceType :: IO PieceType
randomPieceType = toEnum <$> randomRIO (0, 6)

initGame :: IO GameState
initGame = do
  pt1 <- randomPieceType
  pt2 <- randomPieceType
  return GameState
    { gsBoard     = emptyBoard
    , gsCurrent   = spawnPiece pt1
    , gsNext      = pt2
    , gsScore     = 0
    , gsLevel     = 0
    , gsLines     = 0
    , gsTickTimer = 0
    , gsGameOver  = False
    , gsDropTrail = 0
    }

-- ============================================================
-- Input handling
-- ============================================================

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = return gs { gsCurrent = tryMove (gsBoard gs) (movePiece (0, -1) (gsCurrent gs)) (gsCurrent gs) }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = return gs { gsCurrent = tryMove (gsBoard gs) (movePiece (0,  1) (gsCurrent gs)) (gsCurrent gs) }
handleInput (EventKey (SpecialKey KeyDown)  Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = let moved = movePiece (-1, 0) (gsCurrent gs)
                in if canPlace (gsBoard gs) (pieceBlocks moved)
                   then return gs { gsCurrent = moved, gsScore = gsScore gs + 1 }
                   else lockAndSpawn gs
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = return gs { gsCurrent = wallKickRotate (gsBoard gs) (gsCurrent gs) }
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = do
      let dropped = hardDrop (gsBoard gs) (gsCurrent gs)
          dist = fst (pieceOrigin (gsCurrent gs)) - fst (pieceOrigin dropped)
      gs' <- lockAndSpawn gs { gsCurrent = dropped, gsScore = gsScore gs + dist * 2 }
      return gs' { gsDropTrail = 1.0 }
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleInput (EventKey (Char 'q') Down _ _) _ = exitSuccess
handleInput (EventKey (Char 'Q') Down _ _) _ = exitSuccess
handleInput (EventKey (Char 'r') Down _ _) _ = initGame
handleInput (EventKey (Char 'z') Down _ _) gs
  | gsGameOver gs = return gs
  | otherwise = return gs { gsCurrent = wallKickRotateCCW (gsBoard gs) (gsCurrent gs) }
handleInput _ gs = return gs

wallKickRotate :: Board -> Piece -> Piece
wallKickRotate board piece = tryKicks board piece (rotateCW piece) kicks
  where kicks = [(0,0),(0,-1),(0,1),(0,-2),(0,2),(1,0),(-1,0)]

wallKickRotateCCW :: Board -> Piece -> Piece
wallKickRotateCCW board piece = tryKicks board piece (rotateCCW piece) kicks
  where kicks = [(0,0),(0,1),(0,-1),(0,2),(0,-2),(1,0),(-1,0)]

tryKicks :: Board -> Piece -> Piece -> [(Int,Int)] -> Piece
tryKicks _ original _ [] = original
tryKicks board original rotated ((dr,dc):rest)
  | canPlace board (pieceBlocks kicked) = kicked
  | otherwise = tryKicks board original rotated rest
  where kicked = movePiece (dr, dc) rotated

-- ============================================================
-- Game tick (gravity)
-- ============================================================

stepGame :: Float -> GameState -> IO GameState
stepGame dt gs
  | gsGameOver gs = return gs { gsDropTrail = max 0 (gsDropTrail gs - dt * 3) }
  | otherwise =
      let timer = gsTickTimer gs + dt
          trail = max 0 (gsDropTrail gs - dt * 3)
      in if timer >= tickInterval (gsLevel gs)
         then let moved = movePiece (-1, 0) (gsCurrent gs)
              in if canPlace (gsBoard gs) (pieceBlocks moved)
                 then return gs { gsCurrent = moved, gsTickTimer = 0, gsDropTrail = trail }
                 else do
                   gs' <- lockAndSpawn gs
                   return gs' { gsTickTimer = 0, gsDropTrail = trail }
         else return gs { gsTickTimer = timer, gsDropTrail = trail }

-- ============================================================
-- Rendering
-- ============================================================

render :: GameState -> IO Picture
render gs = return $ Pictures
  [ translate (-boardPixelW / 2 - 40) (-boardPixelH / 2 - 40) $ Pictures
    [ renderBoardBg
    , renderGrid
    , renderPlacedBlocks (gsBoard gs)
    , renderGhost (gsBoard gs) (gsCurrent gs)
    , renderPiece (gsCurrent gs)
    ]
  , renderSidebar gs
  , renderTitle
  , if gsGameOver gs then renderGameOver else Blank
  ]

boardPixelW, boardPixelH :: Float
boardPixelW = fromIntegral boardW * cellSize
boardPixelH = fromIntegral boardH * cellSize

renderBoardBg :: Picture
renderBoardBg =
  Color boardBgColor $
    translate (boardPixelW / 2) (boardPixelH / 2) $
      rectangleSolid (boardPixelW + 4) (boardPixelH + 4)

renderGrid :: Picture
renderGrid = Color gridLineColor $ Pictures $
  [ Line [(fromIntegral c * cellSize, 0), (fromIntegral c * cellSize, boardPixelH)]
    | c <- [0..boardW]
  ] ++
  [ Line [(0, fromIntegral r * cellSize), (boardPixelW, fromIntegral r * cellSize)]
    | r <- [0..boardH]
  ]

renderPlacedBlocks :: Board -> Picture
renderPlacedBlocks board = Pictures
  [ renderCuteBlock (pastelColor pt) (darkerColor pt) c r
  | r <- [0..boardH-1], c <- [0..boardW-1]
  , Just pt <- [getCell board (r, c)]
  ]

renderPiece :: Piece -> Picture
renderPiece piece = Pictures
  [ renderCuteBlock (pastelColor pt) (darkerColor pt) c r
  | (r, c) <- pieceBlocks piece
  ]
  where pt = pieceType piece

renderGhost :: Board -> Piece -> Picture
renderGhost board piece = Pictures
  [ renderGhostBlock (ghostColor pt) c r
  | (r, c) <- pieceBlocks ghost
  ]
  where
    ghost = ghostPiece board piece
    pt    = pieceType piece

renderCuteBlock :: Color -> Color -> Int -> Int -> Picture
renderCuteBlock mainCol darkCol c r =
  let x = fromIntegral c * cellSize
      y = fromIntegral r * cellSize
      pad = 1.5
      s = cellSize - pad * 2
      shine = withAlpha 0.35 white
  in translate (x + cellSize / 2) (y + cellSize / 2) $ Pictures
    [ Color darkCol  $ rectangleSolid s s
    , Color mainCol  $ translate (-1) 1 $ rectangleSolid (s - 3) (s - 3)
    , Color shine    $ translate (-3) 3 $ rectangleSolid (s / 3) (s / 3)
    ]

renderGhostBlock :: Color -> Int -> Int -> Picture
renderGhostBlock col c r =
  let x = fromIntegral c * cellSize
      y = fromIntegral r * cellSize
      pad = 2
      s = cellSize - pad * 2
  in translate (x + cellSize / 2) (y + cellSize / 2) $
    Color col $ rectangleSolid s s

renderSidebar :: GameState -> Picture
renderSidebar gs =
  let xOff = boardPixelW / 2 + 60
      yOff = boardPixelH / 2 - 60
  in translate xOff 0 $ Pictures
    [ translate 0 yOff $ Pictures
        [ Color (makeColorI 255 200 220 255) $ scaleText 0.18 "NEXT"
        , translate 0 (-50) $ renderPreview (gsNext gs)
        ]
    , translate 0 (yOff - 160) $ Pictures
        [ Color (makeColorI 200 220 255 255) $ scaleText 0.18 "SCORE"
        , translate 0 (-35) $ Color white $ scaleText 0.15 (show (gsScore gs))
        ]
    , translate 0 (yOff - 260) $ Pictures
        [ Color (makeColorI 200 255 200 255) $ scaleText 0.18 "LEVEL"
        , translate 0 (-35) $ Color white $ scaleText 0.15 (show (gsLevel gs))
        ]
    , translate 0 (yOff - 360) $ Pictures
        [ Color (makeColorI 255 240 200 255) $ scaleText 0.18 "LINES"
        , translate 0 (-35) $ Color white $ scaleText 0.15 (show (gsLines gs))
        ]
    ]

renderPreview :: PieceType -> Picture
renderPreview pt = scale 0.7 0.7 $ Pictures
  [ renderCuteBlock (pastelColor pt) (darkerColor pt) c r
  | (r, c) <- fst (pieceData pt)
  ]

renderTitle :: Picture
renderTitle =
  translate (-boardPixelW / 2 - 40 + boardPixelW / 2) (boardPixelH / 2 - 15) $
    Color (makeColorI 255 200 230 255) $ scaleText 0.22 "~ TETRIS ~"

renderGameOver :: Picture
renderGameOver = Pictures
  [ Color (makeColorI 0 0 0 180) $ rectangleSolid 800 800
  , translate (-110) 30 $ Color (makeColorI 255 150 170 255) $ scaleText 0.3 "GAME OVER"
  , translate (-90) (-20) $ Color white $ scaleText 0.12 "press R to restart"
  ]

scaleText :: Float -> String -> Picture
scaleText s txt = scale s s $ Text txt

-- ============================================================
-- Main
-- ============================================================

windowWidth, windowHeight :: Int
windowWidth  = 520
windowHeight = 720

main :: IO ()
main = do
  gs <- initGame
  playIO
    (InWindow "~ Tetris ~" (windowWidth, windowHeight) (200, 50))
    bgColor
    60
    gs
    render
    handleInput
    stepGame
