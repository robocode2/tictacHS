import Data.List

-- An open piece has an integer (1+index), Player piece, which is X or O
data Piece =
    Open Int |
    Player Char
    deriving Eq


-- Define show for pieces, so it dsiplays constructor arguments 
instance Show Piece where
    show (Open n) = show n
    show (Player c) = [c]

-- Removes the Nth item (index being N-1) from a list
removeNth :: Int -> [a] -> ([a], [a])
removeNth index lst = (left, right)
    where
        (left, ys) = splitAt (index - 1) lst
        right = drop 1 ys

-- Given a board, piece, and index to place it in, place piece
-- at the position N (index being N-1)
placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
    where (xs, ys) = removeNth index board


-- Return true if piece given is an Open piece
pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _        = False


openSpace :: [Piece] -> Int -> Either String Bool
openSpace board index
    | index < 1 || index > length board = Left "Choose spaces (1-9)"
    | pieceIsOpen $ board !! (index - 1) = Right True
    | otherwise = Left "Space is not open. Choose another."


getPiecePosition :: [Piece] -> IO Int
getPiecePosition board = do
    putStrLn "Enter an open position (1-9):"
    input <- getLine
    if length input == 1 && head input `elem` ['1'..'9']
        then do
            let position = read [head input]
            case openSpace board position of
                Left errorMessage -> do
                    putStrLn errorMessage
                    getPiecePosition board
                Right pieceIsOpen -> if pieceIsOpen
                                then return position
                                else do
                                    getPiecePosition board
        else do
            putStrLn "Error: Please enter a valid digit (1-9)."
            getPiecePosition board



-- Makes a single Line of three items in a board list
showBoardLine :: [Piece] -> String
showBoardLine (a:b:c:xs) = show a ++ " | " ++ show b ++ " | " ++ show c
showBoardLine _ = error "List must contain at least three elements"

-- Border to separate board lines 
boardBorder :: String
boardBorder = "\n---------\n"

-- Given the board, turns that board into a string to print out
showBoard :: [Piece] -> String
showBoard board = intercalate boardBorder [top, middle, bottom]
    where
        top = showBoardLine board
        middle = showBoardLine (drop 3 board)
        bottom = showBoardLine (drop 6 board)


-- Given current character char (piece), give other character char (piece)
swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _ = error "swapPlayers only accepts the characters O or X"

-- Given a board, player piece, and position on board, check if
-- the player given won vertically starting from the given position
checkWonVertically :: [Piece] -> Piece -> Int -> Bool
checkWonVertically board player index = topPos == player && middlePos == player && bottomPos == player
    where
        topPos = board !! index
        middlePos = board !! (index + 3)
        bottomPos = board !! (index + 6)

-- Will return true if the player given won at all vertically
playerWonVertically :: [Piece] -> Piece -> Bool
playerWonVertically board player = any (checkWonVertically board player) [0, 1, 2]

-- Given a board, player, and position, check if the 
-- pplayer won by making a full row of their piece
checkWonHorizontally :: [Piece] -> Piece -> Int -> Bool
checkWonHorizontally board player index = firstPos == player && secondPos == player && thirdPos == player
    where
        firstPos = board !! index
        secondPos = board !! (index + 1)
        thirdPos = board !! (index + 2)

--Will return true if the player given won at all horizontally 
playerWonHorizontally :: [Piece] -> Piece -> Bool
playerWonHorizontally board player = any (checkWonHorizontally board player) [0, 3, 6]


-- Given board, player, starting pos, and step, return true if the next three
-- pieces on the board are all the players piece (they won)
checkWonDiagonally :: [Piece] -> Piece -> Int -> Int -> Bool
checkWonDiagonally board player index step = firstPos == player && secondPos == player && thirdPos == player
    where
        firstPos = board !! index
        secondPos = board !! (index + step)
        thirdPos = board !! (index + 2 * step)

-- Given board, player, return true if they won at all diagonally
playerWonDiagonally :: [Piece] -> Piece -> Bool
playerWonDiagonally board player = wonFirstDiagonal || wonSecondDiagonal
    where
        wonFirstDiagonal = checkWonDiagonally board player 0 4
        wonSecondDiagonal = checkWonDiagonally board player 2 2

-- Given a board, a player, return true if they won at all
playerWon :: [Piece] -> Piece -> Bool
playerWon board player = playerWonDiagonally board player || playerWonVertically board player || playerWonHorizontally board player


-- Return true if the game has become a tie
tieGame :: [Piece] -> Bool
tieGame board = all (\piece -> not (pieceIsOpen piece)) board

runTicTacToe :: [Piece] -> Char -> IO ()
runTicTacToe board playerChr = do
    putStrLn $ showBoard board  -- Print the initial state of the board
    playGame board playerChr

playGame :: [Piece] -> Char -> IO ()
playGame board playerChr = do
    rawChoice <- getPiecePosition board
    let newBoard = placePiece board (Player playerChr) rawChoice
    putStrLn $ showBoard newBoard  -- Print the current state of the board
    if playerWon newBoard (Player playerChr)
        then putStrLn $ "Player " ++ [playerChr] ++ " won!"
    else if tieGame newBoard
        then putStrLn "It's a tie!"
    else
        playGame newBoard (if playerChr == 'X' then 'O' else 'X')


main :: IO ()
main = runTicTacToe board 'X'
    where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
