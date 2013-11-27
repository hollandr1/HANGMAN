import Data.Char 
import Data.List
import System.IO 
import System.Random 
import Control.Monad.State 

data GameState = GameState { 
      gsAnswer  :: String,       -- the answer 
      gsKnown   :: [Maybe Char], -- partial answer known to the user 
      gsGuesses :: [Char],       -- incorrect letters guessed so far 
      gsWrong   :: Int,          -- number of incorrect guesses 
      gsWonLost :: Maybe Bool    -- Just true = won, Just false = lost 
    } 
  deriving (Show) 

newGameState :: String -> GameState 
newGameState answer = GameState{ 
                   gsAnswer = map toUpper answer, 
                   gsKnown = (map (filt $ not . isAlpha) answer), 
                   gsGuesses = [], 
                   gsWrong = 0, 
                   gsWonLost = Nothing} 

data UserInput = UIGuess Char | UIQuit | UINewGame | UIRefresh 
  deriving (Show) 

main :: IO () 
main = do 
  hSetBuffering stdout NoBuffering 
  putStrLn $ "*---------------------------*"

  putStrLn $ "|    Welcome to Hangman!    |"

  putStrLn $ "*---------------------------*"
  putStr instructions 
  runStateT startNewGame undefined 
  return ()

startNewGame :: StateT GameState IO() 
startNewGame = do 
 dict <- liftIO $ wordList
 nWord <- liftIO $ getStdRandom (randomR (0,length dict - 1)) 
 let word = dict !! nWord 
 let gs = newGameState word 
 put gs 
 liftIO $ putStrLn $ renderGameState gs 
 gameLoop

gameLoop :: StateT GameState IO() 
gameLoop = do 
  ui <- liftIO getUserInput 
  case ui of 
    UIGuess c -> do 
      modify $ handleGuess c 
      gs <- get 
      liftIO $ putStrLn $ renderGameState gs 
      case (gsWonLost gs) of 
        Nothing -> gameLoop 
        Just True -> do 
          liftIO $ putStrLn "Congratulations, you won!" 
          startNewGame 
        Just False -> do 
          liftIO $ putStrLn "You've been hanged!" 
          liftIO $ putStrLn $ "The word was \'" ++ (gsAnswer gs) ++ "\'." 
          startNewGame 
    UIQuit -> do 
      gs <- get 
      liftIO $ putStrLn $ "The word was \'" ++ (gsAnswer gs) ++ "\'." 
      liftIO $ putStrLn "Thank you for playing!" 
    UINewGame -> do 
      gs <- get 
      liftIO $ putStrLn $ "The word was \'" ++ (gsAnswer gs) ++ "\'." 
      startNewGame 
    UIRefresh -> do 
      gs <- get 
      liftIO $ putStrLn $ renderGameState gs 
      gameLoop 

getUserInput :: IO UserInput 
getUserInput = do 
  putStr "Hangman> " 
  response <- getLine 
  if null response 
    then getUserInput 
    else do 
      let c:cs = response 
      if isAlpha c 
        then return $ UIGuess $ toUpper c 
        else if c == ':' && not (null cs) 
          then case toLower (head cs) of 
                 'q' -> return UIQuit 
                 'n' -> return UINewGame 
                 'r' -> return UIRefresh 
                 '?' -> do 
                   putStr instructions 
                   getUserInput 
                 otherwise -> do 
                   putStrLn $ "Unknown command \'" ++ cs ++ "\'" 
                   putStrLn $ "Use \':?\' for help." 
                   getUserInput 
          else do 
            putStrLn $ "Invalid input \'" ++ response ++ "\'" 
            putStrLn $ "Use \':?\' for help." 
            getUserInput 

instructions :: String 
instructions = 
    "Instructions:\n" 
 ++ "To guess a letter, type the letter and press enter.\n" 
 ++ "To quit or restart the game, use the following commands:\n" 
 ++ "  :q = quit\n" 
 ++ "  :n = new game\n" 
 ++ "  :r = re-display the game state\n" 
 ++ "  :? = show instructions\n" 
 ++ "\n" 

filt :: (a -> Bool) -> a -> Maybe a 
filt pred x = if pred x then Just x else Nothing 

handleGuess :: Char -> GameState -> GameState 
handleGuess ch state = 
    if (elem ch $ gsGuesses state) 
      then state 
      else 
    if (elem ch $ gsAnswer state) 
      then let revealed = map (filt (== ch)) (gsAnswer state) 
               known = zipWith mplus (gsKnown state) revealed 
               won = all (maybe False (const True)) known 
           in state{gsKnown = known, gsWonLost = filt id won} 
      else let wrong = 1 + (gsWrong state) 
           in state{gsGuesses = ch:(gsGuesses state), 
                    gsWrong = wrong, 
                    gsWonLost = filt not (wrong < 11)} 

wordList :: IO [String]
wordList = getWords =<< (return "4words.txt")
 where

  getWords filePath = return . concatMap words . lines =<< readFile filePath


renderGameState :: GameState -> String 
renderGameState gs = 
    let noose = renderNoose $ gsWrong gs 
        report = ["","The Word:","",word,"","Your Guesses:","",guessed] 
        word = intersperse ' ' $ map (maybe '_' id) (gsKnown gs) 
        guessed = gsGuesses gs 
    in (concat $ zipWith (++) noose $ map (++ "\n") report) 

renderNoose :: Int -> [String] 
renderNoose n | n <= 0 = [ 
 "           ", 
 "           ", 
 "           ", 
 "           ", 
 "           ", 
 "           ", 
 "           ", 
 " -+-       "]
renderNoose 1 = [
 "           ", 
 "           ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 2 = [
 "   ___     ", 
 "  /   |    ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 3 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 4 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  |   |    ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 5 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|    ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 6 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |        ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 7 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |   |    ", 
 "  |        ", 
 "  |        ", 
 " -+-       "] 
renderNoose 8 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |   |    ", 
 "  |  /     ", 
 "  |        ", 
 " -+-       "] 
renderNoose 9 = [
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |   |    ", 
 "  |  / \\   ", 
 "  |        ", 
 " -+-       "] 

renderNoose 10 = [
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |   |    ", 
 "  | _/ \\  ", 
 "  |        ", 
 " -+-       "] 
renderNoose n | n >= 11 = [ 
 "   ___     ", 
 "  /   |    ", 
 "  |   O    ", 
 "  | --|--  ", 
 "  |   |    ", 
 "  | _/ \\_  ", 
 "  |        ", 
 " -+-       "] 