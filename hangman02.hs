-- two player hangman
--primitive getCh :: IO Char

hangman :: IO ()
hangman = 
	do putStrLn "Enter a word: "
	   word <- sgetLine
	   putStrLn "Guess the Word:"
	   guess word
	

sgetLine :: IO String
sgetLine  = do x <- getChar
               if x == '\n' then
                  do putChar x
                     return []
                else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)


guess :: String -> IO ()
guess word =
   do putStr "> "
      xs <- getLine
      if xs == word then
         putStrLn "That is Correct"
       else
         do putStrLn (diff word xs)
            guess word

diff :: String -> String -> String -> String
diff xs ys =
    [if elem x ys then x else '_' | x <- xs] 
	
