module LCD where

toLCD n = do
  mapM_ (\f -> putStrLn $ showLine f n) [firstRow, secondRow, thirdRow]

showLine rowF n =
  foldl makeLine "" (num2Digs n)
    where makeLine str d = str ++ (rowF $ digit d) ++ " "

num2Digs 0 = []
num2Digs d = num2Digs (d `div` 10) ++ [d `mod` 10]

data LCDDigit = LCDD Int (String, String, String)

number    (LCDD n _)         = n
firstRow  (LCDD _ (s, _, _)) = s
secondRow (LCDD _ (_, s, _)) = s
thirdRow  (LCDD _ (_, _, s)) = s

digit n = head $ filter ((== n) . number) lcdDigits

lcdDigits = [zero, one, two, three, four, five, six, seven, eight, nine]
zero  = LCDD 0 (" _ ", "| |", "|_|")
one   = LCDD 1 ("   ", "  |", "  |")
two   = LCDD 2 (" _ ", " _|", "|_ ")
three = LCDD 3 (" _ ", " _|", " _|")
four  = LCDD 4 ("   ", "|_|", "  |")
five  = LCDD 5 (" _ ", "|_ ", " _|")
six   = LCDD 6 (" _ ", "|_ ", "|_|")
seven = LCDD 7 (" _ ", "  |", "  |")
eight = LCDD 8 (" _ ", "|_|", "|_|")
nine  = LCDD 9 (" _ ", "|_|", "  |")
