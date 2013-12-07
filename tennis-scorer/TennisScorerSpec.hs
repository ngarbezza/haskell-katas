import Test.Hspec

data TennisGame = TG [Point] deriving Show
data TennisSet = TS [TennisGame] deriving Show
type Player = String
data Point = PT Player deriving (Show, Eq)
data TennisMatch = TM Player Player TennisSet deriving Show

game = gameBetween vilas clerc

gameBetween player1 player2 = TM player1 player2 newSet
newSet  = TS [newGame]
newGame = TG []

pointsPlayer1 (TM p1 _ (TS ((TG points):otherGames))) =
  let (p1p, p2p) = pointsOf p1 points in p1p

pointsPlayer2 (TM _ p2 (TS ((TG points):otherGames))) =
  let (p2p, p1p) = pointsOf p2 points in p2p

pointsOf player [] = ("0", "0")
pointsOf player (point:points) =
  if point `wonBy` player
    then pointWonIn  (pointsOf player points)
    else pointLostIn (pointsOf player points)

wonBy (PT pointPlayer) player = player == pointPlayer

pointWonIn ("40", "40") = ("Ad", "40")
pointWonIn ("Ad", "40") = ("G", "40")
pointWonIn ("40", "Ad") = ("40", "40")
pointWonIn (p1, p2) = ((nextPoint p1), p2)

pointLostIn ("40", "40") = ("40", "Ad")
pointLostIn ("Ad", "40") = ("40", "40")
pointLostIn ("40", "Ad") = ("40", "G")
pointLostIn (p1, p2) = (p1, (nextPoint p2))

pointFor player = PT player

gamesPlayer1 (TM _ _ _) = 1
gamesPlayer2 (TM _ _ _) = 0

pointToPlayer1 (TM p1 p2 (TS (currentGame@(TG ps):otherGames))) =
  let gameAfterPoint = TG ((pointFor p1) : ps)
  in if p1 `wonGame` gameAfterPoint
      then TM p1 p2 (TS (newGame : (gameAfterPoint : otherGames)))
      else TM p1 p2 (TS ((TG ((pointFor p1) : ps)):otherGames))

pointToPlayer2 (TM p1 p2 (TS ((TG ps):otherGames))) =
  TM p1 p2 (TS ((TG ((pointFor p2) : ps)):otherGames))

wonGame player (TG points) =
  let (myPoints, _) = pointsOf player points
  in myPoints == "G"

currentGameHistory (TM _ _ (TS (currentGame:otherGames))) =
  gameHistory currentGame

gameHistory (TG points) = reverse points

nextPoint "0"  = "15"
nextPoint "15" = "30"
nextPoint "30" = "40"
nextPoint "40" = "G"

score_15_0  = pointToPlayer1 game
score_0_15  = pointToPlayer2 game
score_15_15 = pointToPlayer1 score_0_15
score_30_15 = pointToPlayer1 score_15_15
score_30_30 = pointToPlayer2 score_30_15
score_40_30 = pointToPlayer1 score_30_30
score_40_40 = pointToPlayer2 score_40_30
score_ad_40 = pointToPlayer1 score_40_40

vilas = "Vilas"
clerc = "Clerc"

main = hspec $ do

  describe "basic game logic" $ do

    it "returns a 0-0 score for a started game" $ do
      pointsPlayer1 game `shouldBe` "0"
      pointsPlayer2 game `shouldBe` "0"
      putStrLn (show score_ad_40)

    it "returns a 15-0 score if the first player scores" $ do
      pointsPlayer1 score_15_0 `shouldBe` "15"
      pointsPlayer2 score_15_0 `shouldBe` "0"

    it "returns a 0-15 score if the second player scores" $ do
      pointsPlayer1 score_0_15 `shouldBe` "0"
      pointsPlayer2 score_0_15 `shouldBe` "15"

    it "returns a 15-15 score if both players score" $ do
      pointsPlayer1 score_15_15 `shouldBe` "15"
      pointsPlayer2 score_15_15 `shouldBe` "15"

    it "returns a 30-15 score when player1 scores twice, and player2 once" $ do
      pointsPlayer1 score_30_15 `shouldBe` "30"
      pointsPlayer2 score_30_15 `shouldBe` "15"

    it "returns a 40-40 score when both players score 3 times" $ do
      pointsPlayer1 score_40_40 `shouldBe` "40"
      pointsPlayer2 score_40_40 `shouldBe` "40"

    it "returns a Ad-40 score when player1 scores after a 40-40" $ do
      pointsPlayer1 score_ad_40 `shouldBe` "Ad"
      pointsPlayer2 score_ad_40 `shouldBe` "40"

    it "goes back to 40-40 when player2 scores after a Ad-40" $ do
      pointsPlayer1 (pointToPlayer2 score_ad_40) `shouldBe` "40"
      pointsPlayer2 (pointToPlayer2 score_ad_40) `shouldBe` "40"

    it "completes a game and reset points to zero if player 1 scores 4 times" $ do
      gamesPlayer1 (pointToPlayer1 score_40_30) `shouldBe` 1
      gamesPlayer2 (pointToPlayer1 score_40_30) `shouldBe` 0
      pointsPlayer1 (pointToPlayer1 score_40_30) `shouldBe` "0"
      pointsPlayer2 (pointToPlayer1 score_40_30) `shouldBe` "0"

  describe "games history" $ do

    it "returns the game history for a 0-0" $ do
      currentGameHistory game `shouldBe` ([] :: [Point])

    it "returns the game history for a 15-0" $ do
      currentGameHistory score_15_0 `shouldBe` [pointFor vilas]

    it "returns the game history for a 15-15, second player scored first" $ do
      currentGameHistory score_15_15 `shouldBe` [pointFor clerc, pointFor vilas]
