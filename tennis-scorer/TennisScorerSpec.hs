import Test.Hspec

data TennisMatch = TM Player Player [TennisSet] deriving Show
type Player = String
data TennisSet = TS [TennisGame] deriving Show
data TennisGame = TG [Point] deriving Show
data Point = PT Player deriving (Show, Eq)
data PointName = Zero | Fifteen | Thirty | Forty | Advantage | Game deriving Eq
data GameScore = GS PointName PointName

instance Show PointName where
  show Zero      = "0"
  show Fifteen   = "15"
  show Thirty    = "30"
  show Forty     = "40"
  show Advantage = "Ad"
  show Game      = "0"

gameBetween player1 player2 = TM player1 player2 [newSet]
playingAgainst = gameBetween
newSet  = TS [newGame]
newGame = TG []

pointsPlayer1 (TM player1 _ ((TS (currentGame : _)) : _)) =
  myScore $ pointsOf player1 currentGame

pointsPlayer2 (TM _ player2 ((TS (currentGame : _)) : _)) =
  myScore $ pointsOf player2 currentGame

myScore (GS score _) = score

pointsOf player (TG []) = GS Zero Zero
pointsOf player (TG (point : points)) =
  let previousScore = pointsOf player (TG points)
  in if point `wonBy` player
       then pointWonIn  previousScore
       else pointLostIn previousScore

wonBy (PT pointPlayer) player = player == pointPlayer

pointWonIn (GS Forty Forty)     = GS Advantage Forty
pointWonIn (GS Advantage Forty) = GS Game Forty
pointWonIn (GS Forty Advantage) = GS Forty Forty
pointWonIn (GS score1 score2)   = GS (nextPoint score1) score2

pointLostIn (GS Forty Forty)     = GS Forty Advantage
pointLostIn (GS Advantage Forty) = GS Forty Forty
pointLostIn (GS Forty Advantage) = GS Forty Game
pointLostIn (GS score1 score2)   = GS score1 (nextPoint score2)

pointBy player = PT player

gamesPlayer1 (TM player1 _ ((TS games) : _)) =
  length $ filter (wonGame player1) games
gamesPlayer2 (TM _ player2 ((TS games) : _)) =
  length $ filter (wonGame player2) games

setsPlayer1 (TM player1 _ sets) = length $ filter (wonSet player1) sets
setsPlayer2 (TM _ player2 sets) = length $ filter (wonSet player2) sets

pointToPlayer1 match@(TM player1 player2 _) = pointTo player1 match
pointToPlayer2 match@(TM player1 player2 _) = pointTo player2 match

pointTo player (TM player1 player2 ((TS ((TG points) : games)) : sets)) =
  let gameAfterPoint = TG ((pointBy player) : points)
  in if player `wonGame` gameAfterPoint
       then let setAfterPoint = TS (gameAfterPoint : games)
            in if player `wonSet` setAfterPoint
                 then TM player1 player2 (newSet : setAfterPoint : sets)
                 else TM player1 player2 ((TS (newGame : gameAfterPoint : games)) : sets)
       else TM player1 player2 ((TS (gameAfterPoint : games)) : sets)

wonGame player game = (myScore $ pointsOf player game) == Game

wonSet player (TS games) = (length $ filter (wonGame player) games) == 6

currentGameHistory (TM _ _ ((TS (game : _)) : _)) = gameHistory game

gameHistory (TG points) = reverse points

nextPoint Zero    = Fifteen
nextPoint Fifteen = Thirty
nextPoint Thirty  = Forty
nextPoint Forty   = Game

vilasVsClerc = vilas `playingAgainst` clerc
vilas = "Vilas"
clerc = "Clerc"

score_15_0  = pointToPlayer1 vilasVsClerc
score_0_15  = pointToPlayer2 vilasVsClerc
score_15_15 = pointToPlayer1 score_0_15
score_30_15 = pointToPlayer1 score_15_15
score_30_30 = pointToPlayer2 score_30_15
score_40_30 = pointToPlayer1 score_30_30
score_40_40 = pointToPlayer2 score_40_30
score_ad_40 = pointToPlayer1 score_40_40
score_0_40  = pointToPlayer2 $ pointToPlayer2 $ pointToPlayer2 $ vilasVsClerc

main = hspec $ do

  describe "basic game logic" $ do

    it "returns a 0-0 score for a started game" $ do
      pointsPlayer1 vilasVsClerc `shouldBe` Zero
      pointsPlayer2 vilasVsClerc `shouldBe` Zero

    it "returns a 15-0 score if the first player scores" $ do
      pointsPlayer1 score_15_0 `shouldBe` Fifteen
      pointsPlayer2 score_15_0 `shouldBe` Zero

    it "returns a 0-15 score if the second player scores" $ do
      pointsPlayer1 score_0_15 `shouldBe` Zero
      pointsPlayer2 score_0_15 `shouldBe` Fifteen

    it "returns a 15-15 score if both players score" $ do
      pointsPlayer1 score_15_15 `shouldBe` Fifteen
      pointsPlayer2 score_15_15 `shouldBe` Fifteen

    it "returns a 30-15 score when player1 scores twice, and player2 once" $ do
      pointsPlayer1 score_30_15 `shouldBe` Thirty
      pointsPlayer2 score_30_15 `shouldBe` Fifteen

    it "returns a 40-40 score when both players score 3 times" $ do
      pointsPlayer1 score_40_40 `shouldBe` Forty
      pointsPlayer2 score_40_40 `shouldBe` Forty

    it "returns a Ad-40 score when player1 scores after a 40-40" $ do
      pointsPlayer1 score_ad_40 `shouldBe` Advantage
      pointsPlayer2 score_ad_40 `shouldBe` Forty

    it "goes back to 40-40 when player2 scores after a Ad-40" $ do
      pointsPlayer1 (pointToPlayer2 score_ad_40) `shouldBe` Forty
      pointsPlayer2 (pointToPlayer2 score_ad_40) `shouldBe` Forty

    it "completes a game and reset points to zero if player 1 scores 4 times" $ do
      let gameAfterPoint = pointToPlayer1 score_40_30
      gamesPlayer1 gameAfterPoint `shouldBe` 1
      gamesPlayer2 gameAfterPoint `shouldBe` 0
      pointsPlayer1 gameAfterPoint `shouldBe` Zero
      pointsPlayer2 gameAfterPoint `shouldBe` Zero

    it "completes a game and reset points to zero if player 2 scores 4 times" $ do
      let gameAfterPoint = pointToPlayer2 score_0_40
      gamesPlayer1 gameAfterPoint `shouldBe` 0
      gamesPlayer2 gameAfterPoint `shouldBe` 1
      pointsPlayer1 gameAfterPoint `shouldBe` Zero
      pointsPlayer2 gameAfterPoint `shouldBe` Zero

  describe "points history" $ do

    it "returns the points history for a 0-0" $ do
      currentGameHistory vilasVsClerc `shouldBe` ([] :: [Point])

    it "returns the points history for a 15-0" $ do
      currentGameHistory score_15_0 `shouldBe` [pointBy vilas]

    it "returns the points history for a 15-15, second player scored first" $ do
      currentGameHistory score_15_15 `shouldBe` [pointBy clerc, pointBy vilas]

  describe "basic sets logic" $ do

    it "completes a set if player 1 win 6 consecutive games, reseting games" $ do
      let setWonByVilas = iterate pointToPlayer1 vilasVsClerc !! 24
      setsPlayer1 setWonByVilas `shouldBe` 1
      setsPlayer2 setWonByVilas `shouldBe` 0
      gamesPlayer1 setWonByVilas `shouldBe` 0
      gamesPlayer2 setWonByVilas `shouldBe` 0
      pointsPlayer1 setWonByVilas `shouldBe` Zero
      pointsPlayer2 setWonByVilas `shouldBe` Zero

    it "completes two sets for each player" $ do
      let firstSetWonByVilas = iterate pointToPlayer1 vilasVsClerc !! 24
      let secondSetWonByClerc = iterate pointToPlayer2 firstSetWonByVilas !! 24
      let thirdSetWonByVilas = iterate pointToPlayer1 secondSetWonByClerc !! 24
      let fourthSetWonByClerc = iterate pointToPlayer2 thirdSetWonByVilas !! 24
      setsPlayer1 fourthSetWonByClerc `shouldBe` 2
      setsPlayer2 fourthSetWonByClerc `shouldBe` 2
