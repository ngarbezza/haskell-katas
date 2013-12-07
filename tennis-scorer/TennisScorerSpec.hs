import Test.Hspec

data TennisGame = TG String String
data TennisSet = TS TennisGame

game = TS (TG "0" "0")

pointsPlayer1 (TS (TG pp1 _)) = pp1
pointsPlayer2 (TS (TG _ pp2)) = pp2

gamesPlayer1 (TS _) = 1
gamesPlayer2 (TS _) = 0

pointToPlayer1 (TS (TG "40" "Ad")) = TS (TG "40" "40")
pointToPlayer1 (TS (TG "40" "40")) = TS (TG "Ad" "40")
pointToPlayer1 (TS (TG "40" _))    = TS (TG "0" "0")
pointToPlayer1 (TS (TG pp1 pp2))   = TS (TG (nextPoint pp1) pp2)

pointToPlayer2 (TS (TG "Ad" "40")) = TS (TG "40" "40")
pointToPlayer2 (TS (TG "40" "40")) = TS (TG "40" "Ad")
pointToPlayer2 (TS (TG _ "40"))    = TS (TG "0" "0")
pointToPlayer2 (TS (TG pp1 pp2))   = TS (TG pp1 (nextPoint pp2))

nextPoint "0"  = "15"
nextPoint "15" = "30"
nextPoint "30" = "40"
nextPoint "40" = "Ad"

score_15_0  = pointToPlayer1 game
score_0_15  = pointToPlayer2 game
score_15_15 = pointToPlayer1 score_0_15
score_30_15 = pointToPlayer1 score_15_15
score_30_30 = pointToPlayer2 score_30_15
score_40_30 = pointToPlayer1 score_30_30
score_40_40 = pointToPlayer2 score_40_30
score_ad_40 = pointToPlayer1 score_40_40

main = hspec $ do
  it "returns a 0-0 score for a started game" $ do
    pointsPlayer1 game `shouldBe` "0"
    pointsPlayer2 game `shouldBe` "0"

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
