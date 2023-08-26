{-# LANGUAGE LambdaCase #-}
module LikhaMinimaxSpec (spec) where

import Test.Hspec ( describe, it, shouldSatisfy, Spec )
import Test.QuickCheck (Testable(property))

import Data.Either (isLeft, isRight)
import Data.List ((\\))

import ArbitraryGameState (ArbitraryFullGameState(..), ArbitraryFullPostGiftState(..), ArbitraryFullPreGiftState (..))

import Cards (Card)
import LikhaGame (Player, players, Table (..), moves)
import LikhaMinimax (nextStates)
import LikhaGameState (FullGameState(..), hands, PlayerState (..), turn)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
    describe "nextStates" $ do
        it "generates Left for PostGift, Right for PreGift" $ property $
            \(ArbitraryFullGameState fgs) -> nextStates fgs
                `shouldSatisfy` if isPreGift fgs then isRight else isLeft
        it "generates one-card choices for postGift state" $ property $
            \(ArbitraryFullPostGiftState pss hist) -> nextStates (FullPostGift pss hist)
                `shouldSatisfy` \case Left children -> all (legalOneCardMove (FullPostGift pss hist)) children
                                      Right _ -> False
        it "generates 3-card choices for preGift state" $ property $
            \(ArbitraryFullPreGiftState p pss) -> nextStates (FullPreGift p pss)
                `shouldSatisfy` \case Left _ -> False
                                      Right cells -> (all (all (all (all (legalGiftMove (FullPreGift p pss))))) $ take 3 $ map (take 3 . map (take 3 . map (take 3))) cells)

-- checks form not content
legalGiftMove :: FullGameState -> FullGameState -> Bool
legalGiftMove parent child = length diffs == 4 && all ((==) 3 . length . snd) diffs
    where diffs = cardDiffs parent child

-- checks for soundness not completeness
legalOneCardMove :: FullGameState -> FullGameState -> Bool
legalOneCardMove parent child =
        -- one player dealt
        length diffs == 1 &&
        -- one card was dealt
        ((==) 1 . length . snd . head) diffs &&
        -- correct player dealt card
        ((==) (turn parent) . fst . head) diffs && 
        -- dealt card is pushed to table
        lastCardInHistory (fromJust $ history child) == head (snd $ head diffs) && 
        -- dealt card is a legal nmove for player
        legalSuitGivenHistory (fromJust $ history parent) (head $ snd $ head diffs) 
    where diffs = cardDiffs parent child

          history (FullPostGift _ hist) = Just hist
          history (FullPreGift _ _) = Nothing

          lastCardInHistory [] = error "history should not be empty"
          lastCardInHistory (current:rest) = if null $ cards current then lastCardInHistory rest else last $ cards current

          legalSuitGivenHistory [] _ = error "history should not be empty"
          legalSuitGivenHistory hist card = card `elem` moves hist parentTurnHand

          parentTurnHand = hand $ head $ filter ((==) (turn parent) . player) $ hands parent

cardDiffs :: FullGameState -> FullGameState -> [(Player, [Card])]
cardDiffs parent child = filter (not. null . snd) $ do
    p <- players
    let parentHand = hand $ head $ filter ((==) p . player) $ hands parent
    let childHand = hand $ head $ filter ((==) p . player) $ hands child
    return (p, parentHand \\ childHand)

isPreGift :: FullGameState -> Bool
isPreGift (FullPreGift _ _) = True
isPreGift (FullPostGift _ _) = False