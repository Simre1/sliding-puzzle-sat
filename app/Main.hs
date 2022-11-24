module Main where

import Control.Monad
import Data.Foldable
import Data.Text (Text, pack)
import Formula
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Prelude hiding (not, (&&), (||))

-- Print Limboole formula for the game
-- Run in this way: ./formula 12 1 2 3 8 7 4 6 5 0
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 10) incorrectArgs
  (steps : fields) <- traverse (maybe incorrectArgs pure) (readMaybe <$> args)
  printFormula literalToText $ formula fields steps
  where
    incorrectArgs = fail "Need 10 arguments with the fields in left to right top down order:\n./formula <step> <field1> ... <field9>\nExample: ./formula 12 1 2 3 8 7 4 6 5 0"

-- Formula, taking the initial numbers and the number of steps
formula :: [Int] -> Int -> Formula Literal
formula initialNumbers steps =
  initialState initialNumbers
    && finalState (Step steps)
    && allTrue
      ( forEach [(Step 0) .. Step steps - 1] $ \step ->
          actions step && atLeastOneAction step && atMostOneAction step && frameAxioms step
      )

initialState :: [Int] -> Formula Literal
initialState numbers = allTrue $ zipWith (setNumberForField 0) (toEnum <$> numbers) [A .. I]

finalState :: Step -> Formula Literal
finalState step = allTrue $ zipWith (setNumberForField step) [N1, N2, N3, N4, N5, N6, N7, N8, N0] [A .. I]

atLeastOneAction :: Step -> Formula Literal
atLeastOneAction step = anyTrue $ literal <$> allActionLiterals step

atMostOneAction :: Step -> Formula Literal
atMostOneAction step = allTrue $ forEach actionLiterals $ \actionLiteral ->
  let otherActions = literal <$> filter (actionLiteral /=) actionLiterals
   in literal actionLiteral --> allFalse otherActions
  where
    actionLiterals = allActionLiterals step

actions :: Step -> Formula Literal
actions step = allTrue $ action <$> allActionLiterals step
  where
    -- Creates the transition relation for one action
    action :: Literal -> Formula Literal
    action moveLit@(Move from to step) = literal moveLit --> moveN0ToFrom && moveNXToTo
      where
        moveN0ToFrom =
          literal (NumberAtFieldInStep to N0 step)
            && not (literal (NumberAtFieldInStep to N0 (step + 1)))
            && literal (NumberAtFieldInStep from N0 (step + 1))
            && not (literal (NumberAtFieldInStep from N0 step))
        moveNXToTo = allTrue $ forEach [N1 .. N8] $ \number ->
          literal (NumberAtFieldInStep from number step)
            --> literal (NumberAtFieldInStep to number (step + 1))
            && not (literal (NumberAtFieldInStep from number (step + 1)))

frameAxioms :: Step -> Formula Literal
frameAxioms step = allTrue $ frameAxiom step <$> [A .. I]
  where
    -- Creates the frame axiom for a Field. When the Number on a Field changes, a corresponding action must be triggered.
    frameAxiom :: Step -> Field -> Formula Literal
    frameAxiom step field =
      let n0Moves =
            allTrue
              [ fieldLiteral N0 step && not (fieldLiteral N0 (step + 1)) --> anyActionsMovingTo field,
                not (fieldLiteral N0 step) && fieldLiteral N0 (step + 1) --> anyActionsMovingFrom field
              ]
          nXMoves = allTrue $ flip foldMap [N1 .. N8] $ \nX ->
            [ fieldLiteral nX step && not (fieldLiteral nX (step + 1)) --> anyActionsMovingFrom field,
              not (fieldLiteral nX step) && fieldLiteral nX (step + 1) --> anyActionsMovingTo field
            ]
       in n0Moves && nXMoves
      where
        anyActionsMovingTo field = anyTrue $ literals $ filter (\(Move _ to _) -> to == field) (allActionLiterals step)
        anyActionsMovingFrom field = anyTrue $ literals $ filter (\(Move from _ _) -> from == field) (allActionLiterals step)
        fieldLiteral number s = literal (NumberAtFieldInStep field number s)

data Field = A | B | C | D | E | F | G | H | I deriving (Show, Enum, Bounded, Eq, Ord)

data Number = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 deriving (Show, Enum, Bounded, Eq, Ord)

newtype Step = Step Int deriving (Show, Bounded, Eq, Ord, Enum, Num)

-- Datatype for literals
data Literal = NumberAtFieldInStep Field Number Step | Move Field Field Step deriving (Show, Eq)

-- Describes how to serialize the Literal datatype for limboole
literalToText :: Literal -> Text
literalToText = \case
  NumberAtFieldInStep field number step -> fieldToText field <> "_" <> numberToText number <> "_" <> stepToText step
  Move from to step -> "mv_" <> fieldToText from <> fieldToText to <> "_" <> stepToText step
  where
    fieldToText :: Field -> Text
    fieldToText = pack . show
    numberToText :: Number -> Text
    numberToText = pack . show
    stepToText :: Step -> Text
    stepToText (Step i) = pack $ show i

-- Ensures that a Number must be at a Field in a Step and in no other Fields.  
setNumberForField :: Step -> Number -> Field -> Formula Literal
setNumberForField step number field =
  let fieldWithTheNumber = literal $ NumberAtFieldInStep field number step
      makeFalseField f = not $ Literal $ NumberAtFieldInStep f number step
      fieldsWhichDoNotHaveTheNumber = allTrue $ makeFalseField <$> filter (/= field) [A .. I]
   in fieldWithTheNumber && fieldsWhichDoNotHaveTheNumber

allActionLiterals :: Step -> [Literal]
allActionLiterals step = flip foldMap waysToGo $ \(from, tos) -> forEach tos $ \to -> Move from to step

waysToGo :: [(Field, [Field])]
waysToGo =
  [ (A, [B, D]),
    (B, [A, C, E]),
    (C, [B, F]),
    (D, [A, E, G]),
    (E, [B, D, F, H]),
    (F, [C, E, I]),
    (G, [D, H]),
    (H, [E, G, I]),
    (I, [F, H])
  ]

forEach :: Functor f => f a -> (a -> b) -> f b
forEach = flip fmap