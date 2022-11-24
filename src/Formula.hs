module Formula where

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.ByteString (toStrict)
import Data.List ( foldl' )
import qualified Text.Builder as B
import System.Process.Typed ( readProcessStdout_, shell )
import qualified Data.Text.IO as T


data Formula a
  = Top
  | Bottom
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Not (Formula a)
  | Imply (Formula a) (Formula a)
  | Equivalence (Formula a) (Formula a)
  | Literal a
  deriving Show

literal :: a -> Formula a
literal = Literal

literals :: [a] -> [Formula a]
literals = fmap Literal

not :: Formula a -> Formula a
not = Not

top :: Formula a
top = Top

bottom :: Formula a
bottom = Bottom

(&&) :: Formula a -> Formula a -> Formula a
(&&) = And
infixl 6 &&

(||) :: Formula a -> Formula a -> Formula a
(||) = Or
infixl 5 ||

(-->) :: Formula a -> Formula a -> Formula a
(-->) = Imply
infixl 4 -->

(<->) :: Formula a -> Formula a -> Formula a
(<->) = Equivalence
infixl 3 <->

allTrue :: [Formula a] -> Formula a
allTrue [] = Top
allTrue (a:as) = foldl' And a as

anyTrue :: [Formula a] -> Formula a
anyTrue [] = Bottom
anyTrue (f:fs) = foldl' Or f fs

allFalse ::  [Formula a] -> Formula a
allFalse formulas = Not $ anyTrue formulas

anyFalse ::  [Formula a] -> Formula a
anyFalse formulas = Not $ allTrue formulas


makeLimbooleFormula :: (a -> Text) -> Formula a -> Text
makeLimbooleFormula literalToText formula = B.run $ transform formula
  where
    transform Top = "(a | !a)"
    transform Bottom = "(a & !a)"
    transform (Not a) = "!" <> transform a
    transform (Literal i) = B.text $ literalToText i
    transform (And a b) = combine "&" a b
    transform (Or a b) = combine "|" a b
    transform (Imply a b) = combine "->" a b
    transform (Equivalence a b) = combine "<->" a b

    combine operator a b = "(" <> transform a <> " " <> operator <> " " <> transform b <> ")"

runLimbooleSatisfiability :: (a -> Text) -> Formula a -> IO ()
runLimbooleSatisfiability literalToText formula = do
  outputBS <- readProcessStdout_ $ 
    shell ("echo \"" ++ unpack limbooleFormula ++ "\" | limboole")
  let outputText = decodeUtf8Lenient $ toStrict outputBS
  T.putStrLn outputText
  where limbooleFormula = makeLimbooleFormula literalToText formula

printFormula :: (a -> Text) -> Formula a -> IO ()
printFormula literalToText formula = T.putStrLn limbooleFormula
  where limbooleFormula = makeLimbooleFormula literalToText formula

toText :: Show a => a -> Text
toText = pack . show