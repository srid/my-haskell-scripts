{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (Semigroup, sconcat, stimes, (<>))

class Scale a where
  scaleTo :: a -> Float -> a

instance Scale Float where
  scaleTo = (*)

type Grams = Float

newtype Calories = Calories Float deriving (Show, Num, Scale)
newtype Fat = Fat Grams deriving (Show, Num, Scale)
newtype Protein = Protein Grams deriving (Show, Num, Scale)
newtype Carb = Carb Grams deriving (Show, Num, Scale)
newtype Fiber = Fiber Grams deriving (Show, Num, Scale)

data Food
  = Food { name :: String
         , weight :: Grams
         , calories :: Calories
         , fat :: Fat
         , protein :: Protein
         , carb :: Carb
         , fiber :: Fiber
         }
  deriving (Show)

instance Scale Food where
  scaleTo (Food m w c f p b r) n =
    Food m
        (scaleTo w n )
        (scaleTo c n)
        (scaleTo f n)
        (scaleTo p n)
        (scaleTo b n)
        (scaleTo r n)

instance Semigroup Food where
  (<>) (Food n1 w1 c1 f1 p1 b1 r1) (Food n2 w2 c2 f2 p2 b2 r2) =
    Food (n1 <> ", " <> n2) (w1+w2) (c1+c2) (f1+f2) (p1+p2) (b1+b2) (r1+r2)

-- Meat has fat and protein (no carb or fiber), so
-- we need jsut the two arguments.
makeMeat :: String -> Grams -> Calories -> Fat -> Protein -> Food
makeMeat n g kal fat_ protein_ =
  Food n g kal fat_ protein_ (Carb 0) (Fiber 0)

setName :: String -> Food -> Food
setName value food = food { name = value }

inGrams :: Grams -> Food -> Food
inGrams grams food = scaleTo food (grams / weight food)

multiple :: Int -> Food -> Food
multiple n food = stimes n food & setName newName
  where newName = (name food) <> " (" <> show n <> ")"

ribeye :: Food
ribeye = makeMeat "ribeye" 100 (Calories 291) (Fat 22) (Protein 24)

provigoRibeye :: Food
provigoRibeye = inGrams 350 ribeye & setName "Provigo Ribeye"

costcoRibeye :: Food
costcoRibeye = inGrams 500 ribeye & setName "Costco Ribeye"

avocado :: Food
avocado = Food "avocado" 100 (Calories 160) (Fat 15) (Protein 2) (Carb 9) (Fiber 7)

nsAvocado :: Food
nsAvocado = inGrams 200 avocado & setName "NS Avocado"

egg :: Food
egg = Food "egg" 100 (Calories 155) (Fat 11) (Protein 13) (Carb 1) (Fiber 0)

largeEgg :: Food
largeEgg = inGrams 50 egg & setName "Large Egg"

butter :: Food
butter = Food "butter" 113 (Calories 810) (Fat 92) (Protein 1) (Carb 0) (Fiber 0)

oneButterBlock :: Food
oneButterBlock = inGrams 18 butter & setName "Butter block"

omlette :: Int -> Food
omlette eggs = multiple eggs egg

cookedInButter :: Food -> Food
cookedInButter food = food <> oneButterBlock

myMacro :: Food
myMacro = Food "MACRO" (-1) (Calories 2472) (Fat 215) (Protein 134) (Carb 0) (Fiber 0)

main :: IO ()
main = do
  print $ sconcat $
         (provigoRibeye & cookedInButter)
    :| [ multiple 2 nsAvocado
       , omlette 4 & cookedInButter
       ]
  print myMacro
