{-# LANGUAGE OverloadedStrings #-}

module Generators where

import Control.Monad.Random
import Control.Monad.State
import Data.Array
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T

import Text.Printf
generateNames :: Int -> [Text]
generateNames seed = evalRand (sequence $ repeat generateName) (mkStdGen seed)

generateName :: RandomGen g => Rand g Text
generateName =
  join $
  fromList
    [ (regularTitle, 20 % 100)
    , (landTitle, 30 % 100)
    , (description, 40 % 100)
    , (mcface, 2 % 100)
    ]
  where
    regularTitle = do
      title <- randomElement titles
      firstName <- randomElement firstNames
      return . T.pack $ printf "%s %s" title firstName
    landTitle = do
      firstName <- randomElement firstNames
      place <- randomElement places
      return . T.pack $ printf "%s of %s" firstName place
    description = do
      firstName <- randomElement firstNames
      adjective <- randomElement adjectives
      return . T.pack $ printf "%s the %s" firstName adjective
    mcface = do
      firstName <- randomElement firstNames
      return . T.pack $ printf "%s Mc%sface" firstName firstName

randomElement :: RandomGen g => Array Int a -> Rand g a
randomElement as = do
  i <- getRandomR (bounds as)
  return (as ! i)

toArray :: [a] -> Array Int a
toArray xs = listArray (1, length xs) xs

firstNames :: Array Int Text
firstNames =
  toArray
    [ "Alice"
    , "Alf"
    , "Betty"
    , "Bob"
    , "Carol"
    , "Chas"
    , "Diane"
    , "Dave"
    , "Edith"
    , "Edgar"
    , "Fran"
    , "Frank"
    , "Gertrude"
    , "George"
    , "Hattie"
    , "Horace"
    , "Izzy"
    , "Ignatius"
    , "Julie"
    , "Jacob"
    , "Kelly"
    , "Kevin"
    , "Laura"
    , "Larry"
    , "Mavis"
    , "Marvin"
    , "Nancy"
    , "Neville"
    , "Olivia"
    , "Oscar"
    , "Petunia"
    , "Percy"
    , "Queenie"
    , "Quentin"
    , "Rachel"
    , "Ralph"
    , "Tammy"
    , "Tarquin"
    , "Una"
    , "Ulrich"
    , "Vicky"
    , "Victor"
    , "Winona"
    , "Wallace"
    , "Xena"
    , "Xander"
    , "Zara"
    , "Zack"
    ]

titles :: Array Int Text
titles =
  toArray
    [ "Dr."
    , "Prof."
    , "Cllr."
    , "The Rt. Hon."
    , "Alderman"
    , "Prefect"
    , "Ambassador"
    , "Agent"
    , "Citizen"
    , "Comrade"
    , "Chancellor"
    , "Wing Commander"
    , "The Venerable"
    ]

places :: Array Int Text
places =
  toArray
    [ "Gaunt"
    , "Shoreditch"
    , "Rockall"
    , "Saffron Walden"
    , "Westwood Ho!"
    , "Peppard Common"
    , "Trumpton"
    , "Llanfairpwllgwyngyll"
    , "Boggy Bottom"
    , "Dunny-on-the-Wold"
    , "Brockway"
    , "Ogdenville"
    , "North Haverbrook"
    , "Scarborough Fair"
    ]

adjectives :: Array Int Text
adjectives =
  toArray
    [ "Scurrilous"
    , "Fearful"
    , "Brave"
    , "Pensive"
    , "Bewildered"
    , "Unready"
    , "Amiable"
    , "Cromulent"
    , "Calamitous"
    , "Feckless"
    , "Histrionic"
    , "Stabby"
    , "Messy"
    , "Beneficent"
    , "Grouchy"
    , "Expedient"
    , "Bizarre"
    , "Snazzy"
    , "Not Too Shabby"
    , "Hapless"
    ]

generateColors :: Int -> [Text]
generateColors seed =
  let (genR,genG) = split $ mkStdGen seed
      (genB,_) = split genG
  in zipWith3 toColor
              (randomRs (50,240) genR)
              (randomRs (50,240) genG)
              (randomRs (50,240) genB)
  where toColor :: Int -> Int -> Int -> Text
        toColor r g b = T.pack $ printf "#%02x%02x%02x" r g b
