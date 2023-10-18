module HW2.T2
  ( joinWith,
    splitOn,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn separator (x : xs) =
  let ret@(retHead :| retTail) = splitOn separator xs
   in if x == separator
        then [] :| toList ret
        else (x : retHead) :| retTail

joinWith :: a -> NonEmpty [a] -> [a]
joinWith connector (listHead :| listTail) = listHead ++ foldr joinConnector [] listTail
  where
    joinConnector x xs = connector : x ++ xs
