module Check (
  -- * Performs checks on linked syntax tree
  check
  ) where

import CheckAsgn
import CheckMain
import CheckReachable
import CheckReturn
import Grammar
import Link

import Data.List (sortBy)
import Data.Ord
import Data.Set (fromList, toList)
import Text.ParserCombinators.Parsec (ParseError)

check ::    FilePath
         -> Either ParseError (ProgTree LinkId, [ErrMess])
         -> Either String (ProgTree LinkId)
check _ (Left error)                 = Left $ show error
check source (Right (tree, oldMsgs)) =
  let msgs = foldl (\msgs f -> f (tree, msgs) ++ msgs) oldMsgs
        [checkAsgn, checkMain source, checkReturn, checkReachable]
  in if null msgs
     then Right tree
     else Left $ unlines $ toList $ fromList $ fst $ unzip $
            sortBy (comparing snd) msgs

