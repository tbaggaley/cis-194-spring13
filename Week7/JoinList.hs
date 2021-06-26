-- | JoinList: "delayed" appending of sub-trees
--   Metadata stored in monoidal tags on each node

{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import           Buffer
import           Data.Monoid
import           Scrabble
import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving(Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

getSized :: Sized a => a -> Int
getSized = getSize . size

indexedJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexedJ _ Empty        = Nothing
indexedJ 0 (Single _ x) = Just x
indexedJ _ (Single _ _) = Nothing
indexedJ i (Append s l r) | i > getSize (size s) = Nothing
                          | i < lSize            = indexedJ i l
                          | otherwise            = indexedJ (i - lSize) r
  where lSize = getSize (size (tag l))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ i x | i <= 0   = x
dropJ _ (Single _ _) = Empty
dropJ i (Append s l r) | i <= getSized s      = Empty
                       | i < getSized (tag l) = dropJ i l +++ r
                       | otherwise            = dropJ (i - lSize) r
  where lSize = getSized (tag l)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ i x | i <= 0     = Empty
takeJ i x@(Single _ _) = x
takeJ i x@(Append s l r) | i >= getSized s = x
                         | i <= lSize      = takeJ i l
                         | otherwise       = l +++ takeJ (i - lSize) r
  where lSize = getSized (tag l)

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = ""
  toString (Single _ s  ) = s
  toString (Append _ l r) = toString l ++ toString r

  fromString =
    foldr (\line acc -> Single (scoreString line, 1) line +++ acc) Empty . lines

  line 0 (Single _ l) = Just l
  line i (Append (_, s) l r) | i >= aSize = Nothing
                             | i < lSize  = line i l
                             | otherwise  = line (i - lSize) r
   where
    aSize = getSized s
    lSize = getSized (size (tag l))
    rSize = getSized (size (tag r))
  line _ _ = Nothing

  -- replaceLine i ln buf
  replaceLine i _ buf | i < 0   = buf
  replaceLine 0 ln (Single _ _) = Single (scoreString ln, 1) ln
  replaceLine i ln buf@(Append (sc, lines) l r)
    | i > getSized (size lines) = buf
    | i < getSized (size (tag l)) = replaceLine i ln l +++ r
    | otherwise = l +++ replaceLine (i - getSized (size (tag l))) ln r
  replaceLine _ _ buf = buf

  numLines l = getSized $ size $ tag l

  value buf = getScore $ fst $ tag buf

type ScrabbleIndexList = JoinList (Score, Size) String
