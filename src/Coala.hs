{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Coala
    ( Filename(Filename)
    , coala
    , coalaIO
    , Result (..)
    , Severity (..)
    , Line (..)
    , Column (..)
    , Affect (..)
    , CodeRef (line, column)
    , codeRef
    , codeRefLine
    , codeRefInLine
    , FileRef
    , filename
    , RefPoint
    , (-->)
    , (-+>)
    ) where

import Prelude hiding ( getContents, putStr )
import Data.ByteString.Lazy ( putStr, ByteString, getContents )
import Data.Text ( Text, empty )
import Data.Aeson ( ToJSON, toJSON, object, (.=), Value , encode )
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

newtype Filename = Filename String deriving ( Eq, Show )

coala :: String -> (ByteString -> Maybe a) -> (a -> [Result]) -> IO ()
coala bearname reader bear = do
  c <- getContents
  putStr $ encode $ encodeResults bearname $ bear $ fromJust $ reader c

coalaIO :: String -> (ByteString -> Maybe a) -> (a -> IO [Result]) -> IO ()
coalaIO bearname reader bear = do
  c <- getContents
  res <- bear $ fromJust $ reader c
  putStr $ encode $ encodeResults bearname res

data Severity = Info | Normal | Major deriving ( Show, Eq, Ord )

severityToInt :: Severity -> Int
severityToInt s = case s of
                    Info -> 0
                    Normal -> 1
                    Major -> 2

newtype Line = Line Int deriving ( Show, Eq )
newtype Column = Column Int deriving ( Show, Eq )

data Affect = Affect  { start :: CodeRef
                      , end :: CodeRef
                      } deriving ( Eq, Show )

data CodeRef = CodeRef  { file :: Filename
                        , line :: Line
                        , column :: Maybe Column
                        }  deriving (Eq, Show)

data Result = Result  { severity :: Severity
                      , message :: String
                      , affected :: [Affect]
                      } deriving (Eq, Show)

codeRef fn (sl,sc) (el, ec) = Affect (CodeRef fn sl sc) (CodeRef fn el ec)
codeRefInLine :: Filename -> Line -> Maybe Column -> (Int -> Int) -> Affect
codeRefInLine fn sl sc f = Affect (CodeRef fn sl sc) (CodeRef fn sl (sc >>= \c -> Just $ columnMap f c))
codeRefLine fn sl = Affect (CodeRef fn sl Nothing) (CodeRef fn sl Nothing)

lineMap f (Line a) = Line $ f a
columnMap f (Column a) = Column $ f a

class RefPoint a where
  (-->) :: a -> a -> (Filename -> Affect)
  (-+>) :: a -> Int -> (Filename -> Affect)

instance RefPoint Line where
  from --> to = \fn -> codeRef fn (from, Nothing) (to, Nothing)
  from -+> to = \fn -> codeRef fn (from, Nothing) (lineMap (to +) from, Nothing)

instance RefPoint (Line, Column) where
  from --> to = \fn -> codeRef fn (fst from, Just $ snd from) (fst to, Just $ snd to)
  from -+> to = \fn -> codeRefInLine fn (fst from) (Just $ snd from) (to +)

instance RefPoint (Line, Maybe Column) where
  from --> to = \fn -> codeRef fn from to
  from -+> to = \fn -> uncurry (codeRefInLine fn) from (to +)

instance RefPoint (Column, Line) where
  from --> to = swap from --> swap to
  from -+> to = swap from -+> to

instance RefPoint (Maybe Column, Line) where
  from --> to = swap from --> swap to
  from -+> to = swap from -+> to

class FileRef a where
    filename :: a -> Filename

instance FileRef CodeRef where
    filename  = file

instance ToJSON Severity where
    toJSON sev = toJSON $ severityToInt sev

instance ToJSON Filename where
    toJSON (Filename fn) = toJSON fn

instance ToJSON Line where
    toJSON (Line l) = toJSON l

instance ToJSON Column where
    toJSON (Column c) = toJSON c

instance ToJSON CodeRef where
    toJSON ref = object [
        "file" .= file ref
      , "line" .= line ref
      , "column" .= column ref
      ]

instance ToJSON Affect where
  toJSON affect = let s = start affect
                      e = end affect
                  in object [
                      "file" .= if file s == file e
                                then file s
                                else Filename ""
                    , "start" .= s
                    , "end" .= e
                    ]

encodeResult :: String -> Result -> Value
encodeResult bearname r = object [
    "message" .= message r
  , "origin" .= bearname
  , "debug_msg" .= empty
  , "additional_info" .= empty
  , "severity" .= severity r
  , "affected_code" .= affected r
  ]

encodeResults :: String -> [Result] -> Value
encodeResults bearname rs = object [
    "results" .= map (encodeResult bearname) rs
  ]
