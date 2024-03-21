{-# LANGUAGE DeriveAnyClass #-}

module Main
  ( main
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8
import Data.Vector
import System.Exit
import System.IO
import WithCli

main :: IO ()
main =
  withCliModified
    [ AddVersionFlag "2023.04.24"
    , AddOptionHelp "extraHelp" helpMessage
    , RenameOption "extraHelp" "🛈 " -- yes this is terrible
    ]
    field_n_eq
  where
    helpMessage =
      "(this is not an actual option)\nDESCRIPTION\n    STRING should be JSON\nEXAMPLES\n    printf '[\"foobar\", 10]' | field-n-eq? 0 '\"foobar\"'"

data Options = Options
  { extraHelp :: Maybe String
  } deriving (Show, Generic, HasArguments)

field_n_eq :: Main.Options -> Int -> String -> IO ()
field_n_eq _ column_index match_as_str = do
  match :: Value <- throwDecode $ fromString match_as_str
  csv_line <- getContents'
  array :: Value <- throwDecode $ fromString csv_line
  case parse (withArray "foobar" return) array of
    Success vec ->
      let field_n :: Value = vec ! column_index
       in if field_n == match
            then exitWith ExitSuccess
            else exitWith $ ExitFailure 1
    Error msg -> error msg
