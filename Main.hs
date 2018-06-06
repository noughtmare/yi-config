{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Lens.Micro.Platform ((.=))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Options.Applicative

import qualified Yi.Keymap.Vim as Vim

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Mode.Haskell (fastMode, cleverMode, preciseMode)
import Yi.Config.Simple (addMode)

data CommandLineOptions = CommandLineOptions
  { startOnLine :: Maybe Int
  , mode :: Maybe String
  , files :: [String]
  }

versionFlag :: Parser (Maybe CommandLineOptions)
versionFlag = flag' Nothing $ mconcat
  [ long "version"
  , short 'v'
  , help "Show the version number"
  ]

lineOption :: Parser (Maybe Int)
lineOption = optional $ option auto $ mconcat
  [ long "line"
  , short 'l'
  , metavar "NUM"
  , help "Open the (last) file on line NUM"
  ]

modeOption :: Parser (Maybe String)
modeOption = optional $ strOption $ mconcat
  [ long "mode"
  , short 'm'
  , metavar "HASKELL_MODE"
  , help "The haskell mode to use (fast, clever, or precise)"
  ]

filesList :: Parser [String]
filesList = many (argument str (metavar "FILES..."))

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = versionFlag
  <|> (Just <$> (CommandLineOptions <$> lineOption <*> modeOption <*> filesList))

main :: IO ()
main = do
  mayClo <- execParser opts
  case mayClo of
    Nothing -> putStrLn "Yi 0.17.1"
    Just clo -> do
      let
        openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
        moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
      cfg <- execStateT
        (runConfigM (myConfig (mode clo) >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
        defaultConfig
      startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions) $ mconcat
     [ fullDesc
     , progDesc "Edit files"
     , header "Yi - a flexible and extensible text editor written in haskell"
     ]

modeTable :: [(String, AnyMode)]
modeTable =
  [ ("fast", fastMode)
  , ("clever", cleverMode)
  , ("precise", preciseMode)
  , ("none", 

myConfig :: Maybe String -> ConfigM ()
myConfig m = do
  configureVim
  configureVty
  case m of
    Just "fast" -> addMode fastMode
    Just "clever" -> addMode cleverMode
    Just "precise" -> addMode preciseMode
    Just "none" -> return ()
    _ -> addMode cleverMode
  configureJavaScriptMode
  configureMiscModes
  configUIA . configThemeA .= myTheme
  defaultKmA .= myKeymapSet

myTheme :: Proto UIStyle
myTheme = defaultTheme `override` \super _ -> super

myKeymapSet :: KeymapSet
myKeymapSet = Vim.keymapSet
