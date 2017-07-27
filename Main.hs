{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=), (&), (.~))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)

import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)

import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)

import Yi.Mode.Haskell (fastMode, cleverMode, preciseMode)
import Yi.Config.Simple (addMode)

import Control.Monad (void)

import Yi.Intero
import qualified Data.Attoparsec.Text as P
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Data.Text (Text,unpack)

data CommandLineOptions = CommandLineOptions
  { startOnLine :: Maybe Int
  , mode :: Maybe String
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing
                       ( long "version"
                      <> short 'v'
                      <> help "Show the version number")
  <|> (Just <$> (pure CommandLineOptions
    <*> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> optional (strOption
        ( long "mode"
       <> short 'm'
       <> metavar "HASKELL_MODE"
       <> help "The haskell mode to use (fast, clever, or precise)"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.14.0"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
            (runConfigM (myConfig (mode clo) >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
            defaultConfig
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

myConfig :: Maybe String -> ConfigM ()
myConfig m = do
  configureVim
  configureVty
  case m of
    Just "fast" -> addMode fastMode
    Just "clever" -> addMode cleverMode
    Just "precise" -> addMode preciseMode
    Just "none" -> return ()
    _ -> addMode (cleverMode & modeAdjustBlockA .~ (\_ _ -> return ()))
  configureJavaScriptMode
  configureMiscModes
  defaultKmA .= myKeymapSet

-- Intero

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super _ -> super
  { V.vimExCommandParsers = interoExCommands <> V.vimExCommandParsers super
  }

interoExCommands :: [V.EventString -> Maybe V.ExCommand]
interoExCommands = [exInteroUses,exInteroTypeAt,exInteroEval,exInteroLocAt,exInteroStart]

exInteroEval :: V.EventString -> Maybe V.ExCommand
exInteroEval = Common.parse $ do
  void $ P.string "intero-eval"
  void $ P.many1 P.space
  instr <- P.takeWhile (const True)
  return $ Common.impureExCommand
    { V.cmdShow = "intero-eval " <> instr
    , V.cmdAction = interoEval (unpack instr)
    }

parseText :: Text -> Action -> V.EventString -> Maybe V.ExCommand
parseText txt action = Common.parse $ do
  void $ P.string txt
  return $ Common.impureExCommand
    { V.cmdShow = txt
    , V.cmdAction = action
    }

exInteroStart, exInteroLocAt,exInteroUses,exInteroTypeAt
  :: V.EventString -> Maybe V.ExCommand
exInteroStart  = parseText "intero-start"   interoStart
exInteroLocAt  = parseText "intero-loc-at"  interoLocAt
exInteroUses   = parseText "intero-uses"    interoUses
exInteroTypeAt = parseText "intero-type-at" interoTypeAt