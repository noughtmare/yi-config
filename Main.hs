{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State.Lazy (execStateT, gets)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=), (&), (.~))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V

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

import qualified Intero

import Control.Monad.Base (liftBase)
import Control.Monad (void)

import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import qualified Data.Attoparsec.Text as P
import qualified Yi.Rope as R
import Control.Concurrent.MVar
import Data.Text (Text, unpack, pack)

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
    interoVar <- newEmptyMVar
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.14.0"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
            (runConfigM (myConfig interoVar (mode clo) >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
            defaultConfig
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

myConfig :: MVar Intero.Intero -> Maybe String -> ConfigM ()
myConfig interoVar m = do
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
  defaultKmA .= myKeymapSet interoVar

myKeymapSet :: MVar Intero.Intero -> KeymapSet
myKeymapSet interoVar = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
  let eval = V.pureEval this
  in super { V.vimExCommandParsers = exInteroUses interoVar : exInteroTypeAt interoVar : exInteroEval interoVar : exInteroLocAt interoVar : exInteroStart interoVar : V.vimExCommandParsers super }

exInteroStart :: MVar Intero.Intero -> V.EventString -> Maybe V.ExCommand
exInteroStart interoVar "intero-start" = Just $ V.impureExCommand
  { V.cmdShow = "intero-start"
  , V.cmdAction = YiA $ do
     b <- liftBase $ isEmptyMVar interoVar
     if b
       then liftBase (Intero.start "." >>= putMVar interoVar)
       else errorEditor "Cannot intero-start: Already started"
  }
exInteroStart _ _ = Nothing

exInteroEval interoVar = Common.parse $ do
  void $ P.string "intero-eval"
  void $ P.many1 P.space
  command <- P.takeWhile (const True)
  return $ Common.impureExCommand
    { V.cmdShow = "intero-eval " <> command
    , V.cmdAction = YiA $ do
        mayIntero <- liftBase $ tryReadMVar interoVar 
        case mayIntero of
          Nothing -> errorEditor "Cannot intero-eval: Intero not initialized"
          Just intero -> do
            res <- liftBase $ Intero.eval intero (unpack command)
            inNewBuffer "*intero*" (R.fromString res)
    }

exInteroLocAt interoVar = Common.parse $ do
  void $ P.string "intero-loc-at"
  return $ Common.impureExCommand
     { V.cmdShow = "intero-loc-at"
     , V.cmdAction = YiA $ interoLocAt interoVar
     }

exInteroUses interoVar = Common.parse $ do
  void $ P.string "intero-uses"
  return $ Common.impureExCommand
    { V.cmdShow = "intero-uses"
    , V.cmdAction = YiA $ interoUses interoVar
    }

exInteroTypeAt interoVar = Common.parse $ do
  void $ P.string "intero-type-at"
  return $ Common.impureExCommand
    { V.cmdShow = "intero-type-at"
    , V.cmdAction = YiA $ interoTypeAt interoVar
    }

interoLocAt :: MVar Intero.Intero -> YiM ()
interoLocAt interoVar = do
  mayFile <- withCurrentBuffer $ gets file
  case mayFile of
    Nothing -> errorEditor "Cannot intero-loc-at: Not in file"
    Just file -> do
      mayIntero <- liftBase $ tryReadMVar interoVar
      case mayIntero of
        Nothing -> errorEditor "Cannot intero-loc-at: Intero not initialized"
        Just intero -> do
          region <- withCurrentBuffer $ regionOfB unitWord
          range <- regionToRange region
          name <- withCurrentBuffer $ readRegionRopeWithStyleB region Inclusive
          res <- liftBase $ Intero.locAt intero file range (R.toString name)
          inNewBuffer "*intero*" (R.fromString res)
              
interoUses :: MVar Intero.Intero -> YiM ()
interoUses interoVar = do
  mayFile <- withCurrentBuffer $ gets file
  case mayFile of
    Nothing -> errorEditor "Cannot intero-uses: Not in file"
    Just file -> do
      mayIntero <- liftBase $ tryReadMVar interoVar
      case mayIntero of
        Nothing -> errorEditor "Cannot intero-uses: Intero not initialized"
        Just intero -> do
          region <- withCurrentBuffer $ regionOfB unitWord
          range <- regionToRange region
          name <- withCurrentBuffer $ readRegionRopeWithStyleB region Inclusive
          res <- liftBase $ Intero.uses intero file range (R.toString name)
          inNewBuffer "*intero*" (R.fromString res)

interoTypeAt :: MVar Intero.Intero -> YiM ()
interoTypeAt interoVar = do
  mayFile <- withCurrentBuffer $ gets file
  case mayFile of
    Nothing -> errorEditor "Cannot intero-type-at: Not in file"
    Just file -> do
      mayIntero <- liftBase $ tryReadMVar interoVar
      case mayIntero of
        Nothing -> errorEditor "Cannot intero-type-at: Intero not initialized"
        Just intero -> do
          region <- withCurrentBuffer $ regionOfB unitWord
          range <- regionToRange region
          name <- withCurrentBuffer $ readRegionRopeWithStyleB region Inclusive
          res <- liftBase $ Intero.typeAt intero file range (R.toString name)
          inNewBuffer "*intero*" (R.fromString res)
          
regionToRange :: Region -> YiM (Int,Int,Int,Int)
regionToRange region = withCurrentBuffer $ do
  line  <- lineOf $ regionStart region
  col   <- colOf  $ regionStart region
  line' <- lineOf $ regionEnd   region
  col'  <- colOf  $ regionEnd   region
  return (line,col,line',col')

inNewBuffer :: Text -> R.YiString -> YiM ()
inNewBuffer bufName content = withEditor $ withOtherWindow $ do
  void $ newBufferE (MemBuffer bufName) $ content