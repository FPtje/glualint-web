{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GLualintWeb.Editor where

import qualified "ghcjs-base"     Data.JSString as JS
import qualified "ghcjs-base"     GHCJS.Concurrent as C
import qualified "ghcjs-base"     GHCJS.Foreign.Callback as F
import qualified "ghcjs-dom"      GHCJS.DOM.Types as JS
import           "ghcjs-ffiqq"    GHCJS.Foreign.QQ

import           "reflex-dom"     Reflex.Dom.Class

import           "reflex"         Reflex
import           "reflex"         Reflex.Host.Class
import           "transformers"   Control.Monad.IO.Class (liftIO, MonadIO)
import           "dependent-sum"  Data.Dependent.Sum (DSum (..))
import           "base"           Data.Functor.Identity


data CodeMirror t = CodeMirror
  { cmValue :: Dynamic t String
  }


data CodeMirrorConfig t = CodeMirrorConfig
  { cmcInitialText :: JS.JSString
  , cmcMode :: JS.JSString
  , cmcTheme :: JS.JSString
  , cmcSetText :: Event t String
  }



createCodeMirror :: JS.JSString -> JS.JSString -> JS.JSString -> IO ()
createCodeMirror initValue mode theme = [js_|createCodeMirror(`initValue, `mode, `theme)|]

cmSetText :: JS.JSString -> IO ()
cmSetText str = [js_| editor.setValue(`str) |]

cmGetText :: (MonadIO m) => m String
cmGetText = liftIO $ [js| editor.getValue() |]

cmGetEditor :: (MonadIO m) => m JS.GObject
cmGetEditor = liftIO [js| getEditor() |]


foreign import javascript unsafe
  "createOnEvent($1, $2)"
  cmAddCallback :: JS.JSString -> F.Callback (IO ()) -> IO ()

codemirror :: MonadWidget t m => (CodeMirrorConfig t) -> m (CodeMirror t)
codemirror (CodeMirrorConfig initTxt mode theme setText) = do
  liftIO $ createCodeMirror initTxt mode theme
  performEvent_ $ fmap (liftIO . cmSetText . JS.pack) setText

  postGui <- askPostGui
  runWithActions <- askRunWithActions

  -- Create read event
  ev' <- newEventWithTrigger $ \et -> do
    callback <- F.syncCallback C.ContinueAsync $ do
      txt <- cmGetText
      postGui $ runWithActions [et :=> Identity txt]

    cmAddCallback "change" callback

    return $ F.releaseCallback callback

  value <- holdDyn (JS.unpack initTxt) $ leftmost [setText, ev']

  return $ CodeMirror value
