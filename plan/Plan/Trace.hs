module Plan.Trace (tracePlan, runPlanTrace) where

import Distribution.ArchHs.Internal.Prelude
import System.IO (Handle, hFlush, hPutStrLn)

tracePlan :: Member Trace r => String -> Sem r ()
tracePlan = trace . ("[plan] " <>)

runPlanTrace :: Member (Embed IO) r => Bool -> Handle -> Sem (Trace ': r) a -> Sem r a
runPlanTrace False _ = ignoreTrace
runPlanTrace True handle = interpret $ \case
  Trace message -> when ("[plan] " `isPrefixOf` message) $ embed $ do
    hPutStrLn handle message
    hFlush handle
