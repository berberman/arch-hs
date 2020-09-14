module Polysemy.Req
  ( reqToIO,
  )
where

import Network.HTTP.Req
import Polysemy

reqToIO :: forall a r. Member (Embed IO) r => Req a -> Sem r a
reqToIO r = embed @IO $ runReq defaultHttpConfig r