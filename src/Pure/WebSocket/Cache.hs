{-# language PatternSynonyms, ExplicitNamespaces, RankNTypes, LambdaCase, TypeOperators, DataKinds, TypeApplications, TypeFamilies, PartialTypeSignatures, FlexibleContexts, ExistentialQuantification, OverloadedStrings #-}
{-# options_ghc -fno-warn-partial-type-signatures #-}
module Pure.WebSocket.Cache (Policy(..),cache,req,with) where

import Pure.Elm (publish,pattern SimpleHTML,pattern Applet,pattern Null,command,subscribe,View,Elm,run)
import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Maybe (producingKeyed)
import Pure.WebSocket as WS (request,API,type (∈),WebSocket,Request(..))

import Data.Map as Map (Map,empty,insert,lookup,singleton)

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import Data.Foldable (for_)
import Data.Proxy (Proxy)
import Data.Typeable (typeRep,TypeRep)
import Unsafe.Coerce (unsafeCoerce)

data RequestMap
  = forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
  => RequestMap (Proxy rq) (Map.Map pl (Either [WS.Rsp rq -> IO ()] (WS.Rsp rq)))

data Model = Model
  { responses :: Map.Map TypeRep RequestMap
  }

data Msg
  = forall rq rqs msgs pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True)
  => Request Bool Bool (WS.API msgs rqs) (Proxy rq) pl (WS.Rsp rq -> IO (WS.Rsp rq)) (WS.Rsp rq -> IO ())

  | forall rq pl rsp. (WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl)
  => Satisfy (Proxy rq) pl rsp

  | Startup

cache :: WS.WebSocket -> View
cache = run (Applet [Startup] [] [] (pure mdl) update view)
  where
    mdl = Model Map.empty

type Update = Elm Msg => WS.WebSocket -> Model -> IO Model

update :: Msg -> Update
update = \case
  Startup -> startup
  Request f bp api p pl process cb -> request' f bp api p pl process cb
  Satisfy p pl rsp -> satisfy p pl rsp

startup :: Update
startup _ mdl = do
  subscribe
  pure mdl

request' :: forall rq rqs msgs pl
          . (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True)
         => Bool -> Bool -> WS.API msgs rqs -> Proxy rq -> pl -> (WS.Rsp rq -> IO (WS.Rsp rq)) -> (WS.Rsp rq -> IO ()) 
         -> Update
request' force bypass api p pl process cb ws mdl 
  | force = do
    WS.request api ws p pl $ \rsp -> do
      rsp' <- process rsp
      if bypass
        then cb rsp'
        else command (Satisfy p pl rsp')
    pure mdl 
      { responses = 
        if bypass 
          then responses mdl
          else Map.insert (typeRep p) (RequestMap p (Map.singleton pl ((Left [cb])))) (responses mdl)
      }

  | otherwise =
    case Map.lookup (typeRep p) (responses mdl) of
      Nothing -> do
        WS.request api ws p pl $ \rsp -> do
          rsp' <- process rsp
          if bypass
            then cb rsp'
            else command (Satisfy p pl rsp')
        pure mdl 
          { responses = 
            if bypass
              then responses mdl
              else Map.insert (typeRep p) (RequestMap p (Map.singleton pl ((Left [cb])))) (responses mdl)
          }
          
      Just (RequestMap _ rm) ->
        -- that's a lot of coercions
        case Map.lookup pl (unsafeCoerce rm) of
          Just (Left cbs) -> do
            let 
              cbs' = cbs ++ [unsafeCoerce cb]
              rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs') (unsafeCoerce rm)))
            pure mdl
              { responses = Map.insert (typeRep p) rm' (responses mdl)
              }

          Just (Right rsp) -> do
            cb rsp
            pure mdl

          Nothing -> do
            WS.request api ws p pl (command . Satisfy p pl)
            let 
              cbs = [cb]
              rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs) (unsafeCoerce rm)))
            pure mdl
              { responses = Map.insert (typeRep p) rm' (responses mdl)
              }

satisfy :: forall rq pl rsp
         . (WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl)
        => Proxy rq -> pl -> rsp 
        -> Update
satisfy p pl rsp _ mdl = do
  case Map.lookup (typeRep p) (responses mdl) of
    Just (RequestMap _ rm) ->
      case Map.lookup (unsafeCoerce pl) rm of
        Just (Left cbs) -> do
          for_ @[] (unsafeCoerce cbs) ($ rsp)
          pure mdl
            { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
            } 

        Just (Right _) -> do
          pure mdl
          
        Nothing -> do
          -- huh?
          pure mdl
            { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
            } 

    Nothing -> do
      -- This case allows for pre-seeding the cache with responses.
      pure mdl
        { responses = Map.insert (typeRep p) (RequestMap p (Map.singleton pl (Right rsp))) (responses mdl)
        } 
      
view _ _ = SimpleHTML "pure-websocket-cache"

data Policy = Cached | Fresh | Uncached

req :: (Ord payload, WS.Request request, ToJSON payload, FromJSON response, _) 
    => Policy -> WS.API msgs reqs -> Proxy request -> payload -> IO response
req Uncached api rq pl = do
  mv <- newEmptyMVar
  publish (Request True True api rq pl pure (putMVar mv))
  takeMVar mv
req Fresh api rq pl = do
  mv <- newEmptyMVar
  publish (Request True False api rq pl pure (putMVar mv))
  takeMVar mv
req _ api rq pl = do
  mv <- newEmptyMVar
  publish (Request False False api rq pl pure (putMVar mv))
  takeMVar mv

-- not the best place for this
with :: forall request msgs reqs payload response. (Ord payload, WS.Rsp request ~ response, _ ) 
     => Policy
     -> WS.API msgs reqs
     -> Proxy request 
     -> payload 
     -> (payload -> Maybe response -> View) 
     -> View
with policy api rq pl f = producingKeyed pl (req policy api rq) f