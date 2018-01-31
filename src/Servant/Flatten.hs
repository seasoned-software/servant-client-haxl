{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Servant.Flatten where
-- From alp, #servant @ Freenode

import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Haxl.Core (GenHaxl)
import Servant.API
import Servant.Client.Core (clientIn)
import Servant.Client.Haxl ()

-- | Client that flattens the API type.
--client' :: forall u cli api. (HasClient (GenHaxl u) (Flat api), ) => Proxy api -> Client (GenHaxl u) (Flat api)
client' api = flatten api `clientIn` (Proxy :: Proxy (GenHaxl u))

-- | Flatten an API type (through a proxy).
flatten :: Proxy api -> Proxy (Flat api)
flatten Proxy = Proxy

-- | Flatten and transform the API type a little bit.
type Flat api = Reassoc (Flatten (Reassoc (Flatten api)))
-- looks like Flatten/Reassoc are missing some opportunities the first time,
-- so we apply them twice for now...

-- | Completely flattens an API type by applying a few simple transformations.
--   The goal is to end up with an API type where things like @a :> (b :<|> c)@
--   are rewritten to @a :> b :<|> a :> c@, so as to have client with very simple
--   types, instead of "nested clients".
type family Flatten (api :: k) :: k where
  Flatten ((a :: k) :> (b :<|> c)) = Flatten (a :> b) :<|> Flatten (a :> c)
  Flatten ((a :: k) :> b)          = Redex b (Flatten b) a
  Flatten (a :<|> b)               = Flatten a :<|> Flatten b
  Flatten (a :: k)                 = a

type family Redex a b (c :: k) :: * where
  Redex a a first = Flatten first :> a
  Redex a b first = Flatten (first :> b)

-- | Reassociates ':<|>'.
type family Reassoc api where
  Reassoc ((a :<|> b) :<|> c) = Reassoc a :<|> Reassoc (b :<|> c)
  Reassoc (a :<|> b) = a :<|> Reassoc b
  Reassoc a = a

-- * Funny and somewhat useful thing we can define with a flat representation

-- | Get the endpoints with given indices in the all-flat
--   representation of the API type, glueing them together
--   with ':<|>'.
type family Nths (idxs :: [Nat]) api where
  Nths  '[i]      api = Nth i api
  Nths  (i ': is) api = Nth i api :<|> Nths is api

-- | Get the endpoint with given index in the all-flat representation
--   of the API type.
type family Nth (i :: Nat) api where
  Nth 0 (a :<|> b) = a
  Nth 0 a          = a
  Nth n (a :<|> b) = Nth (n - 1) b
