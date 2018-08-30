{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits
import GHC.Types
import Data.Proxy

type family SuccFib (a :: [Nat]) :: Nat where
  SuccFib (x2:x1:xs) = x2 + x1

type family ListHead (as :: [a]) :: a where
  ListHead (a1:as) = a1

baseFib :: Proxy ('[0,1] :: [Nat])
baseFib = Proxy

nextFib :: Proxy (nats :: [Nat]) -> Proxy ((SuccFib nats) ': nats)
nextFib l = Proxy

headFib :: Proxy (nats :: [Nat]) -> Proxy (ListHead nats)
headFib l = Proxy

-- TODO: FIX THIS MESS
heterogeneousIterate :: (nats ~~ [Nat], nats' ~~ [Nat], nats ~~ nats') => (Proxy nats -> Proxy nats') -> Proxy [Nat] -> Int -> Proxy nats'
heterogeneousIterate f base 0 = base
heterogeneousIterate f base n = heterogeneousIterate f (f base) (n - 1)

main :: IO ()
main =
  printFib $ (nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib . nextFib) baseFib
  --mapM_ printFib (iterate (nextFib) baseFib)
  --printFib $ heterogeneousIterate nextFib baseFib 10
  where
    printFib = (print . natVal . headFib)
