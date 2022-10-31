{-| Helper library for KVList to use in Esqueleto.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.KVList.Esqueleto
  ( SingleValue
  , singleValue
  , ToKVList(..)
  ) where

import Prelude

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Internal.Internal (SqlSelect(..))
import Data.KVList ((:=)((:=)), (&=), KVList, ListKey(..))
import qualified Data.KVList as KVList
import GHC.TypeLits (KnownSymbol)


class ToKVList a kvls | kvls -> a, a -> kvls where
  toKVList :: a -> Either Text kvls

instance
  ( ToKVList a ls
  ) => ToKVList (Maybe a) (Maybe ls) where
  toKVList ma =
    case ma of
      Nothing -> Right Nothing
      Just a ->
        toKVList a
          <&> Just

{-| -}
newtype SingleValue e = SingleValue e

{-| -}
singleValue :: e -> SingleValue e
singleValue = SingleValue

instance
  forall a ra recA.
  ( SqlSelect a ra
  , ToKVList ra recA
  ) => SqlSelect (SingleValue a) (KVList '[ "value" := recA ]) where
  sqlSelectCols esc (SingleValue a) =
    sqlSelectCols esc a
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy a)
  sqlSelectProcessRow vs =
    do
      (ra :: ra) <- sqlSelectProcessRow vs
      recA <- toKVList ra
      pure $ KVList.singleton (#value := recA)

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , KnownSymbol la
  , KnownSymbol lb
  , ToKVList ra recA
  , ToKVList rb recB
  , KVList.HasKey la '[la := a, lb := b] a
  , KVList.HasKey lb '[la := a, lb := b] b
  ) => SqlSelect (KVList '[ la := a, lb := b ]) (KVList '[ la := recA, lb := recB ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , kvs ~ '[ la := a, lb := b, lc := c]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c]) (KVList '[ la := recA, lb := recB, lc := recC ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , SqlSelect l rl

  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , KnownSymbol ll

  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , ToKVList rl recL

  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  , KVList.HasKey ll kvs l

  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK, ll := recL ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k, KVList.get (ListKey :: ListKey ll) r :: l)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk, rl :: rl) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      l <- toKVList rl

      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k
          &= (ListKey :: ListKey ll) := l

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , SqlSelect l rl
  , SqlSelect m rm

  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , KnownSymbol ll
  , KnownSymbol lm

  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , ToKVList rl recL
  , ToKVList rm recM

  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  , KVList.HasKey ll kvs l
  , KVList.HasKey lm kvs m

  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK, ll := recL, lm := recM ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k, KVList.get (ListKey :: ListKey ll) r :: l, KVList.get (ListKey :: ListKey lm) r :: m)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk, rl :: rl, rm :: rm) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      l <- toKVList rl
      m <- toKVList rm

      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k
          &= (ListKey :: ListKey ll) := l
          &= (ListKey :: ListKey lm) := m

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , SqlSelect l rl
  , SqlSelect m rm
  , SqlSelect n rn

  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , KnownSymbol ll
  , KnownSymbol lm
  , KnownSymbol ln

  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , ToKVList rl recL
  , ToKVList rm recM
  , ToKVList rn recN

  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  , KVList.HasKey ll kvs l
  , KVList.HasKey lm kvs m
  , KVList.HasKey ln kvs n

  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK, ll := recL, lm := recM, ln := recN ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k, KVList.get (ListKey :: ListKey ll) r :: l, KVList.get (ListKey :: ListKey lm) r :: m, KVList.get (ListKey :: ListKey ln) r :: n)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m, n))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk, rl :: rl, rm :: rm, rn :: rn) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      l <- toKVList rl
      m <- toKVList rm
      n <- toKVList rn

      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k
          &= (ListKey :: ListKey ll) := l
          &= (ListKey :: ListKey lm) := m
          &= (ListKey :: ListKey ln) := n

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , SqlSelect l rl
  , SqlSelect m rm
  , SqlSelect n rn
  , SqlSelect o ro

  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , KnownSymbol ll
  , KnownSymbol lm
  , KnownSymbol ln
  , KnownSymbol lo

  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , ToKVList rl recL
  , ToKVList rm recM
  , ToKVList rn recN
  , ToKVList ro recO

  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n, lo := o ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  , KVList.HasKey ll kvs l
  , KVList.HasKey lm kvs m
  , KVList.HasKey ln kvs n
  , KVList.HasKey lo kvs o

  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n, lo := o ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK, ll := recL, lm := recM, ln := recN, lo := recO ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k, KVList.get (ListKey :: ListKey ll) r :: l, KVList.get (ListKey :: ListKey lm) r :: m, KVList.get (ListKey :: ListKey ln) r :: n, KVList.get (ListKey :: ListKey lo) r :: o)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk, rl :: rl, rm :: rm, rn :: rn, ro :: ro) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      l <- toKVList rl
      m <- toKVList rm
      n <- toKVList rn
      o <- toKVList ro

      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k
          &= (ListKey :: ListKey ll) := l
          &= (ListKey :: ListKey lm) := m
          &= (ListKey :: ListKey ln) := n
          &= (ListKey :: ListKey lo) := o

instance
  ( SqlSelect a ra
  , SqlSelect b rb
  , SqlSelect c rc
  , SqlSelect d rd
  , SqlSelect e re
  , SqlSelect f rf
  , SqlSelect g rg
  , SqlSelect h rh
  , SqlSelect i ri
  , SqlSelect j rj
  , SqlSelect k rk
  , SqlSelect l rl
  , SqlSelect m rm
  , SqlSelect n rn
  , SqlSelect o ro
  , SqlSelect p rp

  , KnownSymbol la
  , KnownSymbol lb
  , KnownSymbol lc
  , KnownSymbol ld
  , KnownSymbol le
  , KnownSymbol lf
  , KnownSymbol lg
  , KnownSymbol lh
  , KnownSymbol li
  , KnownSymbol lj
  , KnownSymbol lk
  , KnownSymbol ll
  , KnownSymbol lm
  , KnownSymbol ln
  , KnownSymbol lo
  , KnownSymbol lp

  , ToKVList ra recA
  , ToKVList rb recB
  , ToKVList rc recC
  , ToKVList rd recD
  , ToKVList re recE
  , ToKVList rf recF
  , ToKVList rg recG
  , ToKVList rh recH
  , ToKVList ri recI
  , ToKVList rj recJ
  , ToKVList rk recK
  , ToKVList rl recL
  , ToKVList rm recM
  , ToKVList rn recN
  , ToKVList ro recO
  , ToKVList rp recP

  , kvs ~ '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n, lo := o, lp := p ]
  , KVList.HasKey la kvs a
  , KVList.HasKey lb kvs b
  , KVList.HasKey lc kvs c
  , KVList.HasKey ld kvs d
  , KVList.HasKey le kvs e
  , KVList.HasKey lf kvs f
  , KVList.HasKey lg kvs g
  , KVList.HasKey lh kvs h
  , KVList.HasKey li kvs i
  , KVList.HasKey lj kvs j
  , KVList.HasKey lk kvs k
  , KVList.HasKey ll kvs l
  , KVList.HasKey lm kvs m
  , KVList.HasKey ln kvs n
  , KVList.HasKey lo kvs o
  , KVList.HasKey lp kvs p

  ) => SqlSelect (KVList '[ la := a, lb := b, lc := c, ld := d, le := e, lf := f, lg := g, lh := h, li := i, lj := j, lk := k, ll := l, lm := m, ln := n, lo := o, lp := p ]) (KVList '[ la := recA, lb := recB, lc := recC, ld := recD, le := recE, lf := recF, lg := recG, lh := recH, li := recI, lj := recJ, lk := recK, ll := recL, lm := recM, ln := recN, lo := recO, lp := recP ]) where
  sqlSelectCols esc r =
    sqlSelectCols esc (KVList.get (ListKey :: ListKey la) r :: a, KVList.get (ListKey :: ListKey lb) r :: b, KVList.get (ListKey :: ListKey lc) r :: c, KVList.get (ListKey :: ListKey ld) r :: d, KVList.get (ListKey :: ListKey le) r :: e, KVList.get (ListKey :: ListKey lf) r :: f, KVList.get (ListKey :: ListKey lg) r :: g, KVList.get (ListKey :: ListKey lh) r :: h, KVList.get (ListKey :: ListKey li) r :: i, KVList.get (ListKey :: ListKey lj) r :: j, KVList.get (ListKey :: ListKey lk) r :: k, KVList.get (ListKey :: ListKey ll) r :: l, KVList.get (ListKey :: ListKey lm) r :: m, KVList.get (ListKey :: ListKey ln) r :: n, KVList.get (ListKey :: ListKey lo) r :: o, KVList.get (ListKey :: ListKey lp) r :: p)
  sqlSelectColCount _ =
    sqlSelectColCount (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
  sqlSelectProcessRow vs =
    do
      (ra :: ra, rb :: rb, rc :: rc, rd :: rd, re :: re, rf :: rf, rg :: rg, rh :: rh, ri :: ri, rj :: rj, rk :: rk, rl :: rl, rm :: rm, rn :: rn, ro :: ro, rp :: rp) <- sqlSelectProcessRow vs
      a <- toKVList ra
      b <- toKVList rb
      c <- toKVList rc
      d <- toKVList rd
      e <- toKVList re
      f <- toKVList rf
      g <- toKVList rg
      h <- toKVList rh
      i <- toKVList ri
      j <- toKVList rj
      k <- toKVList rk
      l <- toKVList rl
      m <- toKVList rm
      n <- toKVList rn
      o <- toKVList ro
      p <- toKVList rp

      pure $
        KVList.empty
          &= (ListKey :: ListKey la) := a
          &= (ListKey :: ListKey lb) := b
          &= (ListKey :: ListKey lc) := c
          &= (ListKey :: ListKey ld) := d
          &= (ListKey :: ListKey le) := e
          &= (ListKey :: ListKey lf) := f
          &= (ListKey :: ListKey lg) := g
          &= (ListKey :: ListKey lh) := h
          &= (ListKey :: ListKey li) := i
          &= (ListKey :: ListKey lj) := j
          &= (ListKey :: ListKey lk) := k
          &= (ListKey :: ListKey ll) := l
          &= (ListKey :: ListKey lm) := m
          &= (ListKey :: ListKey ln) := n
          &= (ListKey :: ListKey lo) := o
          &= (ListKey :: ListKey lp) := p
