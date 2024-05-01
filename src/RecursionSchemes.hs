module RecursionSchemes where

import Control.Arrow ((&&&))

newtype Term f = In { out :: f (Term f) }

bottomUp, topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = f . In . fmap (bottomUp f) . out
topDown f = In . fmap (topDown f) . out . f

type Algebra f a = f a -> a

-- | foldr but for all functors
cata :: Functor f => Algebra f a -> Term f -> a
cata f = f . fmap (cata f) . out

type Coalgebra f a = a -> f a

-- | unfoldr but for all fonctors
ana :: Functor f => Coalgebra f a -> a -> Term f
ana f = In . fmap (ana f) . f

type RAlgebra f a = f (Term f, a) -> a

-- | in more complex catamorphisms looking at the original structure can be handy
para :: Functor f => RAlgebra f a -> Term f -> a
para f = f . fmap (id &&& para f) . out

anacata :: Functor f => (a -> f b -> b) -> (a -> Term f -> a) -> a -> Term f -> b
anacata l2r r2l a term = let a' = r2l a term
                         in  l2r a $ anacata l2r r2l a' <$> out term

-- | the tuple from before can be a little unsightly
type RAlgebra' f a = Term f -> f a -> a

para' :: Functor f => RAlgebra' f a -> Term f -> a
para' alg t = alg t $ fmap (para' alg) $ out t

-- | this symbolizes an anamorphism where
-- | Right symbolizes the continuation of the computation (unfold)
-- | Left symbolizes the end of computation
type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In . fmap (id `either` apo f) . f

-- | a problem with the previous schemes is that previously computed values are not visible
-- | this would be useful in something like a fibonacci function
-- | attribute contains the currently computed value of the fold
-- | hole contains the computation history
data Attr f a = Attr { attribute :: a
                     , hole :: f (Attr f a) }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo f = attribute . worker
  where
    worker = mkAttr . (f &&& id) . fmap worker . out
    mkAttr (a,b) = Attr a b

-- | futumorphism allows us to specify manually how the unfold should proceed at each level
-- | encodes a cellualar automaton
data CoAttr f a = Automatic a
                | Manual (f (CoAttr f a))

type CVCoalgebra f a = a -> f (CoAttr f a)

futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu f = In . fmap worker . f
  where
    worker (Automatic a) = futu f a
    worker (Manual g)    = In $ worker <$> g
