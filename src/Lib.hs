{-# LANGUAGE BangPatterns #-}

module Lib where
    
fi fm 0 = 1
fi fm 1 = 1
fi fm x = fm (x-1) + fm (x-2)

l = map (fi fib) [0..]
fib = (l !!)


f :: (Integer -> Integer) -> Integer -> Integer
f mf 0 = 0
f mf n = max n $ mf (n `div` 2) +
                 mf (n `div` 3) +
                 mf (n `div` 4)

data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Integer -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

nats :: Tree Integer
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

fTree :: Tree Integer
fTree = fmap (f fastestF) nats

fastestF :: Integer -> Integer
fastestF = index fTree
