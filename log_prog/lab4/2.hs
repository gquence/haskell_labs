data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Applicative Tree where
  pure x = Leaf x
  (Branch f1 f2) <*> (Branch x y) = Branch (f1 <*> x) (f2 <*> y)
  -- остальное так же
  (Branch f1 f2) <*> (Leaf x) = Branch (f1 <*> Leaf x) (f2 <*> Leaf x)
  (Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
  (Leaf f) <*> (Leaf x) = Leaf (f x)



--pure id <*> v = v
-- 1) pure id = Leaf id
-- 2) Leaf id <*> v  -> Branch (id <$> x) (id <$> y) ->(закон идентичности функтора) -> Branch x y = v
--                   \-> Leaf (id v) ->(закон идентичности функтора) -> Leaf v = v

-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 1) pure (u <*> (v <*> w)) -> Leaf (u <*> (v <*> w)) = u <*> (v <*> w)

-- pure f <*> pure x = pure (f x)
-- 1) pure f <*> pure x -> (Leaf f) <*> (Leaf x) -> Leaf (f x) = pure (f x) 

-- u <*> pure y = pure ($ y) <*> u
-- ниасилил