data NotEmpty a = LastValue a | MidValue a (NotEmpty a) deriving (Eq, Show)


data Singleton a = Singleton a deriving (Eq, Show)

instance Functor Singleton where
    fmap f (Singleton a) = Singleton (f a)

instance Applicative Singleton  where
    pure a = Singleton a
    (Singleton f) <*> (Singleton a) = Singleton (f a)

instance Monad Singleton where
    (Singleton a) >>= (f) = f a

instance Foldable Singleton where
    foldMap f (Singleton a) = f a

instance Traversable Singleton where
    traverse f (Singleton a) = fmap Singleton (f a)



data Productish a b = Productish a b deriving (Eq, Show)

instance Functor (Productish a) where
    fmap f (Productish a b) = Productish a (f b)

instance (Monoid a) => Applicative (Productish a) where
    pure a = Productish mempty a
    (Productish f1 f2) <*> (Productish a b) = Productish (f1 <> a) (f2 b)

instance (Monoid a) => Monad (Productish a) where
    (Productish a b) >>= (f) = f_monmonoid a (f b)

f_monmonoid a (Productish t v) = Productish (a <> t) v

instance Foldable (Productish a) where
    foldMap f (Productish _ b) = f b

instance Traversable (Productish a) where
  sequenceA (Productish a b) = fmap (Productish a) b



data Summish a b = First a | Second b deriving (Eq, Show)

instance Functor (Summish a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Summish a) where
    pure a = Second a
    (Second f) <*> (Second a) = Second (f a)
    --(Second f) <*> (First a) = 
    --(First f) <*> (First a) = 
    --(First f) <*> (Second a) = 

instance Monad (Summish a) where
    (First a) >>= _ = First a
    (Second b) >>= (f) = (f b)

instance Foldable (Summish a) where
    foldr _ g (First _) = g
    foldr f g (Second b) = f b g

instance Traversable (Summish a) where
    traverse f (Second b) = fmap Second (f b)
    traverse _ (First a) = pure (First a)



data Optional a = NoValue | HasValue a deriving (Eq, Show)

instance Functor Optional where
    fmap _ NoValue = NoValue
    fmap f (HasValue a) = HasValue (f a)

instance Applicative Optional where
    pure = HasValue
    (NoValue) <*> _ = NoValue
    _ <*> (NoValue) = NoValue
    (HasValue f) <*> (HasValue a) = HasValue (f a)

instance Monad Optional where
   NoValue >>= _ = NoValue
   (HasValue a) >>= (f) = f a

instance Foldable Optional where
    foldr _ b NoValue = b
    foldr f b (HasValue a) = f a b

instance Traversable Optional where
    traverse _ NoValue = pure NoValue
    traverse f (HasValue a) = fmap HasValue (f a)




data NotQuiteList a = Value a | Layer (NotQuiteList a) deriving (Eq, Show)

instance Functor NotQuiteList where
  fmap f (Value a) = Value (f a)
  fmap f (Layer a) = Layer (fmap f a)

instance Applicative NotQuiteList where
    pure = Value
    (Value f) <*> (Value a) = Value (f a)
    (Layer f) <*> (a) = Layer (f <*> a)
    (f) <*> (Layer a) = Layer (f <*> a)

instance Monad NotQuiteList where
    (Value a) >>= (f) = f a
    (Layer a) >>= (f) = Layer (a >>= f)

instance Foldable NotQuiteList where
    foldr f b (Value a) = f a b
    foldr f b (Layer a) = foldr f b a

instance Traversable NotQuiteList where
    traverse f (Value a) = fmap Value (f a)
    traverse f (Layer a) = fmap Layer (traverse f a)

