import Sized


-- Exercise 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
            deriving (Eq, Show)



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jlx jly = Append (mappend (tag jlx) (tag jly)) jlx jly

tag :: Monoid m => JoinList m a -> m
tag Empty               = mempty
tag (Single mm aa)      = mm
tag (Append mm jlx jly) = mm

-- End Exercise 1


-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty         = Nothing
indexJ 0 (Single mm x) = Just x
indexJ n _     | n < 0 = Nothing
indexJ n (Append mm jlx jly)
    | n > (getSize $ size mm) = Nothing
    | n > lsize               = indexJ (n - lsize) jly
    | otherwise               = indexJ n jlx
    where lsize = getSize . size . tag $ jlx

