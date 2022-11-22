import Data.Map

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

class Functor' f where  
    fmap' :: (a -> b) -> f a -> f b  

instance Functor' (Map k) where
    fmap' f mymap = if mymap == (fromList [])
                    then (fromList [])
                    else fromList $ (fst.head.toList $ mymap, f.snd.head.toList $ mymap) : (toList.fmap' f $ fromList.tail.toList $ mymap)  




