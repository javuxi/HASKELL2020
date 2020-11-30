class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

ifTE :: (YesNo a) => a -> b -> b -> b
ifTE cond ifTrue ifFalse = if (yesno cond) then 
                            ifTrue
                        else
                            ifFalse

whileYN :: (YesNo a) => a -> b -> (a -> b -> (a, b)) -> b
whileYN cond state f = if not (yesno cond) then
                        state
                    else 
                        whileYN (fst (f cond state)) (snd (f cond state)) f