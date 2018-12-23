{- A pedagogical implementation of the standard State monad -}
-- This was code provided for Lab5 of CMSC 16100

module State161 where

newtype State s a = State {
    runState :: s -> (a,s)
}

instance Functor (State s) where
    fmap f ma = State $ \s ->
        let (a, s') = runState ma s
        in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    af <*> aa = State $ \s ->
        let (f, s')  = runState af s
            (a, s'') = runState aa s'
        in (f a, s'')

instance Monad (State s) where
    return = pure
    ma >>= f = State $ \s ->
        let (a, s') = runState ma s
            mb = f a
            (b, s'') = runState mb s'
        in (b, s'')

{- constructor -}

state :: (s -> (a,s)) -> State s a
state = State

{- primitive state manipulation functions -}

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s' = State $ \s -> ((),s')

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((),f s)

{- evaluation of state values -}

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)
