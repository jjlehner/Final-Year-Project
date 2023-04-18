{-# LANGUAGE DataKinds #-}
import GHC.TypeNats
import Data.Vector.Sized
import Control.Monad.State.Lazy
import Data.Finite.Internal
data Fifo (n :: Nat) (a :: *) = Fifo {
    buffer :: Vector n a,
    size :: Int
}

fifoFull :: Fifo a n -> Bool
fifoFull = undefined


fifoPushElement :: a -> State (Fifo n a) ()
fifoPushElement element = do
                            start <- get
                            let newSize = size start + 1
                            let newBuffer = buffer start // [(0, element)]
                            put $ Fifo {buffer=newBuffer, size=newSize}

newtype ST s a = ST (s -> (a,s))

app :: ST s a -> s -> (a, s)
app (ST st) x = st x
instance Functor (ST a) where
    fmap g st = ST (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (ST a) where
    pure f = ST (\s -> (f, s))
    stf <*> stx = ST (\s ->
                        let (a1, sa) = app stf s
                            (a2, saa) = app stx sa
                        in  (a1 a2, saa))

instance Monad (ST a) where
    -- (>>=) :: ST a -> ( a -> ST b ) -> ST b
    st >>= f = ST (\s -> let (x,sa) = app st s in app (f x) sa)

main :: IO ()
main = putStrLn "Hello, Haskell!"
