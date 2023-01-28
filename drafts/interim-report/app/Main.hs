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


main :: IO ()
main = putStrLn "Hello, Haskell!"
