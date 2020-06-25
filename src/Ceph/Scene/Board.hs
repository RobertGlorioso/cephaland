{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Ceph.Scene.Board where


import Apecs
import Ceph.Components
import Ceph.Jams
import Linear (V2(..))
import qualified Data.Vector as V
import Euterpea hiding (Text)
import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Distributive
import Control.Monad

defineSB :: SCoord -> (Position)
defineSB  (SCoordF S4 SI _) = Position $ V2 (-30) 30 
defineSB  (SCoordF S4 SII _) = Position $ V2 (-10) 30 
defineSB  (SCoordF S4 SIII _) = Position $ V2 (10) 30 
defineSB  (SCoordF S4 SIV _) = Position $ V2 (30) 30 
defineSB  (SCoordF S3 SI _) = Position $ V2 (-30) 10 
defineSB  (SCoordF S3 SII _) = Position $ V2 (-10) 10 
defineSB  (SCoordF S3 SIII _) = Position $ V2 (10) 10 
defineSB  (SCoordF S3 SIV _) = Position $ V2 (30) 10 
defineSB  (SCoordF S2 SI _) = Position $ V2 (-30) (-10) 
defineSB  (SCoordF S2 SII _) = Position $ V2 (-10) (-10) 
defineSB  (SCoordF S2 SIII _) = Position $ V2 (10) (-10) 
defineSB  (SCoordF S2 SIV _) = Position $ V2 (30) (-10) 
defineSB  (SCoordF S1 SI _) = Position $ V2 (-30) (-30) 
defineSB  (SCoordF S1 SII _) = Position $ V2 (-10) (-30) 
defineSB  (SCoordF S1 SIII _) = Position $ V2 (10) (-30) 
defineSB  (SCoordF S1 SIV _) = Position $ V2 (30) (-30)  

defineIBPos :: ICoord -> Position
defineIBPos (ICoordF (C,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = pi/2
defineIBPos (ICoordF (Cs,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 2 * pi/3
defineIBPos (ICoordF (D,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 5 * pi/6
defineIBPos (ICoordF (Ef,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = pi
defineIBPos (ICoordF (E,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 7 * pi/6
defineIBPos (ICoordF (F,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 4 * pi/3
defineIBPos (ICoordF (Fs,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 3 * pi/2
defineIBPos (ICoordF (G,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 5 * pi/3
defineIBPos (ICoordF (Af,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 11 * pi/6
defineIBPos (ICoordF (A,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = 0
defineIBPos (ICoordF (Bf,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = pi/6
defineIBPos (ICoordF (B,_) _ _) = Position $ V2 (negate $ cos t) (negate $ sin t) where t = pi/3

listToBoard :: (Functor f, Representable f, Enum (Rep f)) => [a] -> f a
listToBoard es = tabulate (\s -> es !! fromEnum s)

defineIBoard :: ICoord -> Music Pitch
defineIBoard (ICoordF p m _) = Prim . Note m $ p

musicClock :: MusicClock
musicClock = zipR (tabulate defineIBoard, tabulate defineIBPos)

updateBoard ::
  (Eq (f ()), Adjunction f g) => g b -> f () -> b -> g b
updateBoard b s a = leftAdjunct (updateB b s a) ()
  where updateB board ixToChange update anyIx = if ixToChange == anyIx then update else rightAdjunct (const board) anyIx
modifyBoard ::
  (Eq (f ()), Adjunction f g) => g b -> f () -> (() -> b -> b) -> g b
modifyBoard b s m = leftAdjunct (updateB b s m) ()
  where updateB board ixToChange update anyIx = if ixToChange == anyIx then (zapWithAdjunction (flip update) board ixToChange ) else rightAdjunct (const board) anyIx

getBRow :: SRow -> SBoard a -> (a,a,a,a)
getBRow S1 (SBoard a _ _ _) = a  
getBRow S2 (SBoard _ a _ _) = a  
getBRow S3 (SBoard _ _ a _) = a  
getBRow S4 (SBoard _ _ _ a) = a  

fillBRow :: (a,a,a,a) -> SBoard a
fillBRow a = SBoard a a a a
  
bdSing :: SCoord -> SCoord -> Bool
bdSing s s' 
  | s ==  s'  = True
  | otherwise = False

bset :: (Set w m c, Adjunction f g) => g Entity -> f c -> SystemT w m ()
bset bd cd = zapWithAdjunction (\e c -> e `set` c) bd cd  

bget :: (Get w m c, Adjunction f g) => g Entity -> f c -> SystemT w m c
bget bd cd = zapWithAdjunction (\ent _ -> get ent) bd cd  

saveBoard :: Sequencer -> System World ()
saveBoard s = do
  ms <- (mapM.mapM) ( get) s
  liftIO $ writeMidi "ceph.mid" $ Euterpea.line $ fmap chord $ (fmap song <$> fToList ms)

fillB :: a -> SBoard a
fillB x = (\(SCoordF _ _ s) -> s) <$> unit x

fToList :: Foldable f => f a -> [a]
fToList = reverse . foldl (\b a -> a : b) []

netBoard :: Netitor -> MCoord -> System World SFXResources
netBoard sl m = do
  set global Pause
  get $ indexAdjunction sl m

playBoard :: Sequencer -> SCoord -> System World ()
playBoard bd s = do
  e <- return $ index bd s --extracts the entity that matches the coordinate in the board
  flip soundPlay 0 . fmap sound =<< mapM (get) e 

soundBoard :: Sequencer -> SCoord -> System World (SBoard (Bool,Position,[SpriteColor]))
soundBoard bd s = do
  (Beat m i,Camera viewPos _) <- get global
  
  --combine the entity picture with the indicator of the board's current coordinate
  picBoard <- return $ zipR ((tabulate $ bdSing s) , ( + Position (V2 50 50) ) <$> tabulate (defineSB))
  colorBoard <- mapM (\e -> return . fmap clr =<< mapM get e) bd
  --play the entity sound on beat
  when (i == 0) $ do playBoard bd s
  return $ (\((a,b),c) -> (a,b,c)) <$> zipR (picBoard, colorBoard)
 
