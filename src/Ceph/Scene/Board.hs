{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Ceph.Scene.Board where


import Apecs
import Ceph.Components
import Ceph.Jams
import qualified SDL as S
import qualified Data.Vector as V
import Euterpea hiding (Text)
import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Distributive
import Control.Monad

defineSB :: SCoord -> (Position)
defineSB  s'@(SCoordF S4 SI _) = Position $ S.V2 (-30) 30 
defineSB  s'@(SCoordF S4 SII _) = Position $ S.V2 (-10) 30 
defineSB  s'@(SCoordF S4 SIII _) = Position $ S.V2 (10) 30 
defineSB  s'@(SCoordF S4 SIV _) = Position $ S.V2 (30) 30 
defineSB  s'@(SCoordF S3 SI _) = Position $ S.V2 (-30) 10 
defineSB  s'@(SCoordF S3 SII _) = Position $ S.V2 (-10) 10 
defineSB  s'@(SCoordF S3 SIII _) = Position $ S.V2 (10) 10 
defineSB  s'@(SCoordF S3 SIV _) = Position $ S.V2 (30) 10 
defineSB  s'@(SCoordF S2 SI _) = Position $ S.V2 (-30) (-10) 
defineSB  s'@(SCoordF S2 SII _) = Position $ S.V2 (-10) (-10) 
defineSB  s'@(SCoordF S2 SIII _) = Position $ S.V2 (10) (-10) 
defineSB  s'@(SCoordF S2 SIV _) = Position $ S.V2 (30) (-10) 
defineSB  s'@(SCoordF S1 SI _) = Position $ S.V2 (-30) (-30) 
defineSB  s'@(SCoordF S1 SII _) = Position $ S.V2 (-10) (-30) 
defineSB  s'@(SCoordF S1 SIII _) = Position $ S.V2 (10) (-30) 
defineSB  s'@(SCoordF S1 SIV _) = Position $ S.V2 (30) (-30)  
  
listToBoard :: [Entity] -> Sequencer
listToBoard es = tabulate (\(s :: SCoord) -> es !! fromEnum s)

updateBoard ::
  (Eq (f ()), Adjunction f g) => g b -> f () -> b -> g b
updateBoard b s a = leftAdjunct (updateB b s a) ()
  where updateB board ixToChange update anyIx = if ixToChange == anyIx then update else rightAdjunct (const board) anyIx

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
  ms <- mapM get s
  liftIO (writeMidi "ceph.mid" $ Euterpea.line . (fmap (\(SFXResources _ s _) -> s)) $ toList ms)

indAdj :: Adjunction f g => g a -> f b -> a
indAdj b s = rightAdjunct (const b) s

fillB :: a -> SBoard a
fillB x = (\(SCoordF _ _ s) -> s) <$> unit x

toList :: Foldable f => f a -> [a]
toList = foldl (\b a -> a : b) []

netBoard :: Netitor -> MCoord -> System World (SFXResources)
netBoard sl m = do
  set global Pause
  return $ indAdj sl m
  
soundBoard :: Sequencer -> SCoord -> System World (SBoard (Bool,Position,SpriteColor))
soundBoard bd s = do
  (Camera viewPos _, Beat m i) <- get global

  --combine the entity picture with the indicator of the board's current coordinate
  picBoard <- return $ zipR ((tabulate $ bdSing s) , ( + Position (S.V2 50 50)) <$> tabulate (defineSB))
  colorBoard <- mapM (\e -> clr <$> get e :: System World SpriteColor) bd
  e <- return $ indAdj bd ( succCycle s ) --extracts the entity that matches the coordinate in the board
  --play the entity sound on beat
  when (m == i) $ do
    playSong e

  return $ (\((a,b),c) -> (a,b,c)) <$> zipR (picBoard, colorBoard)
 
