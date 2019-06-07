{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Ceph.Scene.Board where


import Apecs
import Ceph.Components
import Ceph.Jams
import Graphics.Gloss

import Euterpea hiding (Text)
import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Distributive
import Control.Monad
import qualified SDL.Mixer as M

instance (Show a) => Show (MBoard a) where
  show (MBoard a b c _  ) = "     | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | \n"
    ++ "Player A" ++ show a ++ "\n"
    ++ "Player B" ++ show b ++ "\n"
    ++ "Player C" ++ show c ++ "\n"

instance Distributive MBoard where
  distribute = distributeRep
  
instance Representable MBoard where
  -- We index into our functor using Coord
  type Rep MBoard = MCoord
  -- Given an index and a board, pull out the matching cell
  index (MBoard a _ _ _ ) (MCoordF m GuitarHarmonics _) = a
  index (MBoard _ a _ _ ) (MCoordF m BrassSection _) = a
  index (MBoard _ _ a _ ) (MCoordF m ElectricBassFingered _) = a
  index (MBoard _ _ _ a ) (MCoordF m ReedOrgan _) = a

  tabulate desc = let m = [(C,4)] in MBoard (desc (MCoordF m GuitarHarmonics ())) (desc (MCoordF m BrassSection ())) (desc (MCoordF m ElectricBassFingered ())) (desc (MCoordF m ReedOrgan ()))

instance Adjunction MCoordF MBoard where
  unit a = tabulate (\(MCoordF row col _ ) -> MCoordF row col a)
  counit (MCoordF row col board) = index board (MCoordF row col ())

defineIBoard :: ICoord -> Music Pitch
defineIBoard (ICoordF p m _) = Prim . Note m $ p

instance Distributive IBoard where
  distribute = distributeRep

instance Representable IBoard where
  type Rep IBoard = ICoord
  index (IBoard a _ _ _ _ _ _ _ _ _ _ _) (ICoordF (A,_) _ _) = a
  index (IBoard _ a _ _ _ _ _ _ _ _ _ _) (ICoordF (As,_) _ _) = a
  index (IBoard _ _ a _ _ _ _ _ _ _ _ _) (ICoordF (B,_) _ _) = a
  index (IBoard _ _ _ a _ _ _ _ _ _ _ _) (ICoordF (Bs,_) _ _) = a
  index (IBoard _ _ _ _ a _ _ _ _ _ _ _) (ICoordF (C,_) _ _) = a
  index (IBoard _ _ _ _ _ a _ _ _ _ _ _) (ICoordF (Cs,_) _ _) = a
  index (IBoard _ _ _ _ _ _ a _ _ _ _ _) (ICoordF (D,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ a _ _ _ _) (ICoordF (Ds,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ a _ _ _) (ICoordF (E,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ a _ _) (ICoordF (Es,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ _ a _) (ICoordF (F,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ _ _ a) (ICoordF (G,_) _ _) = a
  tabulate desc = IBoard (desc (ICoordF (A,4) qn ())) (desc (ICoordF (As,4) qn ())) 
    (desc (ICoordF (B,4) qn ())) 
    (desc (ICoordF (Bs,4) qn ())) 
    (desc (ICoordF (C,4) qn ())) 
    (desc (ICoordF (Cs,4) qn ())) 
    (desc (ICoordF (D,4) qn ())) 
    (desc (ICoordF (Ds,4) qn ())) 
    (desc (ICoordF (E,4) qn ())) 
    (desc (ICoordF (Es,4) qn ())) 
    (desc (ICoordF (F,4) qn ())) 
    (desc (ICoordF (G,4) qn ()))
  
instance Adjunction ICoordF IBoard where
  unit a = tabulate (\(ICoordF row col _ ) -> ICoordF row col a)
  counit (ICoordF row col board) = index board (ICoordF row col ())

--i think music is too ethereal to be represented!
instance Distributive Music where
  distribute = distributeRep

--you could probably do it with the right functors...
instance Representable Music where
  type Rep Music = ICoord
  index (Prim (Note _ a )) (ICoordF _ _ _) = a
  index (a :+: _) i@(ICoordF _ _ _) = index a i
  index (a :=: _) i@(ICoordF _ _ _) = index a i
  index (Modify _ a) i@(ICoordF _ _ _) = index a i
  

instance (Show a) => Show (SBoard a) where
  show (SBoard a b c d) = "       I  |  II | III | IV\n"
    ++ "A   " ++ show a ++ "\n"
    ++ "B   " ++ show b ++ "\n"
    ++ "C   " ++ show c ++ "\n"
    ++ "D   " ++ show d ++ "\n"
    
instance Distributive SBoard where
  distribute = distributeRep
  
instance Representable SBoard where
  -- We index into our functor using Coord
  type Rep SBoard = SCoord
  -- Given an index and a board, pull out the matching cell
  index (SBoard (a, _, _, _) _ _ _) (SCoordF S1 SI _) = a
  index (SBoard (_, a, _, _) _ _ _) (SCoordF S1 SII _) = a
  index (SBoard (_, _, a, _) _ _ _) (SCoordF S1 SIII _) = a
  index (SBoard (_, _, _, a) _ _ _) (SCoordF S1 SIV _) = a
  index (SBoard _ (a, _, _, _) _ _) (SCoordF S2 SI _) = a
  index (SBoard _ (_, a, _, _) _ _) (SCoordF S2 SII _) = a
  index (SBoard _ (_, _, a, _) _ _) (SCoordF S2 SIII _) = a
  index (SBoard _ (_, _, _, a) _ _) (SCoordF S2 SIV _) = a
  index (SBoard _ _ (a, _, _, _) _) (SCoordF S3 SI _) = a
  index (SBoard _ _ (_, a, _, _) _) (SCoordF S3 SII _) = a
  index (SBoard _ _ (_, _, a, _) _) (SCoordF S3 SIII _) = a
  index (SBoard _ _ (_, _, _, a) _) (SCoordF S3 SIV _) = a
  index (SBoard _ _ _ (a, _, _, _)) (SCoordF S4 SI _) = a
  index (SBoard _ _ _ (_, a, _, _)) (SCoordF S4 SII _) = a
  index (SBoard _ _ _ (_, _, a, _)) (SCoordF S4 SIII _) = a
  index (SBoard _ _ _ (_, _, _, a)) (SCoordF S4 SIV _) = a

  -- Given a function which describes a slot, build a Board
  tabulate desc = SBoard
      (desc (SCoordF S1 SI ()), desc (SCoordF S1 SII ()), desc (SCoordF S1 SIII ()), desc (SCoordF S1 SIV ()))
      (desc (SCoordF S2 SI ()), desc (SCoordF S2 SII ()), desc (SCoordF S2 SIII ()), desc (SCoordF S2 SIV ()))
      (desc (SCoordF S3 SI ()), desc (SCoordF S3 SII ()), desc (SCoordF S3 SIII ()), desc (SCoordF S3 SIV ()))
      (desc (SCoordF S4 SI ()), desc (SCoordF S4 SII ()), desc (SCoordF S4 SIII ()), desc (SCoordF S4 SIV ()))

instance Adjunction SCoordF SBoard where
  --unit :: a -> SBoard (SCoordF a)
  unit a = tabulate (\(SCoordF row col _ ) -> SCoordF row col a)
  counit (SCoordF row col board) = index board (SCoordF row col ())

defineSB :: SCoord -> SCoord -> (Picture)
defineSB  s s'@(SCoordF S1 SI _) = Translate (-30) 30 $ circ s s'
defineSB  s s'@(SCoordF S1 SII _) = Translate (-10) 30 $ circ s s'
defineSB  s s'@(SCoordF S1 SIII _) = Translate (10) 30 $ circ s s'
defineSB  s s'@(SCoordF S1 SIV _) = Translate (30) 30 $ circ s s'
defineSB  s s'@(SCoordF S2 SI _) = Translate (-30) 10 $ circ s s'
defineSB  s s'@(SCoordF S2 SII _) = Translate (-10) 10 $ circ s s'
defineSB  s s'@(SCoordF S2 SIII _) = Translate (10) 10 $ circ s s'
defineSB  s s'@(SCoordF S2 SIV _) = Translate (30) 10 $ circ s s'
defineSB  s s'@(SCoordF S3 SI _) = Translate (-30) (-10) $ circ s s'
defineSB  s s'@(SCoordF S3 SII _) = Translate (-10) (-10) $ circ s s'
defineSB  s s'@(SCoordF S3 SIII _) = Translate (10) (-10) $ circ s s'
defineSB  s s'@(SCoordF S3 SIV _) = Translate (30) (-10) $ circ s s'
defineSB  s s'@(SCoordF S4 SI _) = Translate (-30) (-30) $ circ s s'
defineSB  s s'@(SCoordF S4 SII _) = Translate (-10) (-30) $ circ s s'
defineSB  s s'@(SCoordF S4 SIII _) = Translate (10) (-30) $ circ s s'
defineSB  s s'@(SCoordF S4 SIV _) = Translate (30) (-30) $ circ s s' 

circ :: Eq a => a -> a -> Picture
circ s s' 
  | s == s'   = (Color red $ Circle 5)
  | otherwise = mempty

updateBoard ::
  (Eq (f ()), Adjunction f g) => g b -> f () -> b -> g b
updateBoard b s a = leftAdjunct (updateB b s a) ()
  where updateB board ixToChange update anyIx = if ixToChange == anyIx then update else rightAdjunct (const board) anyIx

        
bdSing :: SCoord -> SCoord -> Bool
bdSing s s' 
  | s ==  s'  = True
  | otherwise = False

bset :: (Set w m c) => SBoard Entity -> SCoordF c -> SystemT w m ()
bset bd cd = zapWithAdjunction (\e c -> e `set` c) bd cd  

bget :: (Get w m c) => SBoard Entity -> SCoordF c -> SystemT w m c
bget bd cd = zapWithAdjunction (\e _ -> get e) bd cd  

soundBoard :: Sequencer -> SCoord -> System World Picture
soundBoard bd s = do
  sbp <- mapM (getPic) bd
  
  picBoard <- return $ (\(b,Translate x y a) -> Translate x y $ Pictures [a,b]) <$> zipR (sbp, tabulate (defineSB s))
  e <- return $ rightAdjunct (const bd) s
  Beat m i <- get global
  when (m == i) $ do
    (\d -> (Entity 1) `set` (In, Debug $ show d)) =<< bget bd (fmap (const $ Song $ rest qn) s)
    playSong e

  return . Pictures . toList $ picBoard
  where toList = foldl (\b a -> a : b) []
        getPic e = do
          (BodyPicture p) <- get e
          return (Scale 0.2 0.2 p)
