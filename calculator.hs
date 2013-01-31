import Data.Map (Map, empty, fromList, unionWith, foldrWithKey, findWithDefault)
import qualified Data.Map as M
import Data.List (maximumBy)

data Block = Iron
           | Copper
           | Rubber
           | Tin
           | Circuit
           | AdvCircuit
           | Glowstone
           | Redstone
           | Lapis
           | RefIron
           | ReactorChamber
           | DensePlate
           | Machine
           | CopperWire
           | BatBox
           | Battery
           | Furnace
           | Generator
           | NuclearReactor
           | Cobblestone
           | CopperStack
           | IronStack
           | CobbleStack
           | TinStack
           deriving (Show, Eq, Ord, Enum)

type Basket = Map Block Float
type Tech = Map Block Basket

infixr 6 +++
(+++) :: Basket -> Basket -> Basket
b1 +++ b2 = unionWith (+) b1 b2

(***) :: Basket -> Float -> Basket
b1 *** x = M.map (*x) b1


------------------------------
--

(!) :: Float -> Block -> Basket
(!) f b = fromList [(b, f)]

techlist =
  [ (BatBox, 1!CopperWire +++ 3!Battery)
  , (CopperWire, 0.5!Copper +++ 1!Rubber)
  , (NuclearReactor, 3!ReactorChamber +++ 1!Generator +++ 1!AdvCircuit)
  , (AdvCircuit, 4!Redstone +++ 2!Glowstone +++ 2!Lapis +++ 1!Circuit)
  , (Circuit, 6!CopperWire +++ 2!Redstone +++ 1!RefIron)
  , (RefIron, 1!Iron)
  , (ReactorChamber, 4!DensePlate +++ 1!Machine)
  , (Machine, 8!RefIron)
  , (DensePlate, 8!Copper)
  , (Battery, 1!CopperWire +++ 2!Redstone +++ 4!Tin)
  , (Generator, 1!Machine +++ 1!Battery +++ 1!Furnace)
  , (Furnace, 8!Cobblestone)
  ]

stacktech =
  [ (Copper, (1/64.0)!CopperStack)
  , (Iron, (1/64.0)!IronStack)
  , (Cobblestone, (1/64.0)!CobbleStack)
  , (Tin, (1/64.0)!TinStack)
  ]

tech :: Tech
tech = fromList $ techlist ++ stacktech

techtrans :: Tech -> Block -> Float -> Basket
techtrans t b f = (findWithDefault (fromList [(b, 1.0)]) b t) *** f

reduce :: Tech -> Basket -> Basket
reduce t b
  | b == b' = b'
  | otherwise = reduce t b'
    where b' = reduce' t b

reduce' :: Tech -> Basket -> Basket
reduce' t b = foldrWithKey (reduce'' t) empty b

reduce'' :: Tech -> Block -> Float -> Basket -> Basket
reduce'' t b f a = (techtrans t b f) +++ a

main = putStrLn $ show $ r
  where r = reduce tech $ 1!NuclearReactor +++ 6!ReactorChamber