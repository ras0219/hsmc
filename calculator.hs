import Data.Map (Map, empty, fromList, unionWith, foldrWithKey, findWithDefault)
import qualified Data.Map as M
import Data.List (maximumBy, intersperse, sort, filter)
import System

-- Usage
--
-- Change the target recipe here:
-- targetRecipe = 5!OCHeatVent +++ 2!ComponentHeatVent +++ 2!ComponentHeatExchanger
--               +++ (-1)!DensePlate +++ (-1)!Circuit +++ (-4)!CopperWire
-- targetRecipe = 1!Recycler
--
--

parseArgsRecipe :: [String] -> Basket
parseArgsRecipe [] = empty
parseArgsRecipe (x:xs) = M.insert (read x) 1.0 $ parseArgsRecipe xs

main = do
  lines <- getArgs
  let targetRecipe = parseArgsRecipe lines
      r = reduce tech targetRecipe
      (i, p) = partitionBasket r
  putStrLn "Ingredients\n==========="
  putStrLn $ prettyPrint i
  putStrLn "\nProcess\n======="
  putStrLn $ prettyPrint p

-- Add new recipes here
craftlist =
    [ (BatBox, 1!CopperWire +++ 3!Battery)
    , (CopperWire, 0.5!Copper +++ 1!Rubber)
    , (GoldWire, (12.0/3.0)!Gold +++ 2!Rubber)
    , (IronWire, (12.0/3.0)!RefinedIron +++ 3!Rubber)
    , (NuclearReactor, 3!ReactorChamber +++ 1!Generator +++ 1!AdvCircuit)
    , (AdvCircuit, 4!Redstone +++ 2!Glowstone +++ 2!Lapis +++ 1!Circuit)
    , (Circuit, 6!CopperWire +++ 2!Redstone +++ 1!RefinedIron)
    , (RefinedIron, 1!Iron)
    , (ReactorChamber, 4!DensePlate +++ 1!Machine)
    , (Machine, 8!RefinedIron)
    , (Battery, 1!CopperWire +++ 2!Redstone +++ 4!Tin)
    , (Generator, 1!Machine +++ 1!Battery +++ 1!Furnace)
    , (Furnace, 8!Cobblestone)
    , (DensePlate, 8!Copper)
    , (OCHeatVent, 1!ReactorHeatVent +++ 2!Gold)
    , (ReactorHeatVent, 1!HeatVent +++ 2!DensePlate)
    , (ComponentHeatVent, 1!HeatVent +++ 4!Tin +++ 4!IronBar)
    , (HeatVent, 4!RefinedIron +++ 4!IronBar)
    , (ComponentHeatExchanger, 1!HeatExchanger +++ 4!Gold)
    , (HeatExchanger, 1!DensePlate +++ 1!Circuit +++ 3!Tin)
    , (IronBar, (6.0/16.0)!Iron)
    , (MassFabricator, 4!Glowstone +++ 2!AdvCircuit +++ 2!AdvMachine +++ 1!LapotronCrystal)
    , (AdvMachine, 2!AdvAlloy +++ 1!Machine +++ 2!CarbonPlate)
    , (LapotronCrystal, 2!Circuit +++ 6!Lapis +++ 1!EnergyCrystal)
    , (EnergyCrystal, 1!Diamond +++ 8!Redstone)
    , (AdvAlloy, 1!MixedIngot)
    , (CarbonPlate, 1!CarbonMesh)
    , (CarbonMesh, 2!CarbonFiber)
    , (CarbonFiber, 4!CoalDust)
    , (CoalDust, 1!Coal)
    , (MixedIngot, (3/2.0)!RefinedIron +++ (3/2.0)!Bronze +++ (3/2.0)!Tin)
    , (Bronze, (3/2.0)!Copper +++ (1/2.0)!Tin)
    , (Compressor, 1!Machine +++ 6!Cobblestone +++ 1!Circuit)
    , (Recycler, 1!Compressor +++ 1!Glowstone +++ 1!RefinedIron)
-- solar array stuff
    , (Glass, 1!Sand)
    , (LVTransformer, 3!Copper +++ 4!WoodPlank +++ 2!CopperWire)
    , (WoodPlank, (1.0/4.0)!Wood)
    , (MVTransformer, 1!Machine +++ 2!GoldWire)
    , (HVTransformer, 1!MVTransformer +++ 1!Circuit +++ 1!EnergyCrystal +++ 2!IronWire)
    , (EnergyCrystal, 1!Diamond +++ 8!Redstone)
    , (SolarPanel, 3!CoalDust +++ 3!Glass +++ 2!Circuit +++ 1!Generator)
    , (LVSolarArray, 8!SolarPanel +++ 1!LVTransformer)
    , (MVSolarArray, 8!LVSolarArray +++ 1!MVTransformer)
    , (HVSolarArray, 8!MVSolarArray +++ 1!HVTransformer)
    ]

-- Add new blocks here
data Block = Iron
           | Copper
           | Rubber
           | Tin
           | Cobblestone
           | Glowstone
           | Redstone
           | Gold
           | Diamond
           | Lapis
           | Coal
           | Sand
           | Wood
           -- Tier 1 crafts
           | WoodPlank
           | Glass
           | CopperWire
           | GoldWire
           | IronWire
           | RefinedIron
           | DensePlate
           | Machine
           | Furnace
           | IronBar
           | EnergyCrystal
           | CoalDust
           | Bronze
           -- Tier 2 crafts
           | MixedIngot
           | Circuit
           | BatBox
           | Battery
           | CarbonFiber
           | CarbonMesh
           -- Tier 3 crafts
           | CarbonPlate
           | AdvCircuit
           | AdvAlloy
           | Generator
           | LapotronCrystal
           | Compressor
           -- Tier 4+ crafts
           | HVTransformer
           | MVTransformer
           | LVTransformer
           | AdvMachine
           | Recycler
           | MassFabricator
           -- Nuclear crafts
           | ReactorChamber
           | NuclearReactor
           | OCHeatVent
           | ReactorHeatVent
           | ComponentHeatVent
           | HeatVent
           | ComponentHeatExchanger
           | HeatExchanger
           -- Meta ingredient to track process steps
           | Craft Block
           -- Stacks for calculating reduced costs
           | CopperStack
           | IronStack
           | CobbleStack
           | TinStack
           -- Solar Arrays
           | HVSolarArray
           | MVSolarArray
           | LVSolarArray
           | SolarPanel
           deriving (Show, Eq, Ord, Read)

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

addProcess :: [(Block, Basket)] -> [(Block, Basket)]
addProcess = map plusCraft
    where plusCraft (b, k) = (b, k +++ 1!(Craft b))

stacktech =
  [ (Copper, (1/64.0)!CopperStack)
  , (Iron, (1/64.0)!IronStack)
  , (Cobblestone, (1/64.0)!CobbleStack)
  , (Tin, (1/64.0)!TinStack)
  ]

tech :: Tech
tech = fromList $ addProcess craftlist

techtrans :: Tech -> Block -> Float -> Basket
techtrans t b f = (findWithDefault (fromList [(b, 1.0)]) b t) *** f

strip = M.filter (/= 0)

reduce :: Tech -> Basket -> Basket
reduce t b
  | b == b' = b'
  | otherwise = reduce t $ strip b'
    where b' = reduce' t b

reduce' :: Tech -> Basket -> Basket
reduce' t b = foldrWithKey (reduce'' t) empty b

reduce'' :: Tech -> Block -> Float -> Basket -> Basket
reduce'' t b f a = (techtrans t b f) +++ a


-- Filter baskets
isIngredient (Craft _) = False
isIngredient _ = True

partitionBasket = M.partitionWithKey pred
    where pred k _ = isIngredient k

-- Pretty printing
prettyPrint :: Basket -> String
prettyPrint b = concat $ intersperse "\n" $ sort $ map show (M.toList b)
