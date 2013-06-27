import Data.Map (Map, empty, fromList, unionWith, foldrWithKey, findWithDefault)
import qualified Data.Map as M
import Data.List (maximumBy, intersperse, sort, filter)
import System.Environment (getArgs)
import Text.Printf

parseArgsRecipe :: [String] -> Basket
parseArgsRecipe [] = empty
parseArgsRecipe (n:x:xs) = M.insert item amt' basket
  where item = Block x
        basket = parseArgsRecipe xs
        amt = read n
        amt' = case M.lookup item basket
               of Nothing -> amt
                  Just n -> n + amt

main = do
  lines <- getArgs
  if length lines == 0
    then fail "Called calculator with no arguments"
    else return ()
  let targetRecipe = parseArgsRecipe lines
      r = reduce tech targetRecipe
      (i, p) = partitionBasket r
  putStrLn "Ingredients\n==========="
  putStrLn $ prettyPrint i
  putStrLn "\nProcess\n======="
  putStrLn $ prettyPrint p

-- Add new recipes here
craftlist =
    [
      -- Vanilla
      ("Furnace", [(8, "Cobblestone")])
    , ("IronBar", [(6.0/16.0, "Iron")])
    , ("WoodPlank", [(1.0/4.0, "Wood")])
    , ("Glass", [(1, "Sand")])
      -- IC2 Low Level
    , ("RefinedIron", [(1, "Iron")])
    , ("Machine", [(8, "RefinedIron")])
    , ("Battery", [(1, "CopperWire"),
                   (2, "Redstone"),
                   (4, "Tin")])
    , ("Generator", [(1, "Machine"),
                     (1, "Battery"),
                     (1, "Furnace")])
    , ("BatBox", [(1, "CopperWire"),
                  (3, "Battery")])
    , ("CopperWire", [(3.0/6.0, "Copper"),
                      (6.0/6.0, "Rubber")])
    , ("IronWire", [(3.0/12.0, "RefinedIron"),
                    (36.0/12.0, "Rubber")])
    , ("GoldWire", [(1.0/4.0, "Gold"),
                    (8.0/4.0, "Rubber")])
    , ("GlassFibre", [(6.0/6.0, "Glass"),
                      (2.0/6.0, "Silver"),
                      (1.0/6.0, "Diamond")])
    , ("Circuit", [(6, "CopperWire"),
                   (2, "Redstone"),
                   (1, "RefinedIron")])
    , ("AdvCircuit", [(4, "Redstone"),
                      (2, "Glowstone"),
                      (2, "Lapis"),
                      (1, "Circuit")])
    , ("CoalDust", [(1, "Coal")])
    , ("Bronze", [(3/2.0, "Copper"),
                  (1/2.0, "Tin")])
    , ("10kCoolant", [(4, "Tin"),
                      (1, "Cell")])
    , ("Overclocker", [(3, "10kCoolant"),
                       (2, "CopperWire"),
                       (1, "AdvCircuit")])
      -- Nuclear subcomponents
    , ("MixedIngot", [(3/2.0, "RefinedIron"),
                      (3/2.0, "Bronze"),
                      (3/2.0, "Tin")])
    , ("AdvAlloy", [(1, "MixedIngot")])
    , ("CarbonPlate", [(1, "CarbonMesh")])
    , ("CarbonMesh", [(2, "CarbonFiber")])
    , ("CarbonFiber", [(4, "CoalDust")])
    , ("DensePlate", [(8, "Copper")])
    , ("AdvMachine", [(2, "AdvAlloy"),
                      (1, "Machine"),
                      (2, "CarbonPlate")])
    , ("LapotronCrystal", [(2, "Circuit"),
                           (6, "Lapis"),
                           (1, "Sapphire")])
    , ("EnergyCrystal", [(1, "Ruby"),
                         (8, "Redstone")])
      -- Nuclear components
    , ("NuclearReactor", [(3, "ReactorChamber"),
                          (1, "Generator"),
                          (1, "AdvCircuit")])
    , ("ReactorChamber", [(4, "DensePlate"),
                          (1, "Machine")])
    , ("OCHeatVent", [(1, "ReactorHeatVent"),
                      (2, "Gold")])
    , ("AdvHeatVent", [(6, "IronBar"),
                       (1, "Diamond"),
                       (2, "HeatVent")])
    , ("ReactorHeatVent", [(1, "HeatVent"),
                           (2, "DensePlate")])
    , ("ComponentHeatVent", [(1, "HeatVent"),
                             (4, "Tin"),
                             (4, "IronBar")])
    , ("HeatVent", [(4, "RefinedIron"),
                    (4, "IronBar")])
    , ("ComponentHeatExchanger", [(1, "HeatExchanger"),
                                  (4, "Gold")])
    , ("HeatExchanger", [(1, "DensePlate"),
                         (1, "Circuit"),
                         (3, "Tin")])
      -- High level machines
    , ("Compressor", [(1, "Machine"),
                      (6, "Cobblestone"),
                      (1, "Circuit")])
    , ("Recycler", [(1, "Compressor"),
                    (1, "Glowstone"),
                    (1, "RefinedIron")])
      -- solar array stuff
    , ("LVTransformer", [(3, "Copper"),
                         (4, "WoodPlank"),
                         (2, "CopperWire")])
    , ("MVTransformer", [(1, "Machine"),
                         (2, "GoldWire")])
    , ("HVTransformer", [(1, "MVTransformer"),
                         (1, "Circuit"),
                         (1, "EnergyCrystal"),
                         (2, "IronWire")])
    , ("SiliconPlate", [(2, "Silicon")])
    , ("SolarPanel", [(1, "CarbonPlate"),
                      (3, "GlassPane"),
                      (2, "Circuit"),
                      (1, "Generator"),
                      (2, "SiliconPlate")])
      -- Graviplate Shenans
    , ("BoosterEngine", [(2, "Glowstone"),
                         (3, "AdvAlloy"),
                         (2, "AdvCircuit"),
                         (1, "Overclocker"),
                         (1, "AdvHeatVent")])
    , ("Jetpack", [(2, "Glowstone"),
                   (1, "BatBox"),
                   (4, "RefinedIron"),
                   (1, "AdvCircuit")])
    , ("AdvJetpack", [(2, "BoosterEngine"),
                      (1, "Jetpack"),
                      (2, "CarbonPlate"),
                      (2, "GlassFibre"),
                      (1, "AdvLappack"),
                      (1, "AdvCircuit")])
    , ("AdvLappack", [(1, "Lappack"),
                      (1, "LapotronCrystal"),
                      (1, "AdvCircuit")])
    , ("Lappack", [(1, "Batpack"),
                   (54, "Lapis"),
                   (1, "AdvCircuit")])
    , ("Batpack", [(6, "Battery"),
                   (1, "Circuit"),
                   (1, "Tin")])
      -- Applied Energistics
    , ("CraftingCPU", [(2, "QuartzCrystal"),
                       (4, "Iron"),
                       (2, "Glowstone"),
                       (1, "AdvancedProcessor")])
    , ("AdvancedProcessor", [(1, "Diamond"),
                             (2, "Redstone"),
                             (1, "QuartzDust")])
    , ("BasicProcessor", [(1, "Gold"),
                          (2, "Redstone"),
                          (1, "QuartzDust")])
    , ("PatternProvider", [(2, "ConversionMatrix"),
                           (1, "AdvancedProcessor"),
                           (4, "Iron"),
                           (1, "StorageCell"),
                           (1, "CraftingTable")])
    , ("AssemblerWall", [(1, "QuartzCrystal"),
                         (4, "Gold"),
                         (4, "Iron")])
    , ("MEHeatVent", [(1, "MECable"),
                      (4, "Iron"),
                      (4, "IronBar")])
    , ("MECable", [(2, "Redstone"),
                   (1, "QuartzFiber")])
    , ("QuartzFiber", [(3/4.0, "QuartzDust"),
                       (6/4.0, "Glass")])
    , ("StorageCell", [(4, "QuartzCrystal"),
                       (4, "Redstone"),
                       (1, "BasicProcessor")])
    , ("ConversionMatrix", [(2, "QuartzCrystal"),
                            (1, "BasicProcessor"),
                            (4, "Iron"),
                            (2, "Redstone")])
      -- blue powerrrrrr
    , ("BlueAlloy", [(4, "Nikolite"),
                     (1, "Silver")])
    , ("BTBattery", [(6, "Nikolite"),
                     (1, "Tin"),
                     (2, "Copper")])
    , ("BatteryBox", [(4, "BTBattery"),
                      (3, "Iron"),
                      (1, "BlueAlloy"),
                      (1, "WoodPlank")])
    ]

-- Add new blocks here
data Block = Block String
           | Craft Block
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

addProcessSimple :: [(String, [(Float, String)])] -> [(Block, Basket)]
addProcessSimple l = addProcess $ map trans l
  where trans (n, b) = (Block n, fromList $ map f b)
        f (i, s) = (Block s, i)

addProcess :: [(Block, Basket)] -> [(Block, Basket)]
addProcess = map plusCraft
    where plusCraft (b, k) = (b, k +++ 1!(Craft b))

stacktech =
  [ (Block "Copper", (1/64.0)!Block "CopperStack")
  , (Block "Iron", (1/64.0)!Block "IronStack")
  , (Block "Cobblestone", (1/64.0)!Block "CobbleStack")
  , (Block "Tin", (1/64.0)!Block "TinStack")
  ]

tech :: Tech
tech = fromList $ addProcessSimple craftlist

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
prettyPrint b = concat $ intersperse "\n" $ map showItem (M.toList b)
  where showItem (item, amt) = printf "%6.1f %s" amt (show item)
