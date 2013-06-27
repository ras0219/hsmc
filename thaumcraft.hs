import qualified Data.Set as S
import qualified Data.Map as M

type Aspects = M.Map String Int
type MeltRecipe = (String, Aspects)

add :: Aspects -> Aspects -> Aspects
add = M.unionWith (+)

mult :: Int -> Aspects -> Aspects
mult n = M.map (*n)

meltmap = M.fromList (map (\(o,l) -> (o, M.fromList l)) meltmap')
meltmap' =
  [
    ("Cobblestone", [("Fractis", 1), ("Saxum", 1)]),
    ("Furnace", [("Fractis", 6), ("Saxum", 6)]),
    ("Red Xychorium", [("Fractis", 6), ("Vitreus", 2)]),

    ("Brick", [("Ignis", 1), ("Solum", 2)]),
    ("Blaze Rod", [("Ignis", 6), ("Praecantatio", 2)]),
    ("Charcoal", [("Ignis", 2), ("Lignum", 2), ("Potentia", 2)]),
    ("Netherrack", [("Ignis", 1), ("Saxum", 1), ("Solum", 1)]),
    ("Sulfur", [("Ignis", 1), ("Victis", 2)]),
    ("Gold Ingot", [("Carus", 4), ("Metallum", 8)]),
    ("Energy Drop", [("Permutatio", 2), ("Victis", 1)]),
    ("Silver Ingot", [("Permutatio", 2), ("Metallum", 6)]),
    
    -- Pure sources
    ("Awkward Potion", [("Aqua", 1)]),
    ("Brown Mushroom", [("Fungus", 4)]),
    ("Red Mushroom", [("Fungus", 4)]),
    ("Snow", [("Gelum", 3)]),
    ("Snowball", [("Gelum", 3)]),
    ("Leaves", [("Herba", 2)]),
    ("Wood", [("Lignum", 8)]),
    ("Stick", [("Lignum", 1)]),
    ("Planks", [("Lignum", 2)]),
    ("Pumpkin", [("Messis", 8)]),
    ("Gold Nugget", [("Metallum", 1)]),
    ("Iron Ingot", [("Metallum", 8)]),
    ("Light Grey Dye", [("Permutatio", 1)]),
    ("Stone", [("Saxum", 2)]),
    ("Dirt", [("Solum", 2)]),
    ("Sand", [("Solum", 2)]),
    ("Arrow", [("Tellum", 1)]),
    ("Melon Slice", [("Victis", 1)]),
    ("Glass", [("Vitreus", 2)]),
    ("Glass Pane", [("Vitreus", 1)]),
    -- There is a possibility of getting corpus by standing on a crucible?
    ("Player", [("Corpus", 1)])
  ]

createmap = M.fromList (map (\(o,l) -> (o, M.fromList l))

allAspects :: S.Set String
allAspects = foldl f S.empty meltmap'
  where f m (_, la) = S.union m (S.fromList $ map fst la)

type Crucible = Aspects
data Process a = P { runProcess :: Crucible -> Either String (a, Crucible) }

instance Monad Process where
  return x = P $ \c -> Right (x, c)
  m >>= f = P $ \c -> case runProcess m c of Right (v, c) -> runProcess (f v) c
                                             Left err -> Left err



main = do print allAspects
          print $ S.size allAspects