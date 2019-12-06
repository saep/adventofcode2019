{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Day6 where

import PreludeAoC
import qualified RIO.Set as Set
import qualified RIO.Map as Map

data Object = CenterOfMass | Object Text | Santa | Me
  deriving (Eq, Ord, Show)

data OrbitRelation = Object `IsInOrbitAround` Object
  deriving (Eq, Ord, Show)


parseOrbitRelation :: Parser OrbitRelation
parseOrbitRelation = do
  a <- some $ anySingleBut ')'
  void $ char ')'
  b <- anySingle `someTill` (void (some newline) <|> eof)
  pure $ (fromString b) `IsInOrbitAround` (fromString a)

parseOrbitRelations :: Parser [OrbitRelation]
parseOrbitRelations = some parseOrbitRelation

type UniversalOrbitMap = Map Object (Set Object)

insert
  :: OrbitRelation
  -> UniversalOrbitMap
  -> UniversalOrbitMap
insert (a `IsInOrbitAround` b) = Map.insertWith mappend b (Set.singleton a)

instance IsString Object where
  fromString = \case
    "COM" -> CenterOfMass
    "SAN" -> Santa
    "YOU" -> Me
    obj   -> Object $ fromString obj

data Orbits = Orbits
  { indirect :: Int
  , direct   :: Int
  } deriving (Eq, Show)

instance Semigroup Orbits where
  o1 <> o2 = Orbits { indirect = indirect o1 + indirect o2
                    , direct = direct o1 + direct o2
                    }

sumOfRelations :: Orbits -> Int
sumOfRelations o = indirect o + direct o

instance Monoid Orbits where
  mempty = Orbits 0 0

createMap :: [OrbitRelation] -> UniversalOrbitMap
createMap =
  foldr
  (\(a `IsInOrbitAround` b) -> Map.insertWith mappend b (Set.singleton a))
  mempty

calculateOrbits :: [OrbitRelation] -> Orbits
calculateOrbits rs = go (-1) CenterOfMass
  where
    m = createMap rs
    go d o =
      let os = maybe mempty id (Map.lookup o m)
      in Orbits{indirect = max 0 d, direct = Set.size os}
      <> foldMap (go (d+1)) os

solution1 :: IO Int
solution1 =
  parseFile "input/Day6-1.txt" parseOrbitRelations
  <&> sumOfRelations . calculateOrbits

type Path = [Object]

shortestPath :: [OrbitRelation] -> Maybe ([Object], [Object])
shortestPath rs = case go [] CenterOfMass of
   [path1, path2] ->
     Just $ removeCommonPrefix (reverse path1) (reverse path2)
   _ ->
     Nothing
  where
    m = createMap rs
    go :: Path -> Object -> [Path]
    go path = \case
      Me ->
        pure path
      Santa ->
        pure path
      o -> do
        p <- maybe [] Set.toList $ Map.lookup o m
        go (o:path) p

removeCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
removeCommonPrefix (a:as) (b:bs)
  | a == b = removeCommonPrefix as bs
  | otherwise = (a:as, b:bs)
removeCommonPrefix as bs = (as, bs)

lengthOfShortestPath :: ([Object], [Object]) -> Int
lengthOfShortestPath (as, bs) = length as + length bs

solution2 :: IO Int
solution2 =
  parseFile "input/Day6-1.txt" parseOrbitRelations
  <&> maybe 0 lengthOfShortestPath . shortestPath

{-
--- Day 6: Universal Orbit Map ---

You've landed at the Universal Orbit Map facility on Mercury. Because navigation in space often involves transferring between orbits, the orbit maps here are useful for finding efficient routes between, for example, you and Santa. You download a map of the local orbits (your puzzle input).

Except for the universal Center of Mass (COM), every object in space is in orbit around exactly one other object. An orbit looks roughly like this:

                  \
                   \
                    |
                    |
AAA--> o            o <--BBB
                    |
                    |
                   /
                  /

In this diagram, the object BBB is in orbit around AAA. The path that BBB takes around AAA (drawn with lines) is only partly shown. In the map data, this orbital relationship is written AAA)BBB, which means "BBB is in orbit around AAA".

Before you use your map data to plot a course, you need to make sure it wasn't corrupted during the download. To verify maps, the Universal Orbit Map facility uses orbit count checksums - the total number of direct orbits (like the one shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain can be any number of objects long: if A orbits B, B orbits C, and C orbits D, then A indirectly orbits D.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L

Visually, the above map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I

In this visual representation, when two objects are connected by a line, the one on the right directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

    D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
    L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
    COM orbits nothing.

The total number of direct and indirect orbits in this example is 42.

What is the total number of direct and indirect orbits in your map data?

--- Part Two ---

Now, you just need to figure out how many orbital transfers you (YOU) need to take to get to Santa (SAN).

You start at the object YOU are orbiting; your destination is the object SAN is orbiting. An orbital transfer lets you move from any object to an object orbiting or orbited by that object.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN

Visually, the above map of orbits looks like this:

                          YOU
                         /
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN

In this example, YOU are in orbit around K, and SAN is in orbit around I. To move from K to I, a minimum of 4 orbital transfers are required:

    K to J
    J to E
    E to D
    D to I

Afterward, the map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN
                 \
                  YOU

What is the minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting? (Between the objects they are orbiting - not between YOU and SAN.)


-}
