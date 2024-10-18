import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--The function takes RoadMap. Each tuple represents a road between two cities (c1, c2) and the distance (_).
--It uses list comprehension to extract both cities from each tuple and concatenates all of them into a single list.
--Finally, nub is applied to remove duplicates.
cities :: RoadMap -> [City]
cities rm = Data.List.nub $ concat [[c1, c2] | (c1, c2, _) <- rm]


--rm: roadmap (list of truples)
--c1, c2: The two cities you are checking for adjacency.
--any: This function returns True if any element in the list satisfies the condition given by the lambda function.
--The lambda function checks if c1 and c2 are connected in either order ((x == c1 && y == c2) or (x == c2 && y == c1)), so the adjacency is bidirectional.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent g c1 c2 = any (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) g


--It takes a RoadMap (rm) and two cities (c1, c2) as inputs.
--It filters the roadmap to find a tuple where the cities are connected (either c1 -> c2 or c2 -> c1).
--If no connection is found, it returns Nothing.
--If a connection is found, it returns the distance wrapped in a Just (because the result is of type Maybe Distance).
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = case filter (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm of
    []            -> Nothing
    ((_, _, d):_) -> Just d



--List Comprehensions:
    --The first part [(y, d) | (x, y, d) <- rm, x == c] collects all pairs where the city c is the first element of the tuple (x).
        --Here, y represents the neighboring city, and d is the distance to that city.
    --The second part [(x, d) | (x, y, d) <- rm, y == c] does the same but for the case where c is the second element of the tuple (y).
        --This ensures that you capture roads in both directions.
--Concatenation: The results of the two list comprehensions are combined using ++, which appends the two lists together.
--List Comprehension: [output | input <- source, condition]
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = [(y, d) | (x, y, d) <- rm, x == c] ++ [(x, d) | (x, y, d) <- rm, y == c]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

