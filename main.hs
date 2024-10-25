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

--It takes a RoadMap (rm) and a Path as input.
--If the path is empty or has only one city, the distance is 0.
--Otherwise, it calculates the distance between the first two cities in the path using the distance function.
--If the distance is Nothing, the function returns Nothing.
--If the distance is valid, it recursively calls itself with the rest of the path and adds the distance to the result.
--Returns the result wrapped in a Just (because the result is of type Maybe Distance).
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0 --If the path is empty, the distance is 0.
pathDistance _ [_] = Just 0 --If the path has only one city, the distance is 0.
pathDistance rm (c1:c2:cs) = 
    case distance rm c1 c2 of
        Nothing -> Nothing
        Just d  -> case pathDistance rm (c2:cs) of
            Nothing -> Nothing
            Just ds -> Just (d + ds)


--It takes a RoadMap (rm) as input.
--It uses a list comprehension and the previously implemented cities function to iterate over all cities in the roadmap.
--For each city, it calculates the number of adjacent cities using the adjacent function implemented before.
--The result is a list of tuples where the first element is the city, and the second element is the number of adjacent cities.
--The function returns the first element of the tuples with the maximum number of adjacent cities.
rome :: RoadMap -> [City]
rome rm = 
    let cityRoadCounts = [(city, length $ adjacent rm city) | city <- cities rm]
    in map fst $ filter (\(_, count) -> count == maximum (map snd cityRoadCounts)) cityRoadCounts


-- Helper function for DFS to collect reachable cities from a starting city
dfs :: RoadMap -> City -> [City]
dfs rm start = go [start] [] --This initializes the helper function go, passing it a list with the starting city [start] and an empty list [] to keep track of visited cities.
  where
    go [] visited = visited --It takes two lists:
                            --The first list contains cities yet to visit (the "to visit" stack).
                            --The second list contains cities that have already been visited.
    
    go (c:cs) visited --This pattern matches the first city c from the "to visit" list and the rest of the cities cs.
      | c `elem` visited = go cs visited --If the current city c has already been visited it skips this city
      | otherwise = go (adjacentCities ++ cs) (c : visited) --otherwise add c to the visited (c:visited) and collect its adjacent cities and prepend them to the list of cities to visit
      where
        adjacentCities = [y | (x, y, _) <- rm, x == c] ++ [x | (x, y, _) <- rm, y == c] --This list comprehension gathers all cities directly connected to c (both directions)




isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = all (\city -> length (dfs rm city) == length (cities rm)) (cities rm)

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

