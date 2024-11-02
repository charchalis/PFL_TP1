import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

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
        adjacentCities = ([y | (x, y, _) <- rm, x == c] ++ [x | (x, y, _) <- rm, y == c]) --This list comprehension gathers all cities directly connected to c (both directions)
        --adjacentCities = [adjCity | (adjCity, _) <- adjacent rm c]  --less efficient


--all Function:
    --all is a higher-order function that takes a predicate (a function returning True or False) and a list. It checks if the predicate is True for every item in the list.
    --In isStronglyConnected, all applies a lambda function to every city in the list cities rm.
--Lambda Function:
    --(\city -> length (dfs rm city) == length (cities rm))
        --This lambda function checks if, starting from a particular city, dfs can reach all other cities.
    --Breakdown:
        --dfs rm city: Performs a depth-first search from the given city, returning a list of all cities reachable from that city.
        --length (dfs rm city): Counts the number of cities reachable from the starting city.
        --length (cities rm): Counts the total number of cities in the roadmap.
        --length (dfs rm city) == length (cities rm): This comparison checks if the number of reachable cities from city matches the total number of cities. If it does, then starting from this city, we can reach every other city.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = all (\city -> length (dfs rm city) == length (cities rm)) (cities rm)


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end
    | start == end = [[start]]  -- If start and end are the same, return the city as the only path
    | otherwise = dijkstra [(start, [start], 0)] [] []  -- Initialize the priority queue with the start city
                                                        --Start with a priority queue containing a tuple with:
                                                            --start: the starting city,
                                                            --[start]: the path taken so far (initially just the start city),
                                                            --0: the total distance traveled so far (initially zero).
                                                        --The second argument is an empty list for visited cities, and the third is an empty list for shortest paths.
  where
    -- Dijkstra's function
    dijkstra :: [(City, Path, Distance)] -> [City] -> [(Path, Distance)] -> [Path]  --Dijkstra's Function Definition: Defines a helper function dijkstra which takes:
                                                                                        --A priority queue of tuples (current city, path, distance).
                                                                                        --A list of visited cities.
                                                                                        --A list of tuples (path, distance) for the shortest paths found so far.
    dijkstra [] _ shortestPaths = map fst shortestPaths  -- If the queue is empty, return only the paths from the shortest paths
    dijkstra ((currentCity, path, dist):queue) visited shortestPaths  -- the tuple is the first element of the queue
        | currentCity == end =  -- If we reached the destination city
            let newShortestPaths = if null shortestPaths 
                                    then [(reverse path, dist)]  -- Initialize if it's the first found path
                                    else let (p, d) = head shortestPaths in  --If there are already paths found, extract the first one (path and distance).
                                             if dist < d then [(reverse path, dist)]  -- New shorter path, update newShortestPaths to only contain the current path.
                                             else if dist == d then (reverse path, dist) : shortestPaths  -- Same length path, add the current path to the list of shortest paths.
                                             else shortestPaths  -- Ignore longer paths
            in dijkstra queue visited newShortestPaths  --Recursive Call: Continue the Dijkstra process using the remaining queue and updated shortest paths.
        | otherwise = --(currentCity != end)
            let newVisited = currentCity : visited  -- Mark the current city as visited
                adjCities = [(neighbor, d) | (neighbor, d) <- adjacent rm currentCity, neighbor `notElem` visited] --Get all adjacent cities and distances for the current city that haven't been visited yet.
                newPaths = [(neighbor, neighbor : path, dist + d) | (neighbor, d) <- adjCities]  --New Paths: Create new paths for each unvisited neighbor by:
                                                                                                    --Taking the neighbor city,
                                                                                                    --Constructing the new path (adding the neighbor to the current path),
                                                                                                    --Updating the distance by adding the edge weight to the current distance.
                -- Combine new paths with the existing queue and sort by distance
                sortedQueue = Data.List.sortOn (\(_, _, d) -> d) (queue ++ newPaths)  --Sort Queue: Combine the existing queue with the new paths and sort them based on the distance, ensuring that the next city to explore is always the one with the least cumulative distance.
            in dijkstra sortedQueue newVisited shortestPaths --Recursive Call: Call dijkstra again with the updated queue, visited cities, and shortest paths.







type TspCoord = (City, [City]) -- TspCoord represents a city and a list of the remaining cities to visit
type TspEntry = (Maybe Int, [City]) -- TspEntry represents the total distance of the path and the path itself

compTsp :: RoadMap -> City -> [(TspCoord, TspEntry)] -> TspCoord -> TspEntry -- Compute the TSP solution for a given TspCoord
compTsp g end a (i, k)
    | null k = case distance g i end of -- If there are no more cities to visit
        Just w -> (Just w, [i, end]) -- If there is a direct path to the end city (the last city to visit and the one we started from), returns the distance and the path
        Nothing -> (Nothing, []) -- If no path exists, return Nothing and an empty path
    | otherwise = Data.List.minimumBy compareTspEntry [addFst (lookupTsp (j, delList j k) a) (distance g i j) | j <- k] -- Otherwise, find the minimum path by comparing all possible next cities
  where
    addFst (c, p) (Just w) = (fmap (+ w) c, i : p) -- Add the distance to the current total and prepend the current city to the path
    addFst _ Nothing = (Nothing, []) -- If no path exists, return Nothing and an empty path
    compareTspEntry (Nothing, _) (Nothing, _) = EQ -- If both entries are Nothing, they are equal
    compareTspEntry (Nothing, _) _ = GT -- If the first entry is Nothing, the second is greater
    compareTspEntry _ (Nothing, _) = LT -- If the second entry is Nothing, the first is greater
    compareTspEntry (Just c1, _) (Just c2, _) = compare c1 c2 -- Compare the total distances of the two entries

lookupTsp :: TspCoord -> [(TspCoord, TspEntry)] -> TspEntry -- Look up the TSP solution for a given TspCoord like a map
lookupTsp coord = snd . head . filter ((== coord) . fst) -- Find the TspEntry for a given TspCoord

delList :: Eq a => a -> [a] -> [a] -- Delete an element from a list
delList x = filter (/= x)

bndsTsp :: [City] -> ((City, [City]), (City, [City])) -- Define the bounds for the TSP problem, first and last cities
bndsTsp cities = ((head cities, []), (last cities, init cities))

tsp :: RoadMap -> (Maybe Int, [City]) -- Compute the TSP solution for a given RoadMap
tsp g = lookupTsp (end, init citiesList) a -- Look up the TSP solution for the end city and all other cities to visit
  where
    citiesList = cities g -- Get the list of cities from the roadmap
    end = last citiesList -- Define the end city as the last city in the list
    a = [(coord, compTsp g end a coord) | coord <- [(c, s) | c <- citiesList, s <- subsets (init citiesList)]] -- Compute the TSP solution for all possible coordinates

subsets :: [a] -> [[a]] -- Generate all subsets of a list
subsets [] = [[]] -- Base case: the only subset of an empty list is the empty list
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs) -- Recursive case: combine subsets without the first element and subsets with the first element

travelSales :: RoadMap -> Path
travelSales rm = case tsp rm of
    (Just _, path) -> path -- If a valid path is found, return it
    (Nothing, _) -> [] -- If no valid path is found, return an empty list

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

