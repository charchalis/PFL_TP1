# T14_G03

## Group Members
- Rafael Filipe Barbosa da Costa up202205013 (Contribution: )
- Miguel Wojciech de Vasconcelos Charchalis up201506074 (Contribution: )

### ShortestPath Function

### TravelSales Function

Two datatypes were defined: 
	- TspCoord that keeps track of the current city and the cities that have yet to be visited
	- TspEntry that represents the best solution for a given TspCoord keeping track of the total distance of the path and the path itself

The travelSales function tries to solve the tsp problem from every start/end city and returns the shortest one. It uses TspCoord to track the current and remaining cities and TspEntry to store the best solution for each TspCoord so they can be used later if needed.

For each starting city, it is removed from the list of remaining cities and compTsp is called. compTsp recursively explores possible paths checking for remaining cities and calculating the distances to potencial next cities. It uses the compareTspEntry function to keep track of the shortest solution for each state.

After all paths are calculated the path with the least distance is returned. If no path is found, an empty list is returned
