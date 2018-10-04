# prisoners_in_R

This project is for an algorithm to play in the TU Delft EPA1315 Prisoner's Dilemma tournament.

### Functionality
1. Uses an elementary genetic algorithm (based on Axelrod's method [1]) to learn the best ways to react to another agent based on their previous choices
2. Creates an agent that will be able to play with other agents in a tournament

This agent does not evolve during a game – it must be trained beforehand on some set of data. In this case, `tournament.csv` is from the EPA1315 Fall 2017 tournament. Since this agent uses Axelrod's method, the first three encounters will be played with a cooperate and then modified Tit-for-tat methodology. Only the `n > 3` encounter will actually implement Axelrod's method.

See prisoners_in_R.Rmd for more detailed documentation.

### Usage

1. Run main.py and copy the output string
2. Put the output string into `Agent_BRUTE_MELLOW.R`, which is the final agent we submitted.
3. Set your working directory to `Example_Tournament`
4. Run the example tournament `*.Rmd` file and see your results!

## Acknowledgements

This project was made by Siemon Keij, Phillip Seijger, Mees Hoff, and Jason R Wang.

[1] S. Mittal and K. Deb, “Optimal Strategies of the Iterated Prisoner’s Dilemma Problem for Multiple Conflicting Objectives,” IEEE Trans. Evol. Comput., vol. 13, no. 3, pp. 554–565, Jun. 2009.
