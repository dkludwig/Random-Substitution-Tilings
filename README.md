# Random Substitution Tilings!
This repository has some Mathematica code I wrote in 2019 that generates *random substitution tilings* (and some tools to analyze them). The most familiar example of an aperiodic tiling formed via substitution rules is probably the Penrose tiling constructed with the Robsinson triangles (https://en.wikipedia.org/wiki/Penrose_tiling#Robinson_triangle_decompositions). Compared to the Penrose tiling, the substitution rules coded here would usually be called "degenerate", as they don't include any edge-matching requirements or give a definite growth rate. Compounding their degeneracy by applying them randomly is what creates tilings like this:
![ex4](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex4.png) 
And this:
![ex3](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex3.png) 

More specifically, the tilings in this package are formed via substitution on triangles with angles that are integer multiples of *pi/p*, *p >= 4* being an integer (45-45-90 and 30-60-90 triangles are the simplest examples). Each of these triangles can be bisected *p-3* different ways to make two triangles still within that family (e.g. bisect the 90 degree angle in the 45-45-90 triangle and you get two more 45-45-90 triangles). This package allows you to randomly applying (using an input probability vector) a combination of all the different substitution rules possible for the specific *p*-family in question. These can start looking pretty wild:
![ex5](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex5.png) 
(I swear I didn't make those stars myself)



