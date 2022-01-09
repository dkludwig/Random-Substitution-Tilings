# Random Substitution Tilings!
This repository has some Mathematica code I wrote in 2019 that generates *random substitution tilings* (and some tools to analyze them). The most familiar example of an aperiodic tiling formed via substitution rules is probably the Penrose tiling (https://en.wikipedia.org/wiki/Penrose_tiling#Robinson_triangle_decompositions). Compared to the Penrose tiling, the substitution rules coded here would probably be deemed "degenerate", as they don't include any edge-matching requirements or give a definite growth rate. Compounding their degeneracy by applying them randomly is what creates tilings like this:
![ex4](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex4.png) 
And this:
![ex3](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex3.png) 

More specifically, these tilings are formed via substitution rules acting on triangles with angles that are integer multiples of $$\pi/p$$, $$p \geq 4$$ being an integer (45-45-90 and 30-60-90 triangles are the simplest examples). Each of these triangles can be bisected $$p-3$$ different ways to make two triangles still within that family (e.g. bisect the 90 degree angle in the 45-45-90 triangle and you get two more 45-45-90 triangles). This package allows you to randomly compose these rules (using an input probability vector) for whatever $$p$$-family you wish to study. This last example uses the $$p=6$$ family (green triangles: 60-60-60, blue: 120-30-30, black: 30-60-90).
![ex5](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex5.png) 
(I swear I didn't make those stars myself.)

# Getting Started
I'm planning on translating a stripped-down version of this code to Python somewhat soon, but until then, if you have access to Mathematica and want to play around with Substitution-Tilings-Package.m, just download Tutorial.nb and you should be on your way! I apologize in advance for the confusing lack of objects or even dicts...don't really know what I was thinking.



