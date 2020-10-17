# Random-Substitution-Tilings
If you're willing to wade through my really unfortunate lack of dicts or objects, you can make some cool pictures! Like this one: 

![ex4](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex4.png) 
And this one:
![ex3](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex3.png) 

This one I decided to make copies of the triangles in this one to get a kaleidoscope feel:
![ex6](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex6.png) 

All of these are formed using random substitution rules on triangles with angles that are integer multiples of *pi/p*, *p* being an integer >=4 (45-45-90 and 30-60-90 triangles are familiar examples). Each of these triangles can be bisected *p-3* different ways to make two triangles still within that family (e.g. bisect the 90 degree angle in the 45-45-90 triangle and you get two more 45-45-90 triangles). Repeating one of these "substitution rules" over and over again creates a somewhat familiar self-similar tiling. You can make those with this package, but this project was more focused on *random* substitution tilings, which are formed via randomly applying (using an input probability vector) a combination of all the different substitution rules possible for the specific *p*-family that you want to look at. These can start looking pretty wild:
![ex5](https://github.com/dkludwig/Random-Substitution-Tilings/blob/main/examples/ex5.png) 
(I swear I didn't make those stars myself)
