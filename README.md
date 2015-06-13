#Project proposal - Datatype Equality

##Motivation
In functional programming it is usually easy to define simple equality of values of data types by simply comparing two values. However, considering languages using some sort of binding constructs (e.g. lambda calculus), this is no longer straight forward. How can one test if two equivalent lambda expressions are equal, considering the fact that they may declare and use different variables (e.g. (\x -> x) == (\y -> y))? Since many object oriented languages nowadays are starting to incorporate functional constructs (e.g. C# with LINQ), being able to define equality on these constructs is an interesting problem that should be researched.

##Research question

* How can equality be defined on a small language with lambda expressions?
* How can equality be defined on languages using polynomial expressions?
* Is there a way to generically define equality on any language with binding constructs?

##Action plan
In order to answer the proposed research questions, the following list of steps is composed:

1. Define 2 datatypes in Haskell: a simple Lambda language, and Polynomial representation.
2. Try to define equality using de Bruijn indices.
3. Try to define equality using methods like alpha/beta-reduction.
4. Try to define equality on datatypes with binding constructs in a fixed point representation.
5. Try to define equality on type families.
