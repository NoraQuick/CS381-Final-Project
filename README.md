# CS381-Final-Project

1. Team members' names and ONIDs:
  - Thien Nam (namt)
  - Zach Parsons (parsonsz)
  - Luke Puppo (puppol)
  - Nora Quick (quickn)

2. Intro to our language:
  Our language is called Nam’s Quick PP and it is an imperative language. It will have features such as strings and operations as well as list and array data types and operations on top of the overall required features. It will have basic functions such as printing strings, returning a boolean, and simple mathematical functions.

3. How to execute example programs in our language:
  - If your language implementation is intended to be run from GHCi, which module should be loaded?:
      - NQPP.hs
  - If your language implementation is intended to be run from the command line, what command should be executed?:
      - Since our types are represented (shown as t in these examples) as CI(int), CB(bool), CF(float), or CS(string), for:
      - Add, you type "smt (t n :+: t n)
      - Subtract, you type "smt (t n :-: t n)
      - Multiply, you type "smt (t n :*: t n)
      - Divide, you type "smt (t n :/: t n)
      - Etc.
      - Obviously, all of these operators work for intergers and floats, but for bools and strings, only addition and subtraction will work, but they are not functional in our milestone implementation yet.

  - Include precise commands needed to execute both your good examples and bad examples together with expected output.:
  	- We have 6 examples so far. They are labeled ex1 through ex6. To run one call the example you want.
    - Please only do ex3 through ex6. As of this milestone those examples work but because of an issue with precedence we cannot get example 1 and 2 working.
