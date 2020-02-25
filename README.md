# CS381-Final-Project

1. Team members' names and ONIDs:
  - Thiem Nam (namt)
  - Zachary Parsons (parsonsz)
  - Luke Puppo (puppol)
  - Nora Quick (quickn)

2. Intro to our language:
  Our language is called Nam’s Quick PP and it is an imperative language. It will have features such as strings and operations as well as list and array data types and operations on top of the overall required features. It will have basic functions such as printing strings, returning a boolean, and simple mathematical functions.

3. How to execute example programs in our language:
  - If your language implementation is intended to be run from GHCi, which module should be loaded?:
      - NQPP.hs
  - If your language implementation is intended to be run from the command line, what command should be executed?:
      - DoTheAdd
      - DoTheSub
      - DoTheMult
      - DoTheDiv
      - Etc.

  - Include precise commands needed to execute both your good examples and bad examples together with expected output.:
      - DoTheAdd(5 int, 5 int)
        -- Expected output: 10
      - DoTheAdd(True bool, 5 int)
      	-- Expected output: Error
      - DoTheSub(5 int, 5 int) 
        -- Expected output : 0
      - DoTheSub(True bool, 5 int)
      	-- Expected output: Error
			- DoTheMult(5 int, 5 int)
				-- Expected output: 25
      - DoTheMult(5 int, False bool)
      	-- Expected output: Error
			- DoTheDiv(30 int, 5 int)
				-- Expected output: 6
      - DoTheDiv(True bool, 5 int)
      	-- Expected output: Error
