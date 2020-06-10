--  File     : Proj1.hs
--  Author   : Yajing Zhang <yajzhang@student.unimelb.edu.au>
--  Purpose  : Try to guess the locations selected by a composer with a minimum
--             of attempts
--
-- |This haskell program implements a game somewhatakin to the game of BattleShip.
--  The hider starts the game by selecting a three target location coordinate, where each location coordinate comprise a
--  colume and a row. For example, ("A1","B2","C3").
--  The searcher repeatedly choose a list of three location coordinate as a guess and give
--  it to the hider. The hider then responds the performer by giving
--  a feedback. This game is finished when the searcher find exactly the location coordinate that hider choose.
--  The goal of this program is to minimize the number of attempts.


module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where
              
import Data.List
import Data.Char
import Data.Maybe
import Data.Function (on)


--Define a Location type with consturctor Location.
data Location = Location {colume :: Char, row :: Char} deriving (Eq, Ord)


--The type for Gamestate is a list of Location list used as infomatin exchange cross-function 
type GameState = [[Location]]


--Gives Just the Location named by the string, or Nothing if the string is not a valid location name.
--ToLocation function
toLocation :: String -> Maybe Location
toLocation a
  |stringLen == 2  &&  (colume >= 'A'&& colume <= 'H') &&( row >='1'&& row <='4') = Just( Location colume row )
  |otherwise = Nothing 
   where stringLen = length a
         colume = head a
         row = last a


--instance declaration: rewrite the show as a list such as A1
instance Show Location where show (Location colume row ) =[colume, row]


--Helper function to get colume and row value from a location 
getColume :: Location -> Char
getColume (Location colume row) = colume
getRow :: Location -> Char
getRow (Location colume row) = row


{-
-feedback function takes a target and a guess, respectively, and returns three number the appropriate feedback
-The first number is the number of ships exactly located; 
-The Second number is the number of guesses that were exactly one space away from a ship (The eight squares adjacent to a square, including diagonally adjacent, are counted as distance 1 away); and
-The last number is the number of guesses that were exactly two spaces away from a ship(The sixteen squares adjacent to those squares are considered to be distance 2 away,).

-Feedback function implement the mathematical distance between two point which is the distance between two points P(x1,y1) and Q(x2,y2) is given by: d(P, Q) = √ (x2 − x1)2 + (y2 − y1)2. Thus, the distance with one space away point is either √2 or 1 assuming the distance between Adjacent letters is 1(the distance between A and B is 1). The distancewith two space away is one of 2 , √5 or 2√2. 

-}

feedback:: [Location]->[Location]->(Int,Int,Int)
feedback targetSet guessSet =(correct,oneStep,twoStep)
   where 
        distanceList = miniDistanceList guessSet targetSet  
        classifyList = concatMap classify distanceList  -- 9 digit Int list eg [0,0,1,1,0,0,3,0,0]
        correct = fixIntervalSum classifyList 0
        oneStep = fixIntervalSum classifyList 1
        twoStep = fixIntervalSum classifyList 2

        
--Helper function for feedback to get the correspondent number of correct/one stage away/ two step away elements
--fixIntervalSum generate sum of two number away until end of the input list start from a particular number
fixIntervalSum::[Int]->Int->Int
fixIntervalSum [] _ = 0
fixIntervalSum (e1:e2:e3:xs) startfrom 
 |startfrom == 0 = e1 + fixIntervalSum xs startfrom
 |startfrom == 1 = e2 + fixIntervalSum xs startfrom
 |startfrom == 2 = e3 + fixIntervalSum xs startfrom
               

--Helper function for feedback. Find the correspondent number given alphabet in order to calculate vertical distance between two points
letterToInt:: Char->Int
letterToInt char = fromMaybe 0(elemIndex char ['A'..'H'])

--Helper function for feedback. Find the of distance between two points. For comparation purpose, the distance used here is the square of distance in order to minimize the computation time. 
--find the distance square of distance for the convience 
distanceSquare :: (Int, Int)->(Int, Int)->Int
distanceSquare (x1 , y1) (x2 , y2) =  x'*x' + y'*y'
    where
      x' = x1 - x2
      y' = y1 - y2

--Helper function for feedback to find the minimum distance of one location to three target locations   
miniDistance :: Location -> [Location]-> Int
miniDistance guess [] = 1000
miniDistance  guess (x:xs) = minimum (distanceSquare (x1,y1) (x2,y2): miniDistance guess xs:[])
  where 
    x1 = letterToInt(getColume guess)
    y1 = digitToInt(getRow guess)
    x2 = letterToInt(getColume x)
    y2 = digitToInt(getRow x)    
        

--Helper Function for feedback to generate the list of minimum distance of one guess (including three locations), return three number represent the square of minimum distance of each locations
miniDistanceList :: [Location]->[Location]->[Int]
miniDistanceList [] targetSet = []
miniDistanceList (x:xs) targetSet = miniDistance x targetSet :miniDistanceList xs targetSet

{-
Helper function to convert the distance(more specific, square of minimum distance) to the measurment mentioned above in feedback function comment.
if one location is exactly located at one target location, the distance square is 0 
If one location is one step away from nearest target location, the distance square is either 1 or 2 
if one location is two step away from nearest target location, the distance square is one of 4 , 5 ,8
-}
classify :: Int->[Int]
classify  x
 |x==0 =[1,0,0] 
 |x >=1 && x <=2 = [0,1,0]
 |x >=4 && x <=8 = [0,0,1]
 |otherwise = [0,0,0]
 
 
 --initialGuess function takes no input arguments, and returns a pair of an initial guess and a game state.
initialGuess :: ([Location],GameState)
initialGuess = (guess,firstGameState) 
    where guess = initialGuessLocations
          firstGameState =initialGameState
 
--Two helper function together generate the all possible combination of location within 4X8 grid. it returns 4960 results.
allLocation :: [Location]
allLocation = [Location col row | 
   col <- ['A'..'H'], 
   row <- ['1'..'4'] ]
   
initialGameState :: [[Location]]
initialGameState = [[a,b,c]| a <- allLocation, 
   b <-[d| d <- allLocation, 
   d /= a, d<a], 
   c <- [e| e <- allLocation, 
   e /= a, e/= b, e<a,e<b]]
   
-- initial location to start the program  
initialGuessLocations :: [Location]
initialGuessLocations = [Location 'A' '1',Location 'B''2',Location 'C' '3' ];


{-
nextGuess function takes as input a pair of the previous guess and game state, and the feedback to this guess as a triple of the number of correct locations, the number of guesses exactly one square away from a ship, and the number exactly two squares away, and returns a pair of the next guess and new game state.

GameState is generated based on following logical
- Produc a gameState the only contains locatin candidates that havs the same result as previous guess through sameFeedbackLocations function. The target is one of those candidate.

Next guess is generated by following steps by given the set of possible guess (same as gameState above)
1.For each guess, calculate feedbacks of that Guess  with all other candidates as a list (getfeedback function)
2.For each guess, calculate the sum of (count of each feedback) * (count of each feedback/ all feedback count) and store the score number as second elemnt of tuple and the first element is correspondent guess. 
3. Sort list generated from previous step and sort it by the score and get the correspondent guess of minimum score. This score indicate on average this the guess will leave how many remaining candidate. 

-}
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (preGuess, preGameState) fb = (guess, cleanGameState)
  where sameFbLocations =[location| location<- preGameState, feedback location preGuess == fb] --Generate locations produce same feedback as previous guess
        cleanGameState =  sameFbLocations
        guess = bestGuess cleanGameState cleanGameState
      
--Helper function for generate the next guess. The purpose of this is for step three mentioned. Sort list based on score and get the minimum one.
bestGuess:: [[Location]]->GameState->[Location]
bestGuess target gameState = fst (last sortedList)
   where tupleList =calcuateScore target gameState
         sortedList = sortBy (flip compare `on` snd) tupleList --Sort list by second element of tuple in descending order


--Helper function for generate the next guess. Calcuate score for each guess and store that guess and score as tuple.
calcuateScore :: [[Location]] -> GameState ->[([Location],Float)]
calcuateScore [] _ =[]
calcuateScore target gameState  = ( (head target), score):  (calcuateScore (tail target) gameState)
   where  feedbackList = getfeedback (head target) gameState 
          groupedList = group feedbackList
          freList = freLength groupedList -- each element/int represents the the frequence of each possibility
          total = length gameState --remaining candidate as denominator
          termList = calculateTerm freList total  -- each element/float represent the calculation result of each possibility 
          score = sum (termList) 
         

--Helper function for generate the next guess. Compare one guess with others candicate and generate list of feedback 
getfeedback::[Location] -> [[Location]] -> [(Int, Int, Int)]
getfeedback _ [] = []
getfeedback target (x:xs) = fb : getfeedback target xs
    where fb = feedback target x

--Helper function for generate the next guess. Get the frequence of each type of feedback.
freLength:: [[(Int, Int, Int)]]->[Int]
freLength []=[]
freLength (x:xs) = length(x)  :freLength xs


--Helper function for generate the next guess. Compute each term of (count of each feedback) * (count of each feedback/ all feedback count) and store term as list waiting for compute sum.
calculateTerm :: [Int]->Int->[Float]
calculateTerm freList total = denominatorApplied
   where floatList = [ fromIntegral x| x <- freList] --convert [Int] to [Float]
         squareList =map (^2) floatList 
         denominatorApplied = map (/fromIntegral total) squareList
         
         








