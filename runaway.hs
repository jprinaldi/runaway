module Main (main) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.Regex (mkRegex, matchRegex, splitRegex)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad (mplus)

-- Type synonyms
type Board = [String]
type Path = String
type Pos = (Int,Int)
type Terrain = String
type Level = Int
type Size = Int
type Length = Int
type Info = (Terrain,Length,Length,Size,Level)
type Get a = Info -> a

-- CONSTANTS
name = "Juampi"
spw = "7450415999e17c66bb3a70ee43017266"
url = "http://www.hacker.org/runaway/index.php?name="++name++"&spw="++spw;

-- #########################
-- # Uploading/Downloading #
-- #########################

-- Requests a URL; used for both downloading levels and uploading solutions
request :: String -> IO String
request param = do
	rsp <- simpleHTTP $ getRequest $ url ++ param
	getResponseBody rsp
	
-- Changes current level
goToLevel :: Level -> IO ()
goToLevel lvl = do
	body <- request $ "&gotolevel=" ++ show lvl
	case parse body of
		Nothing -> putStrLn $ "ERROR: " ++ body ++ "\n"
		Just _ -> putStrLn "Level changed successfully.\n"

-- ##########################
-- # Outputting Information #
-- ##########################
	
-- Outputs level info
showInfo :: Info -> IO ()
showInfo (t,max,min,size,lvl) = do
	putStrLn $ " " ++ titlePad ++ "\n-" ++ title ++ "\n " ++ titlePad ++ "\n"
	putStrLn "Level information:\n"
	putStrLn $ "|| Min path length: " ++ show min
	putStrLn $ "|| Max path length: " ++ show max
	putStrLn $ "|| Board size: " ++ show size ++ "\n"
	where	title = "# Level " ++ show lvl ++ " #";
			titlePad = padding '#' title;

-- Outputs solution info
showSolution :: Path -> IO ()
showSolution path = do
	putStrLn "Solution information:\n"
	putStrLn $ "|| Path length: " ++ show (length path)
	putStrLn $ "|| Path: " ++ path ++ "\n"

-- Outputs board
showBoard :: Board -> IO ()
showBoard [] = putStrLn ""
showBoard (row:rest) = do
	putStrLn $ " " ++ row
	showBoard rest
	
-- Outputs program instructions
showInstructions :: IO ()
showInstructions = do
	putStrLn "Instructions:"
	putStrLn "(1) Show current level's info."
	putStrLn "(2) Show current level's board."
	putStrLn "(3) Solve a level (won't upload solution)."
	putStrLn "(4) Solve all levels (WARNING: won't stop until finished)."
	putStrLn "(5) Change level."
	putStrLn "(6) Quit."

padding :: Char -> String -> String
padding c = foldr (\a b -> c:b) []
	
-- #############################
-- # Using/Parsing Information #
-- #############################

-- Parses HTML; extracts terrain, path length, board size and level number
parse :: String -> Maybe Info
parse html = do
	match <- matchRegex regex html
	match2 <- return $ head match
	match3 <- return $ splitRegex regex2 match2
	match4 <- return $ concatMap (splitRegex regex3) match3
	match5 <- return $ filter (\x -> (head x) /= 'F') match4
	return (match5!!0,read $ match5!!1,read $ match5!!2,read $ match5!!3,read $ match5!!5)
	where	regex = mkRegex "FlashVars=\"([^\"]*)\"";
			regex2 = mkRegex "&";
			regex3 = mkRegex "="

-- Creates board with given size and terrain
createBoard :: Size -> Terrain -> Board
createBoard _ [] = []
createBoard n xs = (take n xs):(createBoard n $ drop n xs)

getTerrain :: Get Terrain
getTerrain (terrain,_,_,_,_) = terrain

getMax :: Get Length
getMax (_,max,_,_,_) = max

getMin :: Get Length
getMin (_,_,min,_,_) = min

getSize :: Get Size
getSize (_,_,_,size,_) = size

getLevel :: Get Level
getLevel (_,_,_,_,lvl) = lvl
	
toLevel :: String -> Maybe Level
toLevel s = case reads s :: [(Int,String)] of
	[(lvl,"")] -> Just lvl
	_ -> Nothing

-- #####################
-- # Board Interaction #
-- #####################

-- Replaces a position with given char
replace :: Pos -> Char -> Board -> Board
replace (i,j) c b	= (take i b) ++ [(take j (b!!i)) ++ [c] ++ (drop (j+1) (b!!i))] ++ (drop (i+1) b)	

-- Frees a position
free :: Pos -> Board -> Board
free p b = replace p '.' b

-- Blocks a position
block :: Pos -> Board -> Board
block p b = replace p 'X' b

-- Checks if a position is free
isFree :: Pos -> Board -> Bool
isFree (i,j) b	| b!!i!!j == '.'	= True
				| otherwise			= False

-- Checks if a position is "really" free
isReallyFree :: Pos -> Pos -> Int -> Int -> Board -> Bool
isReallyFree (x,y) (p,q) h w b	| x >= h || y >= w		= True
								| not $ isFree (x,y) b	= False
								| otherwise				= isReallyFree (x+p,y+q) (p,q) h w b
									
-- Searches for a path looping in given position (second argument)
searchPosition :: Pos -> Pos -> Path -> Board -> Maybe Path
searchPosition (x,y) p path b	| (x,y) == (0,0) = Just path
								| not $ isReallyFree (0,0) p (length b) (length $ head b) b = Nothing 
								| x*y > 0 = mplus (searchPosition (x-1,y) p (path++"D") (tail b)) (searchPosition (x,y-1) p (path++"R") (map tail b))
								| x > 0 = searchPosition (x-1,y) p (path++"D") (tail b) --drop first row
								| y > 0 = searchPosition (x,y-1) p (path++"R") (map tail b) --drop first column

-- Searches for a path of given length
searchLength :: Length -> Pos -> Board -> Maybe Path
searchLength n (p,q) b	| p == 0 = Nothing
						| path /= Nothing = path
						| otherwise = searchLength n (p-1,q+1) b
						where path = searchPosition (p,q) (p,q) "" b

-- Creates path that solves the board
createPath :: Length -> Length -> Board -> Maybe Path
createPath min max b	| min > max = Nothing
						| path /= Nothing = path
						| otherwise = createPath (min+1) max b
						where path = searchLength min (min,0) b

-- Uses info provided to try to find a path
solve :: Info -> Maybe Path
solve info = createPath (getMin info) (getMax info) (createBoard (getSize info) (getTerrain info))
	
-- Requests a URL with a given param (used for uploading solutions)
-- Downloads the resulting body, parses it, uses info to solve level, and returns the corresponding solution
solve' :: String -> IO String
solve' param = do
	startDownloadTime <- getCurrentTime
	html <- request param
	endDownloadTime <- getCurrentTime
	case parse html of
		Nothing -> do 
			putStrLn "ERROR: Unable to parse HTML. Site is probably down. Please try again later.\n"
			putStrLn "- - - - - - - - - -"
			putStrLn "- - - - - - - - - -\n"
			return param
		Just info -> do
			if getLevel info > 509
				then do
					putStrLn "reached the end!"
					return ""
				else do
					showInfo info
					putStrLn $ "Download time: " ++ (show $ diffUTCTime endDownloadTime startDownloadTime) ++ "\n"
					putStrLn "| | | | | | | | | |"
					putStrLn "v v v v v v v v v v\n"
					startSolvingTime <- getCurrentTime
					case solve info of
						Nothing -> do
							putStrLn "ERROR: No solution found!\n"
							putStrLn "- - - - - - - - - -"
							putStrLn "- - - - - - - - - -\n"
							return ""
						Just solution -> do
							showSolution solution
							endSolvingTime <- getCurrentTime
							putStrLn $ "Solving time: " ++ (show (diffUTCTime endSolvingTime startSolvingTime)) ++ "\n"
							putStrLn "- - - - - - - - - -"
							putStrLn "- - - - - - - - - -\n"
							return $ "&path=" ++ solution

-- Uploads given solution to a level, downloads next, solves it, and loops with new solution
solveAll' :: String -> IO ()
solveAll' old = do
	new <- solve' old
	case new of
		"" -> putStrLn "\n"
		_ -> solveAll' new
					
-- Downloads and solves a single level; does not upload solution
solveOne :: IO ()
solveOne = do
	solve' ""
	return ()

-- Solves levels one after another
solveAll :: IO ()
solveAll = solveAll' ""

-- #####################
-- # Human Interaction #
-- #####################

-- Asks for and goes to a specific level
changeLevel :: IO ()
changeLevel = do
	putStrLn "Please select a level: "
	s <- getLine
	putStrLn "Retrieving information...\n"
	case toLevel s of
			Nothing -> putStrLn "Invalid input.\n"
			Just lvl -> goToLevel lvl

-- Main
main :: IO ()
main = do
	putStrLn "Runaway Robot Solver (c) Juampi"
	showInstructions
	putStrLn "What do you want to do?"
	opt <- getLine
	case opt of
		"1" -> do
			putStrLn "Retrieving information...\n"
			html <- request ""
			case parse html of
				Nothing -> do
					putStrLn "Unable to parse HTML. Site is probably down. Please try again later.\n"
					main
				Just info -> do
					showInfo info
					main
		"2" -> do
			putStrLn "Retrieving information...\n"
			html <- request ""
			case parse html of
				Nothing -> do
					putStrLn "Unable to parse HTML. Site is probably down. Please try again later.\n"
					main
				Just info -> do
					showBoard $ createBoard (getSize info) (getTerrain info)
					main
		"3" -> do
			putStrLn "Retrieving information...\n"
			solveOne
			main
		"4" -> do
			solveAll
			main
		"5" -> do
			changeLevel
			main
		"6" -> putStrLn "bye!"
		_ -> do
			putStrLn "I don't understand that. Please try again.\n"
			main
