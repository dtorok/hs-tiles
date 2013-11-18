import Control.Lens hiding (transform)


-------------
-- data types
-------------

data Color = White | Yellow | Red deriving (Show, Eq)
data Wall = Wall {
			width  :: Int,
			height :: Int,
			tiles  :: [[Maybe Color]]
} deriving (Show)

type Shape = [(Int, Int)]


-------
-- wall
-------

createWall :: Int -> Int -> Maybe Color -> Wall
createWall w h c = Wall w h createRows
	where
		createRows = ((take h) . repeat) createRow
		createRow = ((take w) . repeat) c

setColor :: Int -> Int -> Color -> Wall -> Wall
setColor x y c wall = wall{tiles=tiles'}
	where
		tiles' = (tiles wall) & element y .~ row'
		row' = ((tiles wall) !! y) & element x .~ Just c

getColor :: (Int, Int) -> Wall -> Maybe Color
getColor (x, y) wall = (tiles wall) !! y !! x

isEmptyTitle :: Wall -> (Int, Int) -> Bool
isEmptyTitle wall coord = getColor coord wall == Nothing

wall2html :: Wall -> String
wall2html wall = mkhtml $ (mktable . unwords) htmlrows
	where
		htmlrows = map (mktr . unwords . htmlrow) $ tiles wall
		htmlrow row = map mktd row
		mkhtml content = "<html>\
		 				    \<head><style>\
		 				        \body { background-color: #dddddd; } \
		 				        \table { margin: auth } \
		 				    	\table tr td { width: 30px; height: 30px } \
						    	\.White { background-color: white } \
						    	\.Yellow { background-color: yellow } \
						    	\.Red { background-color: #99182C } \
						    	\.empty { background-color: grey } \
						  	\</style></head>\
						  	\<body>" ++ content ++ "</body>\
						  \</html>"

		mktable table = "<table>" ++ table ++ "</table>"
		mktr row = "<tr>" ++ row ++ "</tr>"
		mktd tile = "<td class=\"" ++ (getClass tile) ++ "\">&nbsp</td>"

		getClass :: Maybe Color -> String
		getClass (Just title) = show title
		getClass Nothing = "empty"

add :: Shape -> Int -> Int -> Int -> Color -> Wall -> Wall
add shape x y r c wall = wall'
	where
		shape' = transform x y r shape
		wall' = foldl setTile wall shape'
		setTile w (sx, sy) = setColor sx sy c w

addSafe :: Shape -> Int -> Int -> Int -> Color -> Maybe Wall -> Maybe Wall
addSafe _ _ _ _ _ Nothing = Nothing
addSafe shape x y r c (Just wall) = case isClear of
								True  -> Just (add shape x y r c wall)
								False -> Just wall
	where
		shape' = transform x y r shape
		isClear = (filter (hasTitleAlready wall) shape') == []
		hasTitleAlready wall coord = not $ isEmptyTitle wall coord

---------
-- shapes
---------

rotate :: Int -> Shape -> Shape
rotate r shape = map rotateCoord shape
	where 
		norm :: Int -> Int
		norm r | r > 3 = r `mod` 4
		norm r | r < 0 = 4 + (r `mod` 4)
		norm r = r

		nr = norm r
		rotateCoord coord = foldl rotateOnce coord [1..nr]
		rotateOnce (sx, sy) _ = (-sy, sx)

translate :: Int -> Int -> Shape -> Shape
translate x y shape = map translateCoord shape
	where
		translateCoord (sx, sy) = (x + sx, y + sy)

transform :: Int -> Int -> Int -> (Shape -> Shape)
transform x y r = (translate x y) . (rotate r)

sL :: Shape
sL = [(0, 0), (0, 1), (0, 2), (1, 2)]

sJ :: Shape
sJ = [(0, 0), (0, 1), (0, 2), (-1, 2)]

sI :: Shape
sI = [(0, 0), (1, 0), (2, 0), (3, 0)]

sS :: Shape
sS = [(0, 0), (1, 0), (1, -1), (2, -1)]

sZ :: Shape
sZ = [(0, 0), (1, 0), (1, 1), (2, 1)]

sO :: Shape
sO = [(0, 0), (1, 0), (1, 1), (0, 1)]

sT :: Shape
sT = [(0, 0), (1, 0), (2, 0), (1, 1)]


-------
-- main
-------

decorateWall :: Wall -> Wall
decorateWall wall = case decorate of
							Just wall' -> wall'
							Nothing    -> wall
	where
		decorate :: Maybe Wall
		decorate =
			addSafe sJ  8 3 3 Red $ 
			addSafe sI  6 4 0 Yellow $ 
			addSafe sZ  6 1 1 Red $
			addSafe sS  6 3 0 White $ 
			addSafe sJ  7 1 3 Yellow $
			addSafe sL  4 2 0 White $
			addSafe sO 10 1 0 White $
			addSafe sT 11 5 3 Yellow $ 
			addSafe sS 13 4 3 Red $
			Just wall



main :: IO ()
main = do
	let wall = decorateWall $
			   setColor 1 1 Yellow $ 
			   setColor 2 2 Red $ 
			   setColor 1 3 White $ 
			   createWall 30 7 Nothing	
	let wall' =  wall
	writeFile "/tmp/tiles.html" $ wall2html wall'
	--print $ wall
	--print $ wall2html wall
