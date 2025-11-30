module MorseCode where

import Types
import Data.List


encodeWord :: Table -> String -> Code
encodeWord table str = let codes = [code | c <- str, Just code <- [lookup c table]] 
                        in if null codes then [] 
                       else intercalate shortGap codes

encodeWords :: Table -> [String] -> Code
encodeWords table xs = case xs of
                       [] -> []
                       [x] -> encodeWord table x
                       (x:xs) -> encodeWord table x ++ mediumGap ++ encodeWords table xs

encodeText :: Table -> String -> Code
encodeText table str = encodeWords table (split " " str)

split :: Eq a => [a] -> [a] -> [[a]]
split sep str = reverse (map reverse (splitReversed (reverse sep) (reverse str)))
  where
    splitReversed sep str
      | null str = [[]]
      | take (length sep) str == sep = [] : splitReversed sep (drop (length sep) str)
      | otherwise = let (c:cs) = str
                        (w:ws) = splitReversed sep cs
                    in (c:w):ws

decodeText :: Table -> Code -> String
decodeText t code = 
    let wordCodes = split mediumGap code
        decodedWords = map (\wordCode -> 
                              let charCodes = split shortGap wordCode
                                  decodedChars = map (\charCode -> 
                                                       case lookup charCode [(c, ch) | (ch, c) <- t] of
                                                         Just ch -> ch
                                                         Nothing -> '?'
                                                     ) charCodes
                              in decodedChars
                           ) wordCodes
    in unwords decodedWords

decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree tree code = 
    let wordCodes = split mediumGap code
        decodedWords = map (\wordCode -> 
                              let charCodes = split shortGap wordCode
                                  decodedChars = map (\charCode -> 
                                                       traverseTree tree charCode
                                                     ) charCodes
                              in decodedChars
                           ) wordCodes
    in unwords decodedWords
  where
    traverseTree t c = 
        case t of
            Empty -> '?'
            Branch maybeChar left right -> 
                case c of
                    [] -> case maybeChar of
                            Just ch -> ch
                            Nothing -> '?'
                    _ | take (length dit) c == dit -> traverseTree left (drop (length dit) c)
                      | take (length dah) c == dah -> traverseTree right (drop (length dah) c)
                      | otherwise -> '?'                                            
                               

ramify :: Table -> Tree
ramify table = foldl insertPair initialTree table
 where
  initialTree = Branch Nothing Empty Empty
  insertPair tree (char,code) = insert char code tree
   where
    insert char code tree = 
     case tree of
      Empty -> 
       case code of
       _| null code -> Branch (Just char) Empty Empty
        | take (length dit) code == dit -> Branch Nothing (insert char (drop (length dit) code) Empty) Empty
        | take (length dah) code == dah -> Branch Nothing Empty (insert char (drop (length dah) code) Empty)
        | otherwise -> Empty 
      Branch maybeChar left right ->
       case code of
       _| null code ->  Branch (Just char) left right
        | take (length dit) code == dit -> Branch maybeChar (insert char (drop (length dit) code) left) right
        | take (length dah) code == dah -> Branch maybeChar left (insert char (drop (length dah) code) right)
        | otherwise -> Branch maybeChar left right 
               

tabulate :: Tree -> Table
tabulate tree = helper tree []
 where 
  helper tree currentPath =
   case tree of
    Empty -> []
    Branch maybeChar left right ->
     let leftResults = helper left (currentPath ++ dit)
         rightResults = helper right (currentPath ++ dah)
         resultOfBranch = 
          case maybeChar of
           Just char -> [(char, currentPath)]
           Nothing -> []
     in leftResults ++ resultOfBranch ++ rightResults 
         

brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree xs = 
    case parseOne xs of
     Nothing -> Nothing
     Just (bracket, remaining) -> 
        if null remaining 
        then Just bracket 
        else Nothing
  where
    parseOne str = 
        case str of
            ('(':rest) -> 
                case parseMany rest of
                    Nothing -> Nothing
                    Just (trees, remaining) ->
                        case remaining of
                            (')':after) -> Just (Round trees, after)
                            _ -> Nothing
            ('{':rest) -> 
                case parseMany rest of
                    Nothing -> Nothing
                    Just (trees, remaining) ->
                        case remaining of
                            ('}':after) -> Just (Curly trees, after)
                            _ -> Nothing
            _ -> Nothing
    
    parseMany str = 
        case str of
            (')':_) -> Just ([], str)
            ('}':_) -> Just ([], str)
            [] -> Just ([], [])
            _ -> case parseOne str of
                    Nothing -> Nothing
                    Just (tree, remaining) -> 
                        case parseMany remaining of
                            Nothing -> Nothing
                            Just (trees, finalRemaining) -> 
                                Just (tree:trees, finalRemaining)
  
   

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True
