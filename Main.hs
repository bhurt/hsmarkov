
import Data.Char
import qualified Data.Map.Strict as Map
import Data.List(foldl', sortBy)
import System.Random
import System.Environment

data Sym =
    Word String
    | Comma
    | Period
    deriving (Show, Read, Eq, Ord)

type NGram = (Sym, Sym)

type WordCount = Map.Map Sym (Map.Map Sym Int)

type MarkovMat = Map.Map Sym (Int, [ (Int, Sym) ])

syms :: String -> [ Sym ]
syms [] = []
syms (x:xs) | isSpace x = syms xs
            | (x == '.') = Period : (syms xs)
            | (x == ',') = Comma : (syms xs)
            | isAlpha x = consumeWord [x] xs
            | otherwise = syms xs
    where
        consumeWord w [] = [ Word (reverse w) ]
        consumeWord w (x:xs) | isAlpha x = consumeWord (x:w) xs
                             | otherwise = Word word : syms (x:xs)
                                            where
                                                word = map toLower $ reverse w

ngrams :: [ Sym ] -> [ NGram ]
ngrams [] = []
ngrams [x] = []
ngrams (x1:x2:xs) = (x1,x2) : ngrams (x2:xs)

follows :: WordCount -> [ NGram ] -> WordCount
follows = foldl' go
    where
        go m (one, two) = Map.alter (go1 two) one m
        go1 two Nothing = Just $ Map.singleton two 1
        go1 two (Just n) = Just $ Map.alter go2 two n
        go2 Nothing = Just 1
        go2 (Just x) = Just (x + 1)


wordCountFile :: IO WordCount -> FilePath -> IO WordCount
wordCountFile wcio filename = do
    wc <- wcio
    putStrLn $ "Reading file: " ++ filename
    contents <- readFile filename
    return $ follows wc $ ngrams $ Period : syms contents

makeProbs :: WordCount -> MarkovMat
makeProbs = fmap go
    where
        go m = (n, sortBy cmp lst2)
            where
                lst = Map.toList m
                n = sum $ map snd lst
                lst2 = map f lst
                f (s, c) = (c, s)
                cmp x y = compare (fst y) (fst x)

markovStep :: MarkovMat ->  Sym -> IO Sym
markovStep mat s = case Map.lookup s mat of
                        Nothing -> return Period -- umm...
                        Just (cnt, lst) -> getStdRandom (go cnt lst)
    where
        go cnt lst stdGen = (go2 (i `mod` cnt) lst, stdGen')
            where
                (i, stdGen') = next stdGen
        go2 _ [ (_, s) ] = s
        go2 i ((j, s) : xs) | i < j = s
                            | otherwise = go2 (i - j) xs

markovSentence :: MarkovMat -> IO [ Sym ]
markovSentence mat = go Period
    where
        go prev = do
            s <- markovStep mat prev
            if (s == Period) then
                return $ [ s ]
            else
                do
                    t <- go s
                    return $ s : t

writeSentence :: [ Sym ] -> IO ()
writeSentence = go Period
    where
        go _ [] = putStr "\n"
        go _ (Period : xs) = do
            putStr "."
            go Period xs
        go _ (Comma : xs) = do
            putStr ", "
            go Comma xs
        go (Word _) (Word s : xs) = do
            putStr $ " " ++ s
            go (Word s) xs
        go _ (Word s : xs) = do
            putStr s
            go (Word s) xs

main :: IO ()
main = do
    args <- getArgs
    wc <- foldl' wordCountFile (return Map.empty) args
    let markovMat = makeProbs wc
    putStrLn "Generating output: "
    sequence_ $ map (go markovMat) [ 1 .. 10 ]
        where
            go mat _ = do
                sentence <- markovSentence mat
                writeSentence sentence

