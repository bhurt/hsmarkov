
import Data.Char
import qualified Data.Map.Strict as Map
import Data.List(foldl')
import System.Random

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
                             | otherwise = (Word (reverse w)) : syms (x:xs)

ngrams :: [ Sym ] -> [ NGram ]
ngrams [] = []
ngrams [x] = []
ngrams (x1:x2:xs) = (x1,x2) : ngrams (x2:xs)

follows :: [ NGram ] -> WordCount -> WordCount
follows = foldl' go
    where
        go m (one, two) = Map.alter (go1 two) one m
        go1 two Nothing = Just $ Map.singleton two 1
        go1 two (Just n) = Just $ Map.alter go2 two n
        go2 Nothing = Just 1
        go2 (Just x) = Just (x + 1)


wordCountFile :: FilePath -> WordCount -> IO WordCount
wordCountFile filename wc = do
    contents <- readFile
    return $ follows (ngrams (Period : syms contents)) wc

makeProbs :: WordCount -> MarkovMat
makeProbs = fmap go
    where
        go m = n, sortBy cmp lst2
            where
                lst = toList m
                n = sum $ map snd lst
                lst2 = map f lst
                f (s, c) = (c, s)
                cmp x y = compare (first y) (first x)

markovStep :: MarkovMat ->  Sym -> IO Sym
markovStep mat s = case Map.lookup s mat of
                        Nothing -> return Period -- umm...
                        Just lst -> getStdRandom (go lst)
    where
        go lst stdGen =
            let (i, stdGen') = next in
            (go2 i lst, stdGen')
        go2 _ [ (_, s) ] = s
        go2 i ((j, s) : xs) | i < j = s
                            | otherwise = go2 (i - j) xs

writeMarkovChain :: MarkovMat -> IO ()
writeMarkovChain mat = go Period
    where
        go prev = do
            s <- markovStep mat prev
            printSym prev s
            if (s == Period) then
                return ()
            else
                go s
        printSym _ Period = putStr ".  "
        printSym _ Comma = putStr ", "
        printSym (Word _) (Word w) = putStr $ " " ++ w
        printSym _ (Word w) = putStr w


main :: IO ()
main = putStrLn "Hello, world!"
