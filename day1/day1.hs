
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    raw_input <- readFile "input.txt"
    let input = lines raw_input
    case args of
        ["1"] -> do
               let answer = part_1 input
               -- let output = list_diffs $ fmap read input
               -- let answer = length $ filter (>0) output
               putStrLn $ show answer
        ["2"] -> do
                let debug = sliding_sum (fmap read input) [] 3
                putStrLn $ show debug
                let answer = part_2 input
                putStrLn $ show answer 
        otherwise -> putStrLn "Please (only) provide 1 argument for part number: 1 or 2"



list_diffs :: [Int] -> [Int]
list_diffs xs = zipWith (-) (tail xs) (init xs)

part_1 :: [String] -> Int
part_1 = length . filter (>0) . list_diffs . fmap read

part_2 :: [String] -> Int
part_2 input = part_1 $ fmap show $ sliding_sum (fmap read input) [] 3

sliding_sum :: [Int] -> [Int] -> Int -> [Int]
sliding_sum num_list window win_len
    | num_list == [] = [sum window]
    | length window < win_len = sliding_sum ns (window++[n]) win_len
    | otherwise = sum(window) : sliding_sum ns (tail window ++ [n]) win_len
    where
        n:ns = num_list

