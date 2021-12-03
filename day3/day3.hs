module Main where

import System.Environment
import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')

type BitString = String 
type BitVotes = [Int]

main :: IO ()
main = do
    args <- getArgs
    raw_input <- readFile "input.txt"
    let input = lines raw_input
    let len = length $ input !! 0
    case args of
        ["1"] -> let ans1 =  bin2dec $ votes_to_bin '0' '0' $ majority_bits len '1' input
                     ans2 = bin2dec $ votes_to_bin '0' '0' $ majority_bits  len '0' input
                 in
                 putStrLn $ show $ [ans1,ans2,ans1*ans2] 
        ["2"] -> let ans1 = bin2dec $ part_2 input '1' 0
                     ans2 = bin2dec $ part_2 input '0' 0
                 in
                 putStrLn $ show [ans1,ans2,ans1*ans2]
        otherwise -> putStrLn "Please (only) provide 1 argument for part number: 1 or 2"


bit_counts :: Char -> BitVotes -> BitString -> BitVotes
bit_counts _ [] [] = []
bit_counts mb (c:cs) (b:bs)
    | b ==  mb = (c+1) : bit_counts mb cs bs  
    | otherwise = (c-1) : bit_counts mb cs bs

majority_bits :: Int -> Char -> [BitString] -> BitVotes
majority_bits bit_len mb = foldl (bit_counts mb) (replicate bit_len 0)

votes_to_bin :: Char -> Char -> BitVotes -> String 
votes_to_bin tie_val mb = fmap (vote_2_bit tie_val mb)
    where 
        vote_2_bit tie_val mb vote
            | vote > 0 = mb
            | vote < 0 = inv(mb)
            | otherwise = tie_val

bin2dec :: BitString -> Int
bin2dec =  foldl' (\acc x -> acc * 2 + digitToInt x) 0

inv :: Char -> Char
inv mb = case mb of
            '1' -> '0'
            '0' -> '1'
          

part_2 :: [BitString] -> Char -> Int -> BitString
part_2 [b] _ _ = b
part_2 inputs ver ind = part_2 new_inputs ver (ind+1)
    where
        maj_bit | count > 0 =  ver
                | count < 0 = inv(ver)
                | otherwise = ver
                where count = (foldl (\a b -> if (b!!ind)=='1' then a+1 else a-1) 0 inputs)
        new_inputs = filter (\x -> (x!!ind)==maj_bit) inputs

