import System.Environment
import Data.List

type Token = (String,Integer)
type Tokens = [Token]
data PositionComponents = Horizontal Integer
                        | Depth Integer
                        | Aim Integer
                        deriving (Show)
data Position = Pos PositionComponents PositionComponents PositionComponents

data Version = One | Two

main :: IO ()
main = do
    args <- getArgs
    raw_input <- readFile "input.txt"
    let input = lines raw_input
    case args of
        ["1"] -> do
                let ans = top_lvl input One
                putStrLn $ show ans
        ["2"] -> do
                let ans = top_lvl input Two
                putStrLn $ show ans
        otherwise -> putStrLn "Please (only) provide 1 argument for part number: 1 or 2"


tokenize :: [String] -> Tokens
tokenize = fmap (\x -> let (a,b) = break (==' ') x in (a, read b :: Integer)) 

top_lvl :: [String] -> Version -> Integer
top_lvl input version = run_cmds (tokenize input) (Pos (Horizontal 0) (Depth 0) (Aim 0)) version

run_cmds :: Tokens -> Position -> Version -> Integer
run_cmds token_list position version =
    case (token_list,position) of
        ([],Pos (Horizontal h) (Depth d) _) -> h * d
        (t:ts,Pos hh@(Horizontal h) dd@(Depth d) aa@(Aim a)) -> let val = snd t
                                                                    new_pos = case (version,fst t) of
                                                                        (One,"forward") -> Pos (Horizontal (h+val)) dd aa
                                                                        (One,"down") -> Pos hh (Depth (d+val)) aa
                                                                        (One,"up") -> Pos hh (Depth (d-val)) aa
                                                                        (Two,"forward") -> Pos (Horizontal (h+val)) (Depth (d+a*val)) aa
                                                                        (Two,"down") -> Pos hh dd (Aim (a+val))
                                                                        (Two,"up") -> Pos hh dd (Aim (a-val))
                                                                in
                                                                run_cmds ts new_pos version
        otherwise -> error "This should not run"

