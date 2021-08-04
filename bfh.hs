import System.Environment
import System.IO (hFlush, stdout)
import Data.Char (chr, ord)
import Data.List
import Data.Word

data BrainOp = In
             | Out
             | OpenLoop
             | CloseLoop
             | Add  Int
             | Move Int
             | Break
             | Zero
             deriving (Show, Eq )

type Tape a = ([a], [a], Integer)

main :: IO()
main = getArgs             >>= (\args ->
       readFile (args!!0)  >>= (\file ->
       let program = preprocess file
           right =
             case elemIndex "-r" args of
               Nothing -> -1
               Just n  -> read $ args!!(n+1) :: Int
           left  =
             case elemIndex "-l" args of
               Nothing -> 0
               Just n  -> read $ args!!(n+1) :: Int
           cellVal =
             case elemIndex "-i" args of
               Nothing -> 0
               Just n  -> read $ args!!(n+1) :: Integer
           wrap =
             case elemIndex "-w" args of
               Nothing -> False
               Just n  -> if left >= 0 && right >= 0
                              then True
                              else False
       in case (read (args!!1) :: Int) of
              0  -> ( run program
                    $ buildTape left right wrap (abs cellVal))
                    >> return ()
              16 -> ( run program
                    $ buildTape left right
                        wrap (fromIntegral cellVal :: Word16))
                    >> return ()
              32 -> ( run program
                    $ buildTape left right
                        wrap (fromIntegral cellVal :: Word32))
                    >> return ()
              64 -> ( run program
                    $ buildTape left right
                        wrap (fromIntegral cellVal :: Word64))
                    >> return ()
              __ -> ( run program
                    $ buildTape left right
                        wrap (fromIntegral cellVal :: Word8))
                    >> return ()
       >> putStrLn "Program finished." ))

-- |Creates a brainfuck tape as a Zipper with specified lengths for
-- each side of the initial cell. Negative length values result in
-- an infinite list of cells for that side. Finite tapes with
-- wrapping are marked by the length of the tape in the third value.
-- Finite tapes without wrapping and infinite tapes are marked with
-- -1 as the third value. Every cell initially contains the value
-- passed as initValue, allowing for a tape of any type.
buildTape :: Int -> Int -> Bool -> a -> Tape a
buildTape leftLength rightLength wrapping initValue =
    ( if leftLength < 0
          then repeat initValue
          else replicate (leftLength + 1) initValue
    , if rightLength < 0
          then repeat initValue
          else replicate rightLength initValue
    , if leftLength >= 0 && rightLength >= 0 && wrapping
          then (fromIntegral leftLength) + (fromIntegral rightLength) + 1
          else -1 )

preprocess :: String -> [BrainOp]
preprocess code = replace []
                $ cancelOps
                $ readOps
                $ filter (`elem` ['<','>','+','-',',','.','[',']']) code

-- |Converts brainfuck source text into their corresponding BrainOp.
-- Sequences of '-', '+', '<', and '>' are also contracted into a
-- single op.
readOps :: String -> [BrainOp]
readOps code = concatMap (\xs ->
    let z = length xs in
      case head xs of
        '>' -> [Move z]
        '<' -> [Move (-z)]
        '+' -> [Add z]
        '-' -> [Add (-z)]
        '[' -> xs >> [OpenLoop]
        ']' -> xs >> [CloseLoop]
        ',' -> xs >> [In]
        '.' -> xs >> [Out]
     ) $ group code

-- |Takes a list of BrainOp and removes or combines Add and Move
-- ops which effectively cancel each other. Unreachable loops are
-- also removed.
cancelOps :: [BrainOp] -> [BrainOp]
cancelOps ops = foldr cancel [] ops
cancel (Add y) (Add x  : xs)  = if x + y == 0 then xs else (Add (x + y) : xs)
cancel (Move y) (Move x : xs) = if x + y == 0 then xs else (Move (x + y) : xs)
cancel CloseLoop (OpenLoop : xs) = (CloseLoop : skipLoop xs)
cancel op xs = (op : xs)

-- |Replaces common brainfuck snippets with a more efficient op.
replace :: [BrainOp] -> [BrainOp] -> [BrainOp]
replace ys (OpenLoop : Add 1 : CloseLoop : xs) =
    replace (Zero : ys) xs
replace ys (OpenLoop : Add (-1) : CloseLoop : xs) =
    replace (Zero : ys) xs
replace ys (x:xs) = replace (x:ys) xs
replace ys [] = reverse ys

-- |Accepts a list of BrainOps and a tape. The tape can be constructed
-- with any integral type, cell behavior is determined by this type.
-- when outputting a character the handle is flushed so it prints
-- immediately.
run :: (Integral a) => [BrainOp] -> Tape a -> IO (Tape a)
run [] tape = return tape
run (op : ops) (x : xs, ys, l) =
  case op of
    OpenLoop  -> if x == 0
                 then run (skipLoop ops) (x : xs, ys, l)
                 else run ops (x : xs, ys, l) >>= run (op : ops)
    CloseLoop -> return (x : xs, ys, l)

    In    -> putChar '?'
             >>  getChar
             >>= (\n -> run ops (fromIntegral (ord n) : xs, ys, l))
    Out   -> putChar (chr $ fromIntegral x)
             >> hFlush stdout
             >> run ops (x : xs, ys, l)

    op    -> run ops $ op # (x : xs, ys, l)

-- |Applies tape operations to the tape. Finite cells overflow and
-- underflow when appropriate. Unbounded cells with a value of zero
-- will remain zero when decremented. When moving off the end of the
-- tape, tapes without wrapping will not move further off the tape,
-- the first '<' or '>' encountered which moves the pointer back on
-- the tape will do so. Wrapping requires O(n) time to the move to the
-- other end of the zipper.
(#) :: (Integral a) => BrainOp -> Tape a -> Tape a
(Move z) # (xs, ys, l) =
    if l < 0
        then if z > 0
            then ((reverse $ take z ys) ++ xs, drop z ys, l)
            else (drop (-z) xs, (reverse $ take (-z) xs) ++ ys, l)
        else if z > 0
            then case drop z ys of
                [] -> (Move (z - length ys)) # ([], reverse xs ++ ys, l)
                ws -> ((reverse $ take z ys) ++ xs, drop z ys, l)
            else case drop (-z) xs of
                [] -> (Move (z + length xs)) # (reverse ys ++ xs, [], l)
                ws -> (drop (-z) xs, (reverse $ take (-z) xs) ++ ys, l)

(Add z) # (x : xs, ys, l) =
    let z' = fromIntegral z in
    if signum (x + z') == -1
        then (0 : xs, ys, l)
        else (x + z' : xs, ys, l)

Zero # (x : xs, ys, l) = (0 : xs , ys, l)

-- |Drops BrainOps until the matching CloseLoop is found, recursively
-- skipping nested loops.
skipLoop :: [BrainOp] -> [BrainOp]
skipLoop (OpenLoop : xs) = skipLoop $ skipLoop xs
skipLoop (CloseLoop : xs)   = xs
skipLoop (x : xs)     = skipLoop xs
