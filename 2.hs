import Data.List

intCode program = intCode' (take 4 program) 0 program

intCode' [] _ program = program
intCode' (99:xs) _ program = program
intCode' [opCode, nPos, mPos, storePos] curPos program =
    let n = program !! nPos
        m = program !! mPos
        res = calc opCode n m
        (a,b) = splitAt storePos program
        program' = (a ++ [res] ++ tail b)
        nextPos = curPos + 4
        nextOp = take 4 $ snd $ splitAt nextPos program'
    in intCode' nextOp nextPos program'

calc (1) n m = n + m
calc (2) n m = n * m

test0 = intCode [1,9,10,3,2,3,11,0,99,30,40,50] == [3500,9,10,70,2,3,11,0,99,30,40,50]
test1 = intCode [1,0,0,0,99] == [2,0,0,0,99]
test2 = intCode [2,3,0,3,99] == [2,3,0,6,99]
test3 = intCode [2,4,4,5,99,0] == [2,4,4,5,99,9801]
test4 = intCode [1,1,1,4,99,5,6,0,99] == [30,1,1,4,2,5,6,0,99]

input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,19,6,23,2,13,23,27,1,9,27,31,2,31,9,35,1,6,35,39,2,10,39,43,1,5,43,47,1,5,47,51,2,51,6,55,2,10,55,59,1,59,9,63,2,13,63,67,1,10,67,71,1,71,5,75,1,75,6,79,1,10,79,83,1,5,83,87,1,5,87,91,2,91,6,95,2,6,95,99,2,10,99,103,1,103,5,107,1,2,107,111,1,6,111,0,99,2,14,0,0]
output1 =
    let input' = take 1 input ++ [12, 2] ++ drop 3 input
        res = intCode input'
    in head res

possiblePrograms = [[1, noun, verb] ++ (drop 3 input) | noun <- [0..99], verb <- [0..99]]

findProgram programs match = 
    let results = map (\program -> (take 3 program, (intCode program))) possiblePrograms
        matchingResults = filter (\(program, val) -> head val == match) results
        [_, noun, verb] = fst $ head matchingResults
    in 100 * noun + verb

output2 = findProgram possiblePrograms 19690720
