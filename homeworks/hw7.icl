module hw7
import StdEnv

CollectSame :: [Int] -> [[Int]]
CollectSame [] = []
CollectSame [x:xs] = [[x:filter  ((==) x) xs]:CollectSame (filter ((<>)x) xs)] 

//Start = CollectSame [1,2,3,2,1]


MakeFrequenceTable :: [Int] -> [[Int]]
MakeFrequenceTable [] = [[]]
MakeFrequenceTable a = map (\x = [x!!0,length x,(length x)*100/(length a)]) (CollectSame a)

Start = MakeFrequenceTable [1,3,2,3,2]
