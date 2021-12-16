import Data.List.Split (chunksOf)
import Data.Bifunctor (second)


data Packet = Packet { ver :: Int,
                       typ :: Int,
                       lab :: Maybe Bool,
                       len :: Maybe Int,
                       num :: Maybe Int,
                       sub ::  Maybe [Packet] } deriving (Show)

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin _ = error "Invalid input"

parse :: String -> String
parse = concatMap hexToBin . filter (/='\n')

convert :: String -> Int
convert [] = 0
convert (x : xs) = y + 2 * convert xs
      where y = read [x] :: Int

getHeader :: String -> ((Int, Int), String)
getHeader (v0:v1:v2:t0:t1:t2:tail') = ((v, t), tail')
      where v = convert . reverse $ [v0, v1, v2]
            t = convert . reverse $ [t0, t1, t2]
getHeader _ = error "Can't parse"

getPacketType :: String -> Int
getPacketType = snd . fst . getHeader

transLiteral :: String -> (Packet, String)
transLiteral n = (packet, drop actualLength tail')
      where ((v, t), tail') = getHeader n
            n' = chunksOf 5 tail'
            last' = tail . head $ dropWhile ((/='0') . head) n'
            init' = map tail $ takeWhile ((/='0') . head) n'
            actualLength = (*5) . (1+) . length $ init'
            value = convert . reverse $ concat init' ++ last'
            packet = Packet {ver=v,
                             typ=t,
                             lab=Nothing,
                             len=Nothing,
                             num=Just value,
                             sub = Nothing}

getIdAndLength :: String -> ((Bool, Int), String)
getIdAndLength n
      | i = ((i, convert . reverse $ bit11), drop 11 tail')
      | otherwise = ((i, convert . reverse $ bit15), drop 15 tail')
      where i = (=='1') . head $ n
            tail' = tail n
            bit15 = take 15 tail'
            bit11 = take 11 tail'

iterNTranslate' :: Int -> String -> [Packet] -> ([Packet], String)
iterNTranslate' 0 rem acc = (reverse acc, rem)
iterNTranslate' c rem acc = iterNTranslate' (c - 1) tail' (packet:acc)
      where (packet, tail') = translate' rem

iterTranslate' :: String -> [Packet] -> [Packet]
iterTranslate' "" acc = reverse acc
iterTranslate' rem acc = iterTranslate' tail' (packet:acc)
      where (packet, tail') = translate' rem

transOperator :: String -> (Packet, String)
transOperator n = (packet, tail''')
      where ((v, t), tail') = getHeader n
            ((lab', len'), tail'') = getIdAndLength tail'
            (packets, tail''') = if lab'
                                 then iterNTranslate' len' tail'' []
                                 else (\n -> (n, drop len' tail''))
                                      . flip iterTranslate' []
                                      . take len'
                                      $ tail''
            packet = Packet {ver=v,
                             typ=t,
                             lab=Just lab',
                             len=Just len',
                             num=Nothing,
                             sub=Just packets}

translate' :: String -> (Packet, String)
translate' n = case getPacketType n of
                  4 -> transLiteral n
                  _ -> transOperator n

translate :: String -> Packet
translate = fst . translate'

sumVersion :: Packet -> Int
sumVersion (Packet n _ _ _ _ Nothing) = n
sumVersion (Packet n _ _ _ _ (Just sum')) = (n+)
                                                                         . sum
                                                                         . map sumVersion
                                                                         $ sum'

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

eval :: Packet -> Int
eval (Packet _ _ _ _ (Just n) Nothing) = n
eval (Packet _ 0 _ _ _ (Just m)) = sum . map eval $ m
eval (Packet _ 1 _ _ _ (Just m)) = product . map eval $ m
eval (Packet _ 2 _ _ _ (Just m)) = minimum . map eval $ m
eval (Packet _ 3 _ _ _ (Just m)) = maximum . map eval $ m
eval (Packet _ 5 _ _ _ (Just [a, b])) = boolToInt $ eval b < eval a
eval (Packet _ 6 _ _ _ (Just [a, b])) = boolToInt $ eval b > eval a
eval (Packet _ 7 _ _ _ (Just [a, b])) = boolToInt $ eval b == eval a
eval _ = error "Invalid syntax"

solve1 :: Packet -> Int
solve1 = sumVersion

solve2 :: Packet -> Int
solve2 = eval

main :: IO ()
main = do
      input <- getContents
      let ast = translate . parse $ input
      print . solve1 $ ast
      print . solve2 $ ast