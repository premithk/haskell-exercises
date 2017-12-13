module IsbnVerifier (isbn) where

cleanupisbn :: [Char] -> [Int]
cleanupisbn isbn = [fromEnum x - fromEnum '0' | x<-isbn, x `elem` 'X':['0'..'9']]

fixcheckdigit :: [Int] -> [Int]
fixcheckdigit isbn 
    | last isbn == 40 = init isbn ++ [10]
    | otherwise  = isbn

isbn :: String -> Bool
isbn isbn 
    | length cleanedupisbn /= 10 = False
    | checksum `mod` 11 == 0 = True
    | otherwise = False
    where cleanedupisbn = cleanupisbn isbn
          cleanedandfixedisbn = fixcheckdigit cleanedupisbn
	  checksum = sum [x*y | (x,y) <- zip cleanedandfixedisbn [10,9..1]]
