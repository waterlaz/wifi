import System.Process
import Data.List.Split
import Control.Monad
import Text.Regex.TDFA
import Data.Maybe
import Data.List

data AccessPoint = AccessPoint {
        essid :: String,
        encryption :: Bool,
        address :: Maybe String,
        frequency :: Maybe String
} deriving (Read, Show)

pad n s = take n $ s ++ (repeat ' ')

formatColumns :: Int -> Int -> [[String]] -> String
formatColumns truncWidth space rows = 
    let cols = transpose rows
        maxWidth = map ((min truncWidth) . maximum . (map length))
                       cols
        cols' = zipWith (\c w -> map (pad w) c) cols maxWidth
        rows' = transpose cols'
     in unlines $ map (intercalate (replicate space ' ')) rows'


prettyFormat :: AccessPoint -> [String]
prettyFormat ap = [ essid ap, 
                    isLocked $ encryption ap,
                    mStoS $ frequency ap,
                    mStoS $ address ap
                  ]
    where mStoS = unwords . maybeToList
          isLocked True = "*  "
          isLocked False = "   "

prettyPrint :: [AccessPoint] -> String
prettyPrint aps = 
    let formatedAps = map prettyFormat aps
        nums = map show [1..]
        rows = ["#", "ESSID", "Key", "Frequency", "Address"]
             : zipWith (:) nums formatedAps
     in formatColumns 20 3 rows

cells :: String -> [String]
cells = tail . splitOn "          Cell "

submatch :: String -> String -> Maybe String
submatch r s = let (_, _, _, ms) = s =~ r 
                    :: (String, String, String, [String])
                in listToMaybe ms
 

getESSID :: String -> Maybe String
getESSID = submatch "ESSID:\"(.*)\"\n"

getAddress :: String -> Maybe String
getAddress = submatch "Address: (.*)\n"

getFrequency :: String -> Maybe String
getFrequency = submatch "Frequency:(.*) GHz"

getEncryption :: String -> Maybe Bool
getEncryption s = do 
    encryption <- submatch "Encryption key:(.*)\n" s
    if encryption == "off" then return False
                           else return True
getAccessPoint :: String -> Maybe AccessPoint
getAccessPoint s = do
    essid' <- getESSID s
    encryption' <- getEncryption s
    return AccessPoint 
        { essid = essid', 
          encryption = encryption',
          address = getAddress s,
          frequency = getFrequency s
        }
    

scan :: String -> IO [AccessPoint]
scan interface = do
    s <- readProcess "iwlist" [interface, "scan"] ""
    return $ concat $ map (maybeToList.getAccessPoint) (cells s)

main = do
    s <- scan "wlp3s0"
    putStrLn $ prettyPrint s
