import System.Process
import System.Environment
import Control.Monad
import Text.Regex.TDFA
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char
import Numeric

accessPointsFile = "/tmp/wifiAccessPoints"

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

findInterface :: IO (Maybe String)
findInterface = do
    (_, s, _) <- readProcessWithExitCode "iwconfig" [] ""
    return $ submatch "([^ ]*) .*ESSID" s

getAccessPoints :: String -> IO [AccessPoint]
getAccessPoints interface = do
    s <- readProcess "iwlist" [interface, "scan"] ""
    return $ concat $ map (maybeToList.getAccessPoint) (cells s)

charToHex :: Char -> String
charToHex c | length s `mod` 2 == 0 = s
            | otherwise = '0':s
    where s = showHex (fromEnum c) ""

isGoodChar c = (isAscii c && isAlphaNum c) || any (==c) goodChars
    where goodChars = "[]!#$%^&()+-_/*"

isGoodString :: String -> Bool
isGoodString = all isGoodChar

--makeGood :: String -> String

config :: AccessPoint -> String -> String
config ap pass = 
    "network={\n" ++ nameLine ++ passLine ++ "}\n"
    where
        nameLine = "\tssid=\"" ++ essid ap ++ "\"\n" 
        passLine | pass == "" = ""
                 | otherwise  = "\tpsk=\"" ++ pass ++"\"\n"

scan interface = do
    aps <- getAccessPoints interface 
    putStrLn $ prettyPrint aps
    writeFile accessPointsFile $ show aps

connect id = do
    aps <- liftM read $ readFile accessPointsFile
    if (length aps >= id && id>0) then do
        putStrLn $ "Connecting..."
        putStrLn $ config (aps!!(id-1)) "some password"
    else do
        putStrLn "No such access point"

usage = do
    name <- getProgName
    putStrLn $ "Usage: \n" ++
        name ++ " scan [<interface>]\n" ++
        name ++ " connect <id>\n"

main = getArgs >>= parse
  where 
    parse ["scan", interface] = scan interface
    parse ["scan"] = do 
        maybeInterface <- findInterface
        case maybeInterface of
            Just interface -> scan interface
            Nothing -> usage
    parse ["connect", idString] = connect $ read idString
    parse _ = usage
