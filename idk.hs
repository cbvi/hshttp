
splitLines :: String -> [String]
splitLines s =
    let (pref, suff) = break lineTerm s
    in pref : case suff of
        ('\r':'\n':xs)  -> splitLines xs
        ('\r':xs)       -> splitLines xs
        ('\n':xs)       -> splitLines xs
        _               -> []
    where
        lineTerm '\r'   = True
        lineTerm '\n'   = True
        lineTerm _      = False

prologue :: String -> Maybe (String, String)
prologue =
    decompose . words
    where
        decompose [a, b, "HTTP/1.0"] = Just (a, b)
        decompose [a, b, "HTTP/1.1"] = Just (a, b)
        decompose _                  = Nothing

header :: String -> Maybe (String, String)
header s =
    let (pref, suff) = break (== ':') s
    in case suff of
        (':':xs)    -> Just (pref, headerValue xs)
        _           -> Nothing
    where
        headerValue v = dropWhile isOws v
        isOws ' '   = True
        isOws '\t'  = True
        isOws _     = False

headers :: [String] -> Maybe [(String, String)]
headers hl = mapM header $ filter (not . null) hl

putSuc :: (String, String) -> IO ()
putSuc (k, v) = putStr k >> putStr "=" >> putStrLn v

main :: IO ()
main = do
    let s = "POST /this/test HTTP/1.1\r\nHost:    localhost\r\nUser-Agent: \ttest\r\nBlah: stuff\r\n\r\n"

    let l = splitLines s
    --let l = lines s

    case prologue $ head l of
        Just (m, p) -> putStrLn p >> putStrLn m
        Nothing -> putStrLn "oops"

    case headers $ tail l of
        Just h -> mapM_ putSuc h
        Nothing -> putStrLn "oops"
