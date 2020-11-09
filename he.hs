
data Method = GET | POST
type Path = String

firstSpace :: String -> Int -> Maybe Int
firstSpace [] _ = Nothing
firstSpace s i = if head s == ' '
    then Just i
    else firstSpace (drop 1 s) (i+1)

takeWord :: String -> Maybe String
takeWord s = case firstSpace s 0 of
    Just i -> Just $ take i s
    Nothing -> Nothing

strMethod :: String -> Maybe Method
strMethod s = case s of
    "GET"   -> Just GET
    "POST"  -> Just POST
    _       -> Nothing

parsePrologue :: String -> Maybe (Method, Path)
parsePrologue s = do
    ms <- takeWord s
    m <- strMethod ms
    p <- takeWord $ drop ((length ms) + 1) s
    Just (m, p)

prologue :: String -> Maybe (Method, Path)
prologue s = parsePrologue s

parse :: String -> IO ()
parse s = do
    case prologue s of
        Just _ -> return ()
        Nothing -> return ()
    return ()

main :: IO ()
main = do
    let s = "POST /this/is/a/test HTTP/1.1\r\nHost: localhost\r\nUser-Agent: test\r\n\r\n"

    case prologue s of
        Just (m, s) -> putStrLn s
        Nothing -> putStrLn "oops"

    putStrLn "done"
