
import Network
import System.IO
 
main = withSocketsDo $ do
    h <- connectTo "danaphalife.com" (PortNumber 80)
    hSetBuffering h LineBuffering
    hPutStr h "GET /vi HTTP/1.1\nhost: danaphalife.com\n\n"
    contents <- hGetContents h
    putStrLn contents
    hClose h

--https://wiki.haskell.org/Cookbook/Network_programming