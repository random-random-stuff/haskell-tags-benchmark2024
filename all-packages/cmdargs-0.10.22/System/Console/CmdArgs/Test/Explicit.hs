
module System.Console.CmdArgs.Test.Explicit(test, demo) where

import System.Console.CmdArgs.Default
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util


demo = [newDemo act dem]

act xs | ("help","") `elem` xs = print $ helpText [] def dem
       | otherwise = print xs

dem :: Mode [(String,String)]
dem = mode "explicit" [] "Explicit sample program" (flagArg (upd "file") "FILE")
      [flagOpt "world" ["hello","h"] (upd "world") "WHO" "World argument"
      ,flagReq ["greeting","g"] (upd "greeting") "MSG" "Greeting to give"
      ,flagHelpSimple (("help",""):)
      ]
    where upd msg x v = Right $ (msg,x):v


test :: IO ()
test = do
    testUnnamedOnly
    testFlags
    testModes

testUnnamedOnly = do
    let m = name "UnnamedOnly" $ mode "" [] "" (flagArg (upd "") "") []
    checkFail m ["-f"]
    checkFail m ["--test"]
    checkGood m ["fred","bob"] ["fred","bob"]
    checkGood m ["--","--test"] ["--test"]
    checkGood m [] []
    checkComp m [] (0,0) []
    checkComp m ["--"] (0,2) []
    checkComp m ["bob"] (0,3) []
    checkComp m ["-"] (0,1) [CompleteValue "-"]

testFlags = do
    let m = name "Flags" $ mode "" [] "" (flagArg (upd "") "")
                [flagNone ["test","t"] ("test":) ""
                ,flagNone ["more","m"] ("more":) ""
                ,flagReq ["color","colour","bobby"] (upd "color") "" ""
                ,flagOpt "" ["bob","z"] (upd "bob") "" ""
                ,flagBool ["x","xxx"] (upb "xxx") ""]
    checkFail m ["-q"]
    checkGood m ["--test"] ["test"]
    checkGood m ["-t"] ["test"]
    checkFail m ["-t="]
    checkFail m ["--test=value"]
    checkFail m ["--bo"]
    checkGood m ["--bobb=r"] ["colorr"]
    checkGood m ["--bob"] ["bob"]
    checkGood m ["--bob=foo"] ["bobfoo"]
    checkGood m ["--bob","foo"] ["bob","foo"]
    checkGood m ["-zfoo"] ["bobfoo"]
    checkGood m ["-z=foo"] ["bobfoo"]
    checkGood m ["-z","foo"] ["bob","foo"]
    checkGood m ["--mo"] ["more"]
    checkGood m ["-tm"] ["test","more"]
    checkGood m ["--col=red"] ["colorred"]
    checkGood m ["--col","red","-t"] ["colorred","test"]
    checkComp m ["--tes"] (0,5) [CompleteValue "--test"]
    checkComp m ["--color","--tes"] (1,5) []
    checkComp m ["--more","--tes"] (1,5) [CompleteValue "--test"]
    checkComp m ["--moo","--tes"] (1,5) [CompleteValue "--test"]
    checkComp m ["--col"] (0,5) [CompleteValue "--color"]
    checkComp m ["--bob"] (0,5) [CompleteValue "--bobby",CompleteValue "--bob"]
    checkComp m ["-"] (0,1) $ map CompleteValue $ words "--test --more --color --bob -x -"
    checkComp m ["--"] (0,2) $ map CompleteValue $ words "--test --more --color --bob --xxx"

testModes = do
    let m = name "Modes" $ modes "" [] ""
                [(mode "test" ["test"] "" undefined [flagNone ["bob"] ("bob":) ""]){modeArgs=([],Nothing)}
                ,mode "dist" ["dist"] "" (flagArg (upd "") "") [flagNone ["bob"] ("bob":) "", flagReq ["bill"] (upd "bill") "" ""]]
    checkGood m [] []
    checkFail m ["--bob"]
    checkFail m ["tess"]
    checkFail m ["test","arg"]
    checkGood m ["test","--b"] ["test","bob"]
    checkGood m ["t","--bo"] ["test","bob"]
    checkGood m ["dist","--bob"] ["dist","bob"]
    checkFail m ["dist","--bill"]
    checkGood m ["dist","--bill","foo"] ["dist","billfoo"]


---------------------------------------------------------------------
-- UTILITIES

upd pre s x = Right $ (pre++s):x
upb pre s x = (pre ++ show s):x
name x y = ("Explicit " ++ x, y)

checkFail :: (String,Mode [String]) -> [String] -> IO ()
checkFail (n,m) xs = case process m xs of
    Right a -> failure "Succeeded when should have failed" [("Name",n),("Args",show xs),("Result",show a)]
    Left a -> length (show a) `hpc` success

checkGood :: (String,Mode [String]) -> [String] -> [String] -> IO ()
checkGood (n,m) xs ys = case process m xs of
    Left err -> failure "Failed when should have succeeded" [("Name",n),("Args",show xs),("Error",err)]
    Right a | reverse a /= ys -> failure "Wrong parse" [("Name",n),("Args",show xs),("Wanted",show ys),("Got",show $ reverse a)]
    _ -> success

checkComp :: (String,Mode [String]) -> [String] -> (Int,Int) -> [Complete] -> IO ()
checkComp (n,m) xs ab want
    | want == got = success
    | otherwise = failure "Bad completions" [("Name",n),("Args",show xs),("Index",show ab),("Wanted",show want),("Got",show got)]
    where got = complete m xs ab
