{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

{-
Naive Bayes classifier

runghc genderguess train -t male_list -g male
runghc genderguess train -t female_list -g female
runghc genderguess classify -n 姓名
-}

import Control.Monad
import Data.Binary
import Data.Maybe
import Data.Data
import qualified Data.Map as M
import qualified System.Console.CmdArgs as Arg
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import System.Console.CmdArgs((+=),Annotate((:=)),(&=))
import System.Directory
import Debug.Trace

data Mode = Train { trainFile :: FilePath, gender :: String }
          | Classify { name :: String } deriving (Eq, Show, Typeable, Data)

trainMode = Train { trainFile = Arg.def &= Arg.help "training file", gender = "male" &= Arg.help "gender" }
classifyMode = Classify { name = Arg.def &= Arg.help "classify name" }

modes = Arg.modes [trainMode, classifyMode]
  &= Arg.program "gender-guess"
  &= Arg.summary "guess gender"
  &= Arg.help "meow"

dispatch :: Mode -> IO ()
dispatch (Train file gender) = train file gender
dispatch (Classify n) = classify n

loadOrNew :: FilePath -> IO (M.Map BL.ByteString Int, M.Map BL.ByteString Int)
loadOrNew file = do
  flag <- doesFileExist file
  if flag
     then decodeFile file
     else return (M.empty, M.empty)

train :: FilePath -> String -> IO ()
train trainfile gender = f 1 >> f 2
  where
    f len = do
      let file = "data" ++ show len
      contents <- BL.readFile trainfile
      (maleF, femaleF) <- loadOrNew file
      let dat = M.unionsWith (+) $ map (flip M.singleton 1 . BL.fromString . reverse . take len . reverse . BL.toString) (BL.lines contents)
      if gender == "male" || gender == "1"
        then encodeFile file (M.unionWith (+) maleF dat, femaleF)
        else encodeFile file (maleF, M.unionWith (+) femaleF dat)

classify :: String -> IO ()
classify name = do
  p1 <- f 1
  p2 <- f 2
  let p_male = fst p1 + fst p2 * 5
      p_female = snd p1 + snd p2 * 5
  if p_male >= p_female
    then putStrLn "male"
    else putStrLn "female"
  where
    f len = do
      let file = "data" ++ show len
      features <- loadOrNew file
      return $ calculate features (reverse . take len $ reverse name)

calculate (maleF, femaleF) name = (p_f_male / p_f, p_f_female / p_f)
  where
    c_male = fromMaybe 0 (M.lookup (BL.fromString name) maleF)
    c_female = fromMaybe 0 (M.lookup (BL.fromString name) femaleF)
    tot_male = sum $ M.elems maleF
    tot_female = sum $ M.elems femaleF
    p_f = if tot_male + tot_female == 0 then 0 else fromIntegral (c_male + c_female) / fromIntegral (tot_male + tot_female)
    p_f_male = if tot_male + tot_female == 0 then 0 else fromIntegral c_male / fromIntegral (tot_male + tot_female)
    p_f_female = if tot_male + tot_female == 0 then 0 else fromIntegral c_female / fromIntegral (tot_male + tot_female)

main = Arg.cmdArgs modes >>= dispatch
