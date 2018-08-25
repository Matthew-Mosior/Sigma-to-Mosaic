{-{-SigmatoMosaic: A File Format Converter-}-}
{-{-Author:  Matthew Mosior-}-}


{-Imports-}

import Data.Char
import Data.Function
import Data.List  
import Data.List.Split 
import Data.Maybe
import Data.Tuple
import Text.Regex
import Network.FTP.Client
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as C
import System.Directory
import System.Environment
import System.IO.Temp
import System.IO

{---------}

{-Type definitions and corresponding function definitions.-}
   
{-Pure Functions.-} 

--linefeed -> To feed file into nested list line by line, delimited by whitespace.
linefeed :: String -> [[String]]
linefeed xs = map words (lines xs)

{-General utility functions.-}

--Define isSubsetOf -> For use in significantgcf.
xs `isSubsetOf` ys = all (`elem` ys) xs

--singlenest -> For use in significantgcf.
singlenest :: [a] -> [[a]]
singlenest [] = []
singlenest xs = [xs]

--allsinglenest -> For use in nesting results of getlines in getassemblyreport.
allsinglenest :: [[a]] -> [[a]]
allsinglenest [] = [[]]
allsinglenest (xs:xss) = (map (: []) xs) ++ allsinglenest xss

--tripletfst -> For use in taxgrouper.
tripletfst :: (a,b,c) -> a
tripletfst (x,_,_) = x

--tripletthird -> For use in zeroadder.
tripletthird :: (a,b,c) -> c
tripletthird (_,_,c) = c

--quadrupletthird -> For use in sortzeroadder.
quadrupletthird :: (a,b,c,d) -> c
quadrupletthird (_,_,c,_) = c

--npletolist -> For use in alltolist.
npletolist ((a,b,c,d):xs) = a : b : c : d : npletolist xs
npletolist _ = [] 

{----------------------------------------------------------}

{-Functions to grab significant organisms relative abundances.-}

--onlyasterisk -> A test condition that looks for "*", which signifies significant relative abundances.
onlyasterisk :: [String] -> Bool
onlyasterisk xs = head xs == "*"

--relabungrabber -> To grab only lines of file that have significant relative abundances.
relabungrabber :: String -> [[String]]
relabungrabber xs = filter (onlyasterisk) (linefeed xs)

--ssrelabun -> To grab only subscripts of significant relative abundances.
ssrelabun :: String -> [String]
ssrelabun [] = []
ssrelabun xs =  concat (map (drop 1) $ map (take 2) (relabungrabber xs))

--relativeabun -> To grab actual relative abundance.
relativeabun :: String -> [String]
relativeabun [] = []
relativeabun xs = map (last) (relabungrabber xs)

{--------------------------------------------------------------}

{-Functions to grab GCFs identifiers to corresponding significant relative abundances.-}

--onlyat ->  A test condition that looks for "@", which signifies organisms identified by Sigma.
onlyat :: [String] -> Bool
onlyat xs = head xs == "@"

--gcfgrabber -> To grab only lines of file that were identified by Sigma.
gcfgrabber :: String -> [[String]]
gcfgrabber xs = filter (onlyat) (linefeed xs)

--significantgcf -> To grab only lines of identified by Sigma that are also identified to be significant.
significantgcf :: [String] -> [[String]] -> [[String]]
significantgcf [] [[]] = [[]]
significantgcf [] ys = [[]]
significantgcf xs [[]] = [[]]
significantgcf xs ys = filter ((singlenest $ head $ xs) `isSubsetOf`) ys ++ significantgcf (tail xs) ys

--siggcfonly -> To grab just the third element of significantgcf (The part used to download the assembly reports).
siggcfonly :: [[String]] -> [String]
siggcfonly [[]] = []
siggcfonly xs = (take 1 $ drop 2 $ head $ xs) ++ siggcfonly (tail xs)

{--------------------------------------------------------------------------------------}

{-Functions used to download assembly report files based on GCF identifier, and parse out TaxId from assembly report.-}

--largeparser -> To prepare the result of siggcfonly for downloading.
largeparser :: [String] -> [String]
largeparser [] = []
largeparser xs = (singlenest $ (subRegex (mkRegex "_genomic.fna_directory") (head xs) "/")) ++ largeparser (tail xs)

--firstparser -> To prepare the result of siggcfonly for downloading.
firstparser :: [String] -> [String]
firstparser [] = [] 
firstparser xs = (singlenest $ take 3 $ (subRegex (mkRegex "GCF_|\\..*") (head xs) "")) ++ firstparser (tail xs)

--finalfirstparser -> To prepare the result of siggcfonly for downloading.
finalfirstparser :: [String] -> [String]
finalfirstparser [] = [] 
finalfirstparser xs = map (++ "/") (firstparser xs)

--secondparser -> To prepare the result of siggcfonly for downloading.
secondparser :: [String] -> [String]
secondparser [] = []
secondparser xs = (singlenest $ drop 3 $ take 6 $ (subRegex (mkRegex "GCF_|\\..*") (head xs) "")) ++ secondparser (tail xs)

--finalsecondparser -> To prepare the result of siggcfonly for downloading.
finalsecondparser :: [String] -> [String]
finalsecondparser [] = []
finalsecondparser xs = map (++ "/") (secondparser xs)

--thirdparser -> To prepare the result of siggcfonly for downloading.
thirdparser :: [String] -> [String]
thirdparser [] = []
thirdparser xs = (singlenest $ drop 6 $ take 9 $ (subRegex (mkRegex "GCF_|\\..*") (head xs) "")) ++ thirdparser (tail xs)

--finalthirdparser -> To prepare the result of siggcfonly for downloading.
finalthirdparser :: [String] -> [String]
finalthirdparser [] = []
finalthirdparser xs = map (++ "/") (thirdparser xs)

--preparser -> To prepare the result of siggcfonly for downloading.
preparser :: [String] -> [String]
preparser [] = []
preparser xs = (singlenest $ (subRegex (mkRegex "^.*") (head xs) "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/")) ++ preparser (tail xs)

--postparser -> To prepare the result of siggcfonly for downloading.
postparser :: [String] -> [String]
postparser [] = []
postparser xs = (singlenest $ (subRegex (mkRegex "$") (head xs) "_assembly_report.txt")) ++ postparser (tail xs)

--prefinalparser -> To prepare the result of siggcfonly for downloading.
prefinalparser :: [String] -> [String]
prefinalparser [] = []
prefinalparser xs = zipWith6 (\a b c d e f -> a ++ b ++ c ++ d ++ e ++ f) (preparser xs) (finalfirstparser xs) (finalsecondparser xs) (finalthirdparser xs) (largeparser xs) (postparser xs)

--postfinalparser -> To preset the url to download the assembly report
postfinalparser :: [String] -> [String]
postfinalparser [] = []
postfinalparser xs = (singlenest $ (subRegex (mkRegex "_genomic.fna_directory") (head xs) "")) ++ postfinalparser (tail xs)

--assemblyreport -> Actual URLs once logged into ftp.ncbi.nih.gov to download assembly reports.
assemblyreport :: [String] -> [String]
assemblyreport [] = []
assemblyreport xs = (singlenest $ (subRegex (mkRegex "ftp://ftp.ncbi.nlm.nih.gov/") (head xs) "")) ++ assemblyreport (tail xs)

--grabtaxid -> To parse out taxid from downloaded assembly report.
grabtaxid :: [String] -> [String]
grabtaxid [] = []
grabtaxid xs = filter (isPrefixOf "# Taxid:") xs

--onlytaxid -> To parse out "# Taxid:" from grabtaxid. 
onlytaxid :: [String] -> [String]
onlytaxid [] = []
onlytaxid xs = (singlenest $ (subRegex (mkRegex "# Taxid:          ") (head xs) "")) ++ onlytaxid (tail xs)

{---------------------------------------------------------------------------------------------------------------------}

{-Functions to prepare for the efetch URL downloading.-}

--efetchbeginning -> To correctly set up the url for downloading.
efetchbeginning :: [String] -> [String]
efetchbeginning [] = []
efetchbeginning xs = (singlenest $ (subRegex (mkRegex "^") (head xs) "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=")) ++ efetchbeginning (tail xs)

--efetchend -> To correctly set up the url for downloading.
efetchend :: [String] -> [String]
efetchend [] = []
efetchend xs = (singlenest $ (subRegex (mkRegex "$") (head xs) "&mode=text&report=xml")) ++ efetchend (tail xs)

{------------------------------------------------------}

{-Functions to parse the output of the efetch url download (efetchdata).-}

--efetchdatatostring -> To grab the full taxnomic rank of the present organism (efetchdata).
efetchdatatostring :: C.ByteString -> String
efetchdatatostring xs = C.unpack xs

--wordsefetchdatatostring -> To parse String to [String] by words.
wordsefetchdatatostring :: C.ByteString -> [String]
wordsefetchdatatostring xs = splitOneOf "\n" $ (efetchdatatostring xs)

--nowhitespaceefetchdatatostring -> To remove whitespace from each string.
nowhitespaceefetchdatatostring :: C.ByteString -> [String]
nowhitespaceefetchdatatostring xs = map (dropWhile isSpace) $ (wordsefetchdatatostring xs)

--filterefetchdatatostring -> To keep only words that fit the predicate.
filterefetchdatatostring :: C.ByteString -> [String]
filterefetchdatatostring xs = filter (\x -> isPrefixOf "<TaxId>" x || isPrefixOf "<Rank>" x) $ (nowhitespaceefetchdatatostring xs)

--twochunkefetchdatatostring -> To merge every two elements of filterefetchdatatostring.
twochunkefetchdatatostring :: C.ByteString -> [[String]]
twochunkefetchdatatostring xs = chunksOf 2 $ (filterefetchdatatostring xs)

--grabtaxrank -> To grab lists of TaxId and Rank that only contain correct classification (see where statement).
grabtaxrank :: C.ByteString -> [[String]]
grabtaxrank xs = filter predicate (twochunkefetchdatatostring xs)
                   where predicate = (any (\x -> isInfixOf "superkingdom" x || isInfixOf "phylum" x || isInfixOf "class" x || isInfixOf "order" x || isInfixOf "family" x || isInfixOf "genus" x || isInfixOf "species" x))

--removesubspecies -> To remove subspecies from grabtaxrank.
removesubspecies :: C.ByteString -> [[String]]
removesubspecies xs = filter predicate (grabtaxrank xs) 
                        where predicate = ((not) . any (\x -> isInfixOf "subspecies" x))

--sorttaxrank -> To sort grabtaxrank into order : Superkingdom, phylum, class, order, family, genus, species.
sorttaxrank :: C.ByteString -> [[String]]
sorttaxrank xs = concatMap (\x -> filter (any (\y -> isInfixOf x y)) (removesubspecies xs)) order
                   where order = ["superkingdom" , "phylum" , "class" , "order" , "family" , "genus" , "species"]

--finaltaxrank -> To grab just the actual TaxIds of each sublist within sorttaxrank.
finaltaxrank :: C.ByteString -> [String]
finaltaxrank xs = map (head) (sorttaxrank xs)

--finaltaxrankparser -> To parse out the beginning tags of finaltaxrank.
finaltaxrankparser :: C.ByteString -> [String]
finaltaxrankparser xs = map (filter (not . (`elem` "<>TaxId/"))) (finaltaxrank xs)  

--printtaxrank -> To merge each string into one separate with commas.
printtaxrank :: C.ByteString -> [String]
printtaxrank xs = singlenest (intercalate "," (finaltaxrankparser xs))

{------------------------------------------------------------------------}

{-Functions to Prepare the Command Line Arguments.-}

--filenameparser -> To parse out the full filepath down to just the filename.
filenameparser :: String -> String
filenameparser [] = [] 
filenameparser xs = subRegex (mkRegex "^.*/") xs ""

--allfilenameparser -> To parse out the full filepath down to just the filename for all command line arguments.
allfilenameparser :: [String] -> [String]
allfilenameparser [] = [] 
allfilenameparser xs = (singlenest $ (subRegex (mkRegex "^.*/") (head xs) "")) ++ (allfilenameparser (tail xs))

{--------------------------------------------------}

{-Final Transformations.-}
{-Functions to group all lines from all four files by taxonomic rank.-}

--linecollector -> To collect all lines into new [String].
linecollector :: String -> [[String]]
linecollector [] = [[]]
linecollector xs = linefeed xs

--linestotriplets -> To collect each list to a triplet.
linestotriplets :: String -> [(String,String,String)]
linestotriplets [] = []
linestotriplets xs = map (\[a,b,c] -> (a,b,c)) (linecollector xs)

--sortlinestotriplets -> To sort each triplet.
sortlinestotriplets :: String -> [(String,String,String)]
sortlinestotriplets [] = [] 
sortlinestotriplets xs = sortBy (\(x,_,_) (y,_,_) -> compare x y) (linestotriplets xs)

--taxgrouper -> To group the nested lists based on taxonomic rank (heads of each sublist).
taxgrouper :: String -> [[(String,String,String)]]
taxgrouper [] = []
taxgrouper xs = groupBy (\x y -> tripletfst x == tripletfst y) (sortlinestotriplets xs)

--zeroadder -> To add zeros to each sublist that doesn't have identification from certain file.
zeroadder :: [[(String,String,String)]] -> [String] -> [[(String,String,String)]]
zeroadder [[]]    [] = [[]] 
zeroadder [[]]    ys = [[]]
zeroadder (x:xs)  [] = [[]]
zeroadder []   (_:_) = [[]]
zeroadder []      [] = [[]]
zeroadder (x:xs) cmdargss = (singlenest $ (x ++ (zip3 (replicate (length $ (cmdargss \\ (map (tripletthird) x))) "NULL") (replicate (length $ (cmdargss \\ (map (tripletthird) x))) "0.00000000000000000000") (cmdargss \\ (map (tripletthird) x))))) ++ (zeroadder xs cmdargss)  

--taxadder -> To add a the full taxonomic rank to all tuples in all sublists.
taxadder :: [[(String,String,String)]] -> [[(String,String,String,String)]]
taxadder [[]] = [[]]
taxadder (x:xs) = (singlenest $ (map (\(a,b,c) -> (a,b,c,head (sort (map (tripletfst) x)))) x)) ++ (taxadder xs) 

--sortzeroadder -> To sort the output of zeroadder.
sorttaxadder :: [[(String,String,String,String)]] -> [[(String,String,String,String)]]
sorttaxadder [[]] = [[]]
sorttaxadder xs = map (sortOn quadrupletthird) xs

--alltolist -> To convert n-tuples to list of lists.
alltolist :: [[(String,String,String,String)]] -> [[String]]
alltolist [[]] = [[]]
alltolist xs = map (npletolist) xs

--specificgrablist -> To grab out the elements to be in the final list.
specificgrablist :: [[String]] -> [[String]]
specificgrablist [[]] = [[]]
specificgrablist [] = [[]]
specificgrablist (x:xs) = (singlenest $ (x !! 3 : x !! 1 : x !! 5 : x !! 9 : x !! 13 : [])) ++ (specificgrablist xs)

--finalprintlist -> Put a header at the beginning of specificgrablist.
finalprintlist :: [[String]] -> [String]  -> [[String]]
finalprintlist [[]] []     = [[]]
finalprintlist [[]] ys     = [[]]
finalprintlist xs   []     = [[]]
finalprintlist [] (_:_)    = [[]]
finalprintlist xs cmdargss = ("FullTaxonomicRank" : (head cmdargss) : (head $ (tail cmdargss)) : (last $ (take 3 cmdargss)) : (last cmdargss) : []) : xs 
 
{--------------------------------------------------------------------}
{------------------------}

{-----------------}

{-IO Function.-}
{-Function that Download Files and Grab specific information.-}

--run -> Function to download, grab data from assembly reports, and add to temp file.
run :: [String] -> [String] -> String -> (FilePath,Handle) -> IO [[String]]
run [] [] [] temp = return []
run xs [] [] temp = return []
run [] ys [] temp = return []
run [] [] zs temp = return []
run xs ys [] temp = return []
run [] ys zs temp = return []
run xs [] zs temp = return []
run (x:xs) (y:ys) zs (tempnamed,temphd) = do
    --Login to the NCBI FTP Server and download assembly report files.
    handle <- easyConnectFTP "ftp.ncbi.nih.gov"
    loginAnon handle
    preassemblydata <- getlines handle x
    let assemblydata = fst preassemblydata
    --Parse out Taxid from assembly report.
    let parsedassemblydata = grabtaxid assemblydata
    let taxid = onlytaxid parsedassemblydata
    --Set the corresponding efetch URL of the Taxid just parsed above (taxid).
    let efetch = efetchend $ (efetchbeginning taxid)  
    --Use simplehttp function in Network.HTTP.Conduit to return Bytestring of each efetch URL.
    efetchdata <- simpleHttp $ (unwords efetch)
    --Parse efetchdata and grab TaxId if it belongs to major Taxonomic Rank.
    let fullrank = printtaxrank efetchdata
    --Add Current relative abundance to end of fullrank.
    let finaldata = fullrank ++ (singlenest y) ++ (singlenest zs)
    --Create intercalate finaldata into one string with spaces.
    let truefinaldata = intercalate " " finaldata
    --Add finaldata to temp.txt.
    hPutStrLn temphd truefinaldata
    --Peform the above steps on next assembly report.
    run xs ys zs (tempnamed,temphd)

{-------------------------------------------------------------}
{---------------}

{-Main Function.-}

main :: IO ()
main = do
    --Get Command line arguments.
    cmdargs <- getArgs
    case cmdargs of
        []                    -> error "Please supply four filepaths."
        [arg1,arg2,arg3,arg4] -> do  --Create Temporary file.
                                     (tempfile , temph) <- openTempFile "." "temp.txt"
                                     --Work on first argument.
                                     inputfile1 <- readFile arg1
                                     taxonomicranks1 <- run (assemblyreport $ postfinalparser $ prefinalparser $ siggcfonly $ (significantgcf (ssrelabun inputfile1) (gcfgrabber inputfile1))) (relativeabun inputfile1) (filenameparser arg1) (tempfile,temph)
                                     --Work on second argument.
                                     inputfile2 <- readFile arg2
                                     taxonomicranks2 <- run (assemblyreport $ postfinalparser $ prefinalparser $ siggcfonly $ (significantgcf (ssrelabun inputfile2) (gcfgrabber inputfile2))) (relativeabun inputfile2) (filenameparser arg2) (tempfile,temph)
                                     --Work on third argument.
                                     inputfile3 <- readFile arg3
                                     taxonomicranks3 <- run (assemblyreport $ postfinalparser $ prefinalparser $ siggcfonly $ (significantgcf (ssrelabun inputfile3) (gcfgrabber inputfile3))) (relativeabun inputfile3) (filenameparser arg3) (tempfile,temph)
                                     --Work on fourth argument.
                                     inputfile4 <- readFile arg4
                                     taxonomicranks4 <- run (assemblyreport $ postfinalparser $ prefinalparser $ siggcfonly $ (significantgcf (ssrelabun inputfile4) (gcfgrabber inputfile4))) (relativeabun inputfile4) (filenameparser arg4) (tempfile,temph)
                                     --Seek to beginning of temp file.
                                     hSeek temph AbsoluteSeek 0
                                     --Read temp file into [[String]].
                                     fulltempread <- hGetContents temph
                                     --Get final print ready nested list by applying all functions in Final Transformation section to fulltempread.
                                     let final = finalprintlist (specificgrablist (alltolist (sorttaxadder (taxadder (zeroadder (taxgrouper fulltempread) (allfilenameparser cmdargs)))))) (allfilenameparser cmdargs)
                                     --Print to Mosaic.txt. 
                                     writeFile "Mosaic.txt" $ (intercalate "\n" (map (intercalate "\t") final) ++ "\n") 
                                     --Close temporary file.
                                     hClose temph
                                     --Delete temporary file.
                                     removeFile tempfile
        _                     -> error "Please supply four filepaths."
 
{----------------}

{-{-------------------------}-}
{-{----------------------------------------}-}
