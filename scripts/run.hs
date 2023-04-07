#!/usr/bin/env stack
-- stack --system-ghc --resolver lts-18.13 script --package shake --package directory --optimize export/export.hs
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List
import Text.Printf
import Control.Monad
import System.Console.GetOpt
import System.IO
import System.Directory as D
import Debug.Trace

--
-- Hardcoded defaults, overridable via commandline options:
-- 1. Range of years to produce reports for. You would
--    typically want all the years  you have data for.
-- 2. Location of your journal files (path, relative to your export directory)
-- 3. Name of the hledger binary
-- 4. Which accounts to include into opening/closing balances
--
defaultFirstYear     = 1970 :: Int
defaultCurrentYear   = 1971
defaultBaseDir       = "."
defaultHledgerBinary = "hledger"
defaultEquityQuery   = "assets|liabilities|debts"
--
-- Input file naming scheme
--
input base year = base </> (year ++ ".journal")

--
-- Output file naming scheme.
-- It assumes that you do not have similarly-named journals anywhere among files included
-- from you yearly journals
--
transactions      y = y++"-all.journal"
income_expenses   y = y++"-income-expenses.txt"
balance_sheet     y = y++"-balance-sheet.txt"
cash_flow         y = y++"-cash-flow.txt"
accounts          y = y++"-accounts.txt"
unknown           y = y++"-unknown.journal"
closing_balances  y = y++"-closing.journal"
opening_balances  y = y++"-opening.journal"

--
-- Defining the full set of reports and journals to be generated
--
reports first current =
  concat [ [ defaultBaseDir </> "outputs" </> "reports" </> transactions         (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> accounts             (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> income_expenses      (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> balance_sheet        (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> cash_flow            (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> unknown              (show y) | y <- all_years ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> opening_balances     (show y) | y <- all_years, y/=first ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> closing_balances     (show y) | y <- all_years, y/=current ]
         , [ defaultBaseDir </> "outputs" </> "reports" </> "accounts-all.txt" ]
         , [ defaultBaseDir </> "outputs" </> "charts" </> "cashflow.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "income-expenses.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-food.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-groceries.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-takeout.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-obligations.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-services.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-expenses.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-goals.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "monthly-income.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "histogram-income-all.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "histogram-expenses.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "histogram-expenses-1970.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "histogram-expenses-1971.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "megaexpenses.svg",
             defaultBaseDir </> "outputs" </> "charts" </> "wealth.svg"
           ]
         , [ defaultBaseDir </> "outputs" </> "charts" </> "histogram-expenses-1970-" ++ y ++ ".svg" | y <- all_months ]
         , [ defaultBaseDir </> "outputs" </> "charts" </> "histogram-expenses-1971-" ++ y ++ ".svg" | y <- all_months ]
         ]
  where
    all_years=[first..current]
    all_months=["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

-----------------------------------------
-- Extra dependencies of the import files
-----------------------------------------
extraDeps flags source file
  | "//bank1//*.journal" ?== file = [ "../../../" </> "outputs" </> "sources" </> source </> "rules.rules",
                                      "../../../" </> "outputs" </> "sources" </> source </> "generated.rules"]

  | "//bank2//*.journal" ?== file = [ "../../../" </> "outputs" </> "sources" </> source </> "rules.rules",
                                      "../../../" </> "outputs" </> "sources" </> source </> "generated.rules"]

  | "//*.svg" ?== file = [ "outputs" </> "reports" </> show y ++ "-all.journal" | y <- [defaultFirstYear..defaultCurrentYear] ]

  | "//accounts-all.txt" ?== file = [ "outputs" </> "reports" </> show y ++ "-accounts.txt" | y <- [defaultFirstYear..defaultCurrentYear] ]
    ++ ["outputs" </> "reports" </> closing_balances (show y) | y <- [defaultFirstYear..defaultCurrentYear], y/=defaultCurrentYear]

  | otherwise = []


-----------------------------------------------
-- Extra inputs to be fed to conversion scripts
-----------------------------------------------
extraInputs file = []

--
-- Command line flags
--
data Flags =
  Flags { firstYear     :: Int
        , currentYear   :: Int
        , baseDir       :: String
        , hledgerBinary :: String
        , equityQuery   :: String
        } deriving Eq

setFirstYear y flags   = flags{firstYear = read y}
setCurrentYear y flags = flags{currentYear = read y}
setBaseDir d flags     = flags{baseDir = d}
setBinary b flags      = flags{hledgerBinary = b}
setEquityQuery q flags = flags{equityQuery = q}

flags =
  [ Option "" ["first"] (ReqArg (Right . setFirstYear) "YEAR") ("Override current year. Defaults to " ++ show defaultFirstYear)
  , Option "" ["current"] (ReqArg (Right . setCurrentYear) "YEAR") ("Override current year. Defaults to " ++ show defaultCurrentYear)
  , Option "" ["base"] (ReqArg (Right . setBaseDir) "DIR") ("Override the relative location of journal files. Defaults to " ++ show defaultBaseDir)
  , Option "" ["hledger"] (ReqArg (Right . setBinary) "PATH") ("Use this hledger executable. Defaults to " ++ show defaultHledgerBinary)
  , Option "" ["equity"] (ReqArg (Right . setEquityQuery) "QUERY") ("Use this query string to generate opening-closing balances. Defaults to " ++ show defaultEquityQuery)
  ]

main = do
  let defaults = Flags { firstYear = defaultFirstYear, currentYear = defaultCurrentYear, baseDir = defaultBaseDir, hledgerBinary = defaultHledgerBinary, equityQuery = defaultEquityQuery }
  shakeArgsAccumulate shakeOptions flags defaults export_all

-- Build rules
export_all flags targets = return $ Just $ do
  let first = firstYear flags
      current = currentYear flags

  if null targets then want (reports first current) else want targets

  -- Discover and cache the list of all includes for the given .journal file, recursively
  year_inputs <- newCache $ \year -> do
    let file = input ((baseDir flags) </> "targets/") year
    getIncludes ((baseDir flags) </> "targets/") file -- file itself will be included here

  (transactions "//*") %> hledger_process_year flags year_inputs ["print"]

  (accounts "//*") %> hledger_process_year flags year_inputs ["accounts"]

  (income_expenses "//*") %> hledger_process_year flags year_inputs ["is","--flat","--no-elide"]

  (balance_sheet "//*") %> hledger_process_year flags year_inputs ["balancesheet","--no-elide"]

  (cash_flow "//*") %> hledger_process_year flags year_inputs ["cashflow","not:desc:(opening balances)","--no-elide"]

  (unknown "//*") %> hledger_process_year flags year_inputs ["print", "unknown"]

  (closing_balances "//*") %> generate_closing_balances flags year_inputs

  (opening_balances "//*") %> generate_opening_balances flags year_inputs

  -- Enumerate directories with auto-generated cleaned csv files
  [ "//outputs/sources/bank1/csv/*.csv" ] |%> in2csv flags

  -- Enumerate directories with auto-generated journals
  [ "//outputs/sources/bank1/journal/*.journal",
    "//outputs/sources/bank2/journal/*.journal" ] |%> csv2journal flags

  -- Enumerate directories with valid journals
  [ "//outputs/sources/cash/*.journal", "//outputs/sources/budget/*.journal" ] |%> copy_journal flags

  -- Enumerate directories with valid csv
  [ "//outputs/sources/bank2/csv/*.csv" ] |%> copy_csv flags

  -- Whenever we need generated.rules, produce them from rules.psv
  "//outputs//generated.rules" %> generated_rules flags

  -- Whenever we need rules.rules, copy them from inputs
  "//outputs//rules.rules" %> copy_rules flags

  -- Whenever we need rules.rules, copy them from inputs
  "//outputs//rules/*.rules" %> copy_rules_dir flags

  -- charts
  -- "//charts//cashflow.svg" %> report
  "//*cashflow.svg" %> chart flags "cashflow.sh" [ "-X", "USD" ]
  "//*income-expenses.svg" %> chart flags "income-expenses.sh" [ "-X", "USD" ]

  "//*monthly-groceries.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:obligations:groceries" ]
  "//*monthly-takeout.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:services:takeout" ]
  "//*monthly-food.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:services:takeout|expenses:obligations:groceries" ]
  "//*monthly-obligations.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:obligations" ]
  "//*monthly-services.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:services" ]
  "//*monthly-expenses.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses" ]
  "//*monthly-goals.svg" %> chart flags "monthly.sh" [ "-X", "USD", "expenses:goals" ]
  "//*monthly-income.svg" %> chart flags "monthly.sh" [ "-X", "USD", "income" ]

  "//*histogram-income-all.svg" %> chart flags "histogram.sh" [ "-X", "USD", "income" ]
  "//*histogram-expenses.svg" %> chart flags "histogram.sh" [ "-X", "USD", "expenses" ]
  "//*histogram-expenses-1970.svg" %> chart flags "histogram.sh" [ "-X", "USD", "expenses", "-p", "1970"]
  "//*histogram-expenses-1970-*.svg" %> histogram flags [ "-X", "USD", "expenses" ]
  "//*histogram-expenses-1971.svg" %> chart flags "histogram.sh" [ "-X", "USD", "expenses", "-p", "1971"]
  "//*histogram-expenses-1971-*.svg" %> histogram flags [ "-X", "USD", "expenses" ]

  "//*megaexpenses.svg" %> chart flags "megaexpenses.sh" [ "-X", "USD" ]
  "//*wealth.svg" %> chart flags "wealth.sh" [ "-X", "USD" ]

  -- concat all account files
  "//*accounts-all.txt" %> accounts_all flags

-------------------------------------
-- Implementations of the build rules
-------------------------------------

-- Run hledger command on a given yearly file. Year is extracted from output file name.
-- To generate '2017-balances', we will process '2017.journal'
hledger_process_year flags year_inputs args out = do
  let year = head $ split out
  deps <- year_inputs year
  need deps
  (Stdout output) <- cmd (hledgerBinary flags) ("-f" : input ((baseDir flags) </> "targets") year : args)
  writeFileChanged out output

generate_opening_balances flags year_inputs out = do
  let year = head $ split out
  let prev_year = show ((read year)-1)
  deps <- year_inputs prev_year
  need deps
  (Stdout output) <-
    cmd (hledgerBinary flags)
    ["-f",input ((baseDir flags) </> "targets") prev_year,"equity",equityQuery flags,"-e",year,"--opening"]
  writeFileChanged out output

generate_closing_balances flags year_inputs out = do
  let year = head $ split out
  hledger_process_year flags year_inputs ["equity",equityQuery flags,"-e",show (1+(read year)),"-I","--closing"] out

in2csv flags out = do
  let (csv_dir, file) = splitFileName out
  let source_dir = parentOf "csv" csv_dir
  let source = takeBaseName source_dir
  let in_dir = (baseDir flags) </> "inputs" </> source </> "in"
  possibleInputs <- getDirectoryFiles in_dir [file -<.> "*"]
  let inputs =
        case possibleInputs of
          [] -> error $ "no inputs for " ++ show file
          _ -> map (in_dir </>) $ possibleInputs ++ (extraInputs file)
  let deps = map (source_dir </>) $ extraDeps flags source out
  let script = "scripts" </> "in2csv" </> (takeBaseName source_dir) </> "in2csv"
  need $ ((baseDir flags) </> script):(inputs ++ deps)
  (Stdout output) <- cmd Shell script inputs
  writeFileChanged out output

-- To produce <importdir>/journal/filename.journal, look for <importdir>/csv/filename.csv and
-- process it with <importdir>/csv2journal
csv2journal flags out = do
  let (journal_dir, file) = splitFileName $ trace ("csv2journal " ++ out) out
  let source_dir = parentOf "journal" journal_dir
  let source = takeBaseName source_dir
  let outputs_dir = (baseDir flags) </> "outputs" </> "sources" </> source
  let csv = outputs_dir </> "csv" </> (file -<.> "csv")
  let rules = outputs_dir </> "rules.rules"
  let deps = map (source_dir </>) $ extraDeps flags source out
  need $ rules:(csv:deps)
  (Stdout output) <- cmd Shell "hledger" ["print", "--rules-file", rules, "-f", csv]
  writeFileChanged out output

copy_journal flags out = do
  let (journal_dir, file) = splitFileName out
  let source_dir = parentOf "journal" journal_dir
  let source = takeBaseName source_dir
  let input_dir = (baseDir flags) </> "inputs" </> source
  let input = input_dir </> file
  let deps = map (source_dir </>) $ extraDeps flags source out
  need $ (input:deps)
  (Stdout output) <- cmd Shell "cat" input
  writeFileChanged out output

copy_csv flags out = do
  let (journal_dir, file) = splitFileName out
  let source_dir = parentOf "csv" journal_dir
  let source = takeBaseName source_dir
  let input_dir = (baseDir flags) </> "inputs" </> source </> "in"
  let input = input_dir </> file
  let deps = map (source_dir </>) $ extraDeps flags source out
  need $ (input:deps)
  (Stdout output) <- cmd Shell "cat" input
  writeFileChanged out output

generated_rules flags out = do
  let (dir, file) = splitFileName out
  let source = last $ splitDirectories dir
  let script = "psv2rules.awk"
  let scriptDir = "scripts"
  let rules = (baseDir flags) </> "inputs" </> source </> "rules.psv"
  need [ rules, scriptDir </> script ]
  (Stdout output) <- cmd "awk" ["-F", "|", "-f", scriptDir </> script, rules]
  writeFileChanged out output

copy_rules flags out = do
  let (dir, file) = splitFileName out
  let source_dir = last $ splitDirectories dir
  let source = takeBaseName source_dir
  let input_dir = (baseDir flags) </> "inputs" </> source
  let input = input_dir </> file
  let deps = map (source_dir </>) $ extraDeps flags source out
  need $ (input:deps)
  (Stdout output) <- cmd Shell "cat" input
  writeFileChanged out output

copy_rules_dir flags out = do
  let (dir, file) = splitFileName out
  let source_dir = parentOf "rules" dir
  let source = takeBaseName source_dir
  let input_dir = (baseDir flags) </> "inputs" </> source </> "rules"
  let input = input_dir </> file
  let deps = map (source_dir </>) $ extraDeps flags source out
  need $ (input:deps)
  (Stdout output) <- cmd Shell "cat" input
  writeFileChanged out output

chart flags script args out = do
  let scriptDir = (baseDir flags) </> "scripts" </> "journal2chart"
  need $ [ scriptDir </> script ] ++ extraDeps flags "" out
  (Stdout output) <- cmd "sh" $ [ scriptDir </> script ] ++ args
  writeFileChanged out output

histogram flags args out = do
  let script = "histogram.sh"
  let scriptDir = (baseDir flags) </> "scripts" </> "journal2chart"
  let (dir, file) = splitFileName out
  let month = head $ reverse $ split $ dropExtension file
  need $ [ scriptDir </> script ] ++ extraDeps flags "" out
  (Stdout output) <- cmd "sh" $ [ scriptDir </> script ] ++ args ++ [ "-p", month ]
  writeFileChanged out output

accounts_all flags out = do
  let (dir, file) = splitFileName out
  let budget_dir = (baseDir flags) </> "outputs" </> "sources" </> "budget"
  let virtuals = budget_dir </> "virtuals.journal"
  let budget = budget_dir </> "budget.journal"
  need $ [ budget, virtuals ] ++ (extraDeps flags "" out)
  (Stdout output) <- cmd (hledgerBinary flags) ("-f" : ((baseDir flags) </> "targets" </> "all.journal") : ["accounts"])
  writeFileChanged out output

-------------------
-- Helper functions
-------------------

-- To get included files, look for 'include' or '!include'. Note that we can't use "hledger files", as
-- some of the requested includes might be generated and might not exist yet.
getIncludes base file = do
  src <- liftIO $ readFile file
  let includes = [normalisePath base x | x <- lines src, Just x <- [ stripPrefix "!include " x
                                                                   , stripPrefix "include " x]]
  return (file:includes)

normalisePath base x
  | "/" `isPrefixOf` x = x
  | "reports" `isPrefixOf` x = "reports" </> x
  | otherwise = base </> x

split s = takeWhile (/="") $ unfoldr (Just . head . lex) $ takeFileName s

-- Take "dirpath" and return parent dir of "subdir" component
parentOf :: FilePath -> FilePath -> FilePath
parentOf subdir dirpath =
  joinPath $ takeWhile (/= subdir) $ splitDirectories dirpath

-- Take "dirpath" and replace "this" dir component with "that" dir component
replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir this that dirpath =
  joinPath $ map (\subdir -> if subdir == this then that else subdir) $ splitDirectories dirpath
