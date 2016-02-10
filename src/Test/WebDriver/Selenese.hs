module Test.WebDriver.Selenese where
    
import Text.HTML.TagSoup
import System.FilePath
import Data.Char
import Data.String.Utils (strip)

type Target = String
type Value = String

-- TODO: Find out what the formal parameters are here.
data Selenese 
 = Open Target
 | ClickAndWait Target
 | Click Target
 | Type Target Value
 | AssertTextPresent Target Value
 | VerifyTextPresent Target Value
 | VerifyText Target Value
 | AssertText Target Value
 | WaitForElementPresent Target
 | VerifyElementPresent Target
 | VerifyElementNotPresent Target
 | WaitForElementNotPresent Target
 | WaitForTextPresent Target
 | WaitForTextNotPresent Target
 | VerifyTextNotPresent Target
 | WaitForText Target Value
 | WaitForChecked Target
 | WaitForNotChecked Target
 | Store Target Value
 | StoreEval Target Value
 | StoreValue Target Value
 | StoreAttribute Target Value
 | GotoIf Target Value
 | SelectFrame Target
 | Label String
 | Pause Target
 | Echo Target
 | SetTimeout Target
 | Select Target Value
 | AssertElementPresent Target
 | Unsupported Target Value
 deriving (Show,Eq)

getSeleniumFile :: FilePath -> IO [Selenese]
getSeleniumFile path = fmap readSelenese $ readFile path

splitSelenese tags = map readSingleSelenese $ drop 1 $ partitions (~== "<tr>") $ tags

readSingleSelenese :: [Tag String] -> Selenese
readSingleSelenese tags = toSelenese cmd tgt val
 where split = partitions (~== "<td>") tags
       cmd = getValue $ split
       tgt = getValue $ (drop 1) split
       val = getValue $ (drop 2) split
       getValue = innerText . head
       
-- |
toSelenese :: String -> Target -> Value -> Selenese
toSelenese cmd tgt val = 
 case (map toLower $ strip cmd) of
  "open" -> Open tgt
  "clickandwait" -> ClickAndWait tgt
  "click" -> Click tgt
  "type" -> Type tgt val
  "asserttextpresent" -> AssertTextPresent tgt val
  "verifytextpresent" -> VerifyTextPresent tgt val
  "verifytext" -> VerifyText tgt val
  "asserttext" -> AssertText tgt val
  "waitforelementpresent" -> WaitForElementPresent tgt
  "verifyelementpresent" -> VerifyElementPresent tgt
  "verifyelementnotpresent" -> VerifyElementNotPresent tgt
  "waitforelementnotpresent" -> WaitForElementNotPresent tgt
  "waitfortextpresent" -> WaitForTextPresent tgt
  "waitfortextnotpresent" -> WaitForTextNotPresent tgt
  "verifytextnotpresent" -> VerifyTextNotPresent tgt
  "waitfortext" -> WaitForText tgt val
  "waitforchecked" -> WaitForChecked tgt
  "waitfornotchecked" -> WaitForNotChecked tgt
  "store" -> Store tgt val
  "storeeval" -> StoreEval tgt val
  "storevalue" -> StoreValue tgt val
  "storeattribute" -> StoreAttribute tgt val
  "gotoif" -> GotoIf tgt val
  "selectframe" -> SelectFrame tgt
  "label" -> Label tgt
  "pause" -> Pause tgt
  "echo" -> Echo tgt
  "settimeout" -> SetTimeout tgt
  "select" -> Select tgt val
  "assertelementpresent" -> AssertElementPresent tgt
  _ -> Unsupported tgt val
  

readSelenese :: String -> [Selenese]
readSelenese input = splitSelenese (parseTags input)

-- Debugging stuff
input = readFile "../files/testCaseTool.html"
tags = fmap parseTags input
parts = fmap (partitions (~== "<tr>")) tags