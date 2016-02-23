module Test.WebDriver.Selenese where
    
import Text.HTML.TagSoup
import System.FilePath
import Data.Char
import Data.String.Utils (strip)

data Value
 = Placeholder String
 | Normal String
 deriving (Show, Eq)

data Target
 = Identifier String
 | Id String
 | Name String
 | XPath String
 | Link String
 | CSS String
 | DOM String
 deriving (Show, Eq)

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
 | Label Target
 | Pause Target
 | Echo Target
 | SetTimeout Target
 | Select Target Value
 | AssertElementPresent Target
 | Unsupported String Target Value
 deriving (Show,Eq)

getSeleniumFile :: FilePath -> IO [Selenese]
getSeleniumFile path = fmap readSelenese $ readFile path

splitSelenese tags = map readSingleSelenese $ drop 1 $ partitions (~== "<tr>") $ tags

readSingleSelenese :: [Tag String] -> Selenese
readSingleSelenese tags = toSelenese cmd tgt val
 where split = partitions (~== "<td>") tags
       cmd = getValue $ split
       tgt = toTarget $ getValue $ (drop 1) split
       val = toValue $  getValue $ (drop 2) split
       getValue = strip . innerText . head
       
-- |
toSelenese :: String -> Target -> Value -> Selenese
toSelenese cmd tgt val = 
 case (map toLower cmd) of
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
  _ -> Unsupported cmd tgt val
  
toValue :: String -> Value
toValue s | take 2 s == "${" && (take 1 . reverse) s == "}" = Placeholder ((reverse . drop 1 . reverse . drop 2) s)
          | otherwise = Normal s
  
toTarget :: String -> Target
toTarget s | take 3 s' == "id=" = Id (drop 3 s')
           | take 5 s' == "name=" = Name (drop 5 s')
           | take 6 s' == "xpath=" = XPath (drop 6 s')
           | take 2 s' == "//" = XPath s' -- "Locators starting with “//” will use the XPath locator strategy."
           | take 5 s' == "link=" = Link (drop 5 sKeepCase)
           | take 4 s' == "dom=" = DOM (drop 4 s')
           | take 8 s' == "document" = DOM s' -- "Locators starting with “document” will use the DOM locator strategy."
           | take 4 s' == "css=" = CSS (drop 4 s')
           | otherwise = Identifier s' -- "Locators without an explicitly defined locator strategy will default to using the identifier locator strategy."
           
 where s' = (map toLower . strip) s -- Get rid of whitespace and lowercase everything
       sKeepCase = strip s -- Link is case sensitive.

readSelenese :: String -> [Selenese]
readSelenese input = splitSelenese (parseTags input)

-- Debugging stuff
input = readFile "files/testCaseTool.html"
tags = fmap parseTags input
parts = fmap (partitions (~== "<tr>")) tags
