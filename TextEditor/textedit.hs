----- Control.Monad module is imported for the use of different IO functions later in the code.
import Control.Monad
----- Data.Char module imported to use the isSpace function later in the code.
import Data.Char
----- System.Exit module imported for proper program termination.
import System.Exit
----- Creates the StringEdit typeclass, using record syntax.
data StringEdit = StringEdit {
                              lOfCursor :: String
                             ,highlighted :: String
                             ,rOfCursor :: String
                             ,clipboard :: String
                             } deriving (Show)
----- Initialises the StringEdit typeclass in the initialString function.
initialString :: StringEdit
initialString = StringEdit [] [] [] []
----- Preset string for testing program.
preset :: StringEdit
preset = StringEdit " quick brown fox jumps over the lazy dog." [] [] []
----- Concatenates the existing string with an inputted string.
addTo :: StringEdit -> String -> StringEdit
addTo (StringEdit l h r c) x = StringEdit (l ++ x) h r c
----- Removes a character to the left of the cursor.
backspace :: StringEdit -> StringEdit
backspace (StringEdit l h r c)
   | h == [] = StringEdit (init l) h r c
   | otherwise = StringEdit l [] r c
----- Removes a character to the right of the cursor.
delete :: StringEdit -> StringEdit
delete (StringEdit l h r c) 
   | h == [] = StringEdit l h (tail r) c
   | otherwise = StringEdit l [] r c
----- Deletes the word that the cursor is currently on/next to.
delWord :: StringEdit -> StringEdit
delWord (StringEdit l h r c)
   | l /= [] && not (isSpace (last l)) = delWord (backspace (StringEdit l h r c))
   | r /= [] && not (isSpace (head r)) = delWord (delete (StringEdit l h r c))
   | otherwise = StringEdit l h r c 
----- Moves the cursor left by a character.
cLeft :: StringEdit -> StringEdit
cLeft (StringEdit l h r c)
   | l /= [] = StringEdit (init l) [] ([last l] ++ h ++ r) c
   | otherwise = StringEdit l h r c
----- Moves the cursor right by a character.
cRight :: StringEdit -> StringEdit
cRight (StringEdit l h r c)
   | r /= [] = StringEdit (l ++ h ++ [head r]) [] (tail r) c
   | otherwise = StringEdit l h r c
----- Moves the cursor left by a word.
cWordLeft :: StringEdit -> StringEdit
cWordLeft (StringEdit l h r c)
   | l == [] = StringEdit l h r c
   | isSpace (last l) = StringEdit l h r c
   | otherwise = cWordLeft (cLeft (StringEdit l h r c))
----- Moves the cursor right by a word.
cWordRight :: StringEdit -> StringEdit
cWordRight (StringEdit l h r c)
   | r == [] = StringEdit l h r c
   | isSpace (head r) = StringEdit l h r c
   | otherwise = cWordRight (cRight (StringEdit l h r c))
----- Moves the cursor to the beginning of the string.
cStart :: StringEdit -> StringEdit
cStart (StringEdit l h r c) = StringEdit [] [] (l ++ h ++ r) c
----- Moves the cursor to the endo f the string.
cEnd :: StringEdit -> StringEdit
cEnd (StringEdit l h r c) = StringEdit (l ++ h ++ r) [] [] c
----- Total character count of the string.
charCount :: StringEdit -> Int
charCount (StringEdit l h r c) = length (l ++ h ++ r)
----- Highlights the character left of the cursor.
highlightLeft :: StringEdit -> StringEdit
highlightLeft (StringEdit l h r c) = StringEdit (init l) ([last l] ++ h) r c
----- Highlights the character right of the cursor.
highlightRight :: StringEdit -> StringEdit
highlightRight (StringEdit l h r c) = StringEdit l (h ++ [head r]) (tail r) c
----- Highlights the word that the cursor is currently on/next to.
highlightWord :: StringEdit -> StringEdit
highlightWord (StringEdit l h r c)
   | l /= [] && not (isSpace (last l)) = highlightWord (highlightWordLeft (StringEdit l h r c))
   | r /= [] && not (isSpace (head r)) = highlightWord (highlightWordRight (StringEdit l h r c))
   | otherwise = StringEdit l h r c
----- Highlights the word to the left of the cursor.
highlightWordLeft :: StringEdit -> StringEdit
highlightWordLeft (StringEdit l h r c)
   | l == [] = StringEdit l h r c
   | isSpace (last l) = StringEdit l h r c
   | otherwise = highlightWordLeft (highlightLeft (StringEdit l h r c))
----- Highlights the word to the right of the cursor.
highlightWordRight :: StringEdit -> StringEdit
highlightWordRight (StringEdit l h r c)
   | r == [] = StringEdit l h r c
   | isSpace (head r) = StringEdit l h r c
   | otherwise = highlightWordRight (highlightRight (StringEdit l h r c))
----- Highlights the string to the left of the cursor.
highlightAllLeft :: StringEdit -> StringEdit
highlightAllLeft (StringEdit l h r c) = StringEdit [] (l ++ h) r c
----- Highlights the string to the right of the cursor.
highlightAllRight :: StringEdit -> StringEdit
highlightAllRight (StringEdit l h r c) = StringEdit l (h ++ r) [] c
----- Copies the highlighted text to the clipboard.
copyHighlighted :: StringEdit -> StringEdit
copyHighlighted (StringEdit l h r c) = StringEdit l h r (h)
----- Copies the highlighted text to the clipboard, and deletes the highlighted text.
cutHighlighted :: StringEdit -> StringEdit
cutHighlighted (StringEdit l h r c) = StringEdit l [] r (h)
----- Deletes the highlighted text.
delHighlighted :: StringEdit -> StringEdit
delHighlighted (StringEdit l h r c) = StringEdit l [] r c
----- Pastes the copied text from the clipboard to the left of the cursor.
paste :: StringEdit -> StringEdit
paste (StringEdit l h r c) =  StringEdit (l ++ c) [] r c
----- Deletes the contents of the clipboard
clearClipboard :: StringEdit -> StringEdit
clearClipboard (StringEdit l h r c) = StringEdit l h r []
----- Loads a text file from user input into the program.
load :: String -> IO StringEdit
load fn = do
    x <- readFile (fn ++ ".txt")
    let t = StringEdit x [] [] []
    return t
----- Gets file name from user input and saves the contents of the StringEdit typeclass to a text file.
save :: StringEdit -> IO ()
save (StringEdit l h r c) = do
    putStr "Enter Filename: "
    x <- getLine
    writeFile (x ++ ".txt") (l ++ h ++ r)
    
----- MAIN UI (WIP) -----
--main = do
--   putStrLn "Load File (L) | Input String (S) | Show Controls (C)"
--   q <- getLine
--   when (q == "c") (forever $ do
--        putStrLn "| <- (cl/cwl/cs) | -> (cr/cwr/ce) | Add to string (add) | Backspace (bksp) | Delete (del) | Delete Word (delw) |"
--        putStrLn "| Character Count (cc) | <-Highlight (hll/hllw/hlls) | ->Highlight (hlr/hlrw/hlrs) | Highlight Word (hlw) |"
--        putStrLn "| Copy (cpy) | Cut (cut) | Paste (pst) | Delete Highlighted (delh) | Clear Clipboard (clr) | Save File (save) |"
--        q <- getLine
--        putStrLn q