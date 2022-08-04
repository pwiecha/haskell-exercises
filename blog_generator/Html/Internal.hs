module Html.Internal where

-- Html logic
newtype Html = Html String
    deriving Show

render :: Html -> String
render (Html contents) = contents

-- Structure logic
newtype Structure = Structure String

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str
-- or (Structure content) = content

type Title = String

html_ :: Title -> Structure -> Html
html_ title (Structure content) = 
  Html ((el "html" . el "head" . el "title" . escape $ title)
  <> el "body" content)

makeList :: String -> [Structure] -> Structure
makeList tag = Structure . el tag . concatMap (el "li" . getStructureString)

ul_ :: [Structure] -> Structure
ul_ = makeList "ul"

ol_ :: [Structure] -> Structure
ol_ = makeList "ol"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_ :: String -> Structure
body_ = Structure . el "body"

head_ :: String -> Structure
head_ = Structure . el "head"

title_ :: String -> Structure
title_ = Structure . el "title"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape


-- concat for flatting list of lists [[String]]-> [String]
escape :: String -> String
escape = concatMap escapeChar where
  escapeChar :: Char -> String
  escapeChar c = case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> [c]

{--
-- Obsolete from 3.5
myhtml :: Html
myhtml = Html (makeHtml "Title" (h1_ "This is a header!" <> p_ "First paragraph"))

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ :: String -> Structure
html_ = Structure . el "html" -- partial application
--}
{--
ul_ :: [Structure] -> Structure
ul_ structureList = append_ (Structure "<ul>") (foldr (append_ . li_) (Structure "\n</ul>") structureList)

ol_ :: [Structure] -> Structure
ol_ structureList = append_ (Structure "<ol>") (foldr (append_ . li_) (Structure "\n</ol>") structureList)

-- wrap in li element
li_ :: Structure -> Structure
li_ = Structure . ("\n  " ++). el "li" . getStructureString
--}
