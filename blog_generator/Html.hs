module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

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
html_ title (Structure content) = Html ((el "html" . el "head" . el "title" $ title) <> el "body" content)

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
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1_"

{--
-- Obsolete from 3.5
myhtml :: Html
myhtml = Html (makeHtml "Title" (h1_ "This is a header!" <> p_ "First paragraph"))

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ :: String -> Structure
html_ = Structure . el "html" -- partial application
--}
