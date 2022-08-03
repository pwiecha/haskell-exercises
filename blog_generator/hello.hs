-- Top
main :: IO ()
main = putStrLn (render myhtml)

-- Html logic
newtype Html = Html String
    deriving Show

render :: Html -> String
render (Html contents) = contents

-- Structure logic
newtype Structure = Structure String

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

type Title = String

-- Program
myhtml :: Html
myhtml = Html (makeHtml "Title" (h1_ "This is a header!" <> p_ "First paragraph"))

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> Structure
html_ = Structure . el "html" -- partial application

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