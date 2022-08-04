import Html

main :: IO ()
main = putStrLn (render myhtml)

-- Program
myhtml :: Html
myhtml = html_ "My Title" (append_ (h1_ "Heading") (append_ (p_ "Paragraph1") (p_ "Paragraph2")))
