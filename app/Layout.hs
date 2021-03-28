{-# LANGUAGE OverloadedStrings #-}

module Layout where

import Data.Text (Text, pack)

px :: Int -> Text
px x = pack (show x) <> "px"

pct :: Int -> Text
pct x = pack (show x) <> "%"

posAbsolute :: (Text, Text)
posAbsolute = ("position", "absolute")

posAbsFill :: [(Text, Text)]
posAbsFill = [ posAbsolute, left (px 0), top (px 0), right (px 0), bottom (px 0) ]

solidBorder :: Text -> Int -> Text
solidBorder color width = ("solid " <> pack (show width) <> "px " <> color)

border :: Text -> (Text, Text)
border b = ("border", b)

borderLeft :: Text -> (Text, Text)
borderLeft b = ("border-left", b)

borderTop :: Text -> (Text, Text)
borderTop b = ("border-top", b)

borderRight :: Text -> (Text, Text)
borderRight b = ("border-right", b)

borderBottom :: Text -> (Text, Text)
borderBottom b = ("border-bottom", b)

left :: Text -> (Text, Text)
left v = ("left", v)

top :: Text -> (Text, Text)
top v = ("top", v)

right :: Text -> (Text, Text)
right v = ("right", v)

bottom :: Text -> (Text, Text)
bottom v = ("bottom", v)

width :: Text -> (Text, Text)
width v = ("width", v)

height :: Text -> (Text, Text)
height v = ("height", v)

--------------------------------------------------------------------------------

color :: Text -> (Text, Text)
color c = ("color", c)

backgroundColor :: Text -> (Text, Text)
backgroundColor c = ("background-color", c)
