{-# LANGUAGE OverloadedStrings #-}

module Refract.DOM where

import qualified Control.Monad.Trans.State as ST

import qualified Data.Map as M
import qualified Data.Text as T

import Refract.DOM.Props (Props(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)
import qualified Replica.VDOM as VDOM

type DomPath = [Int]

newtype Component st = Component { runComponent :: DomPath -> (st -> IO ()) -> st -> VDOM.HTML }

domPath :: (DomPath -> Component st) -> Component st
domPath f = Component $ \path setState st -> runComponent (f path) path setState st

el :: T.Text -> [Props st] -> [Component st] -> Component st
el = elWithNamespace Nothing

elWithNamespace :: Maybe VDOM.Namespace -> T.Text -> [Props st] -> [Component st] -> Component st
elWithNamespace ns name props children = Component $ \path setState st ->
  [ VDOM.VNode
      name
      (M.unions $ map (toProps setState st) props)
      ns
      $ mconcat
          [ runComponent child (i:path) setState st
          | (i, child) <- zip [0..] children
          ]
  ]
  where
    toProps setState st (Props k (PropEvent opts f)) = M.singleton k $ VDOM.AEvent opts $ \de -> ST.execStateT (f de) st >>= setState
    toProps setState st (Props k (PropText v)) = M.singleton k $ VDOM.AText v
    toProps setState st (Props k (PropBool v)) = M.singleton k $ VDOM.ABool v
    toProps setState st (Props k (PropMap m)) = M.singleton k $ VDOM.AMap $ M.unions $ map (toProps setState st) m
 
empty :: Component st
empty = Component $ \_ _ _ -> []

text :: T.Text -> Component st
text txt = Component $ \_ _ _ -> [VDOM.VText txt]

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: [Props st] -> [Component st] -> Component st
div  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: [Props st] -> [Component st] -> Component st
table  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: [Props st] -> [Component st] -> Component st
thead  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: [Props st] -> [Component st] -> Component st
tbody  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: [Props st] -> [Component st] -> Component st
tr  = el "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>

trKeyed :: T.Text -> [Props st] -> [Component st] -> Component st
trKeyed k props = el "tr" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: [Props st] -> [Component st] -> Component st
th  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: [Props st] -> [Component st] -> Component st
td  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: [Props st] -> [Component st] -> Component st
tfoot  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: [Props st] -> [Component st] -> Component st
section  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: [Props st] -> [Component st] -> Component st
header  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: [Props st] -> [Component st] -> Component st
footer  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: [Props st] -> [Component st] -> Component st
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: [Props st] -> [Component st] -> Component st
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: [Props st] -> [Component st] -> Component st
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: [Props st] -> [Component st] -> Component st
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: [Props st] -> [Component st] -> Component st
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: [Props st] -> [Component st] -> Component st
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: [Props st] -> [Component st] -> Component st
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: [Props st] -> [Component st] -> Component st
li = el "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed :: T.Text -> [Props st] -> [Component st] -> Component st
liKeyed k props = el "li" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: [Props st] -> [Component st] -> Component st
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: [Props st] -> [Component st] -> Component st
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: [Props st] -> [Component st] -> Component st
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: [Props st] -> [Component st] -> Component st
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: [Props st] -> [Component st] -> Component st
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: [Props st] -> [Component st] -> Component st
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: [Props st] -> Component st
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: [Props st] -> [Component st] -> Component st
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: [Props st] -> Component st
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: [Props st] -> [Component st] -> Component st
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: [Props st] -> [Component st] -> Component st
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: [Props st] -> [Component st] -> Component st
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: [Props st] -> [Component st] -> Component st
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: [Props st] -> [Component st] -> Component st
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: [Props st] -> [Component st] -> Component st
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: [Props st] -> [Component st] -> Component st
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: [Props st] -> [Component st] -> Component st
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: [Props st] -> Component st
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: [Props st] -> [Component st] -> Component st
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: [Props st] -> [Component st] -> Component st
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: [Props st] -> [Component st] -> Component st
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: [Props st] -> [Component st] -> Component st
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: [Props st] -> [Component st] -> Component st
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: [Props st] -> [Component st] -> Component st
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: [Props st] -> [Component st] -> Component st
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: [Props st] -> [Component st] -> Component st
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: [Props st] -> [Component st] -> Component st
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: [Props st] -> [Component st] -> Component st
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: [Props st] -> [Component st] -> Component st
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: [Props st] -> [Component st] -> Component st
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: [Props st] -> [Component st] -> Component st
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: [Props st] -> [Component st] -> Component st
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: [Props st] -> [Component st] -> Component st
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: [Props st] -> Component st
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: [Props st] -> Component st
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: [Props st] -> Component st
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: [Props st] -> [Component st] -> Component st
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: [Props st] -> Component st
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: [Props st] -> [Component st] -> Component st
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: [Props st] -> [Component st] -> Component st
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: [Props st] -> [Component st] -> Component st
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: [Props st] -> [Component st] -> Component st
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: [Props st] -> [Component st] -> Component st
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: [Props st] -> [Component st] -> Component st
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: [Props st] -> [Component st] -> Component st
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: [Props st] -> [Component st] -> Component st
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: [Props st] -> [Component st] -> Component st
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: [Props st] -> [Component st] -> Component st
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: [Props st] -> [Component st] -> Component st
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: [Props st] -> [Component st] -> Component st
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: [Props st] -> Component st
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: [Props st] -> [Component st] -> Component st
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: [Props st] -> [Component st] -> Component st
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: [Props st] -> [Component st] -> Component st
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: [Props st] -> [Component st] -> Component st
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Props st] -> [Component st] -> Component st
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: [Props st] -> [Component st] -> Component st
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: [Props st] -> [Component st] -> Component st
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: [Props st] -> [Component st] -> Component st
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: [Props st] -> [Component st] -> Component st
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: [Props st] -> [Component st] -> Component st
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: [Props st] -> [Component st] -> Component st
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: [Props st] -> Component st
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: [Props st] -> [Component st] -> Component st
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: [Props st] -> [Component st] -> Component st
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: [Props st] -> [Component st] -> Component st
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: [Props st] -> [Component st] -> Component st
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: [Props st] -> [Component st] -> Component st
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: [Props st] -> [Component st] -> Component st
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: [Props st] -> [Component st] -> Component st
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: [Props st] -> [Component st] -> Component st
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: [Props st] -> Component st
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: [Props st] -> [Component st] -> Component st
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: [Props st] -> [Component st] -> Component st
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: [Props st] -> [Component st] -> Component st
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: [Props st] -> [Component st] -> Component st
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: [Props st] -> [Component st] -> Component st
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: [Props st] -> [Component st] -> Component st
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: [Props st] -> [Component st] -> Component st
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: [Props st] -> [Component st] -> Component st
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: [Props st] -> [Component st] -> Component st
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: [Props st] -> Component st
link = flip (el "link") []
