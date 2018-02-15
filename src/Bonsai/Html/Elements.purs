-- | Bonsai HTML Element Helpers
-- |
-- | These helper functions are borrowed from Elm,
-- | the actual DSL was "inspired" by Smolder.
module Bonsai.Html.Elements
where

import Prelude hiding (div)

import Bonsai.Html.Internal (Markup, leaf, parent)



-- BLOCK LEVEL ELEMENTS




-- | HTML body element
body :: Markup ~> Markup
body =
  parent "body"

-- | A (major) section in a document
section :: Markup ~> Markup
section =
  parent "section"

-- | A section that contains only navigation links
nav :: Markup ~> Markup
nav =
  parent "nav"

-- | Self-contained content that could exist independently.
article :: Markup ~> Markup
article =
  parent "article"

-- | Content that is loosely related to the page.
-- |
-- | If it is removed, the remaining content still makes sense.
aside :: Markup ~> Markup
aside =
  parent "aside"

-- | Html header
h1 :: Markup ~> Markup
h1 =
  parent "h1"

-- | Html header
h2 :: Markup ~> Markup
h2 =
  parent "h2"

-- | Html header
h3 :: Markup ~> Markup
h3 =
  parent "h3"

-- | Html header
h4 :: Markup ~> Markup
h4 =
  parent "h4"

-- | Html header
h5 :: Markup ~> Markup
h5 =
  parent "h5"

-- | Html header
h6 :: Markup ~> Markup
h6 =
  parent "h6"

-- | Header of a page or section.
header :: Markup ~> Markup
header =
  parent "header"

-- | Footer of a page or section.
footer :: Markup ~> Markup
footer =
  parent "footer"

-- | A section containing contact information.
address :: Markup ~> Markup
address =
  parent "address"

-- | Main or important content in the document.
-- |
-- | There is only one main element in the document.
main_ :: Markup ~> Markup
main_ =
  parent "main"

-- | Main or important content in the document.
-- |
-- | There is only one main element in the document.
main :: Markup ~> Markup
main =
  main_

-- | The content should be displayed as a paragraph.
p :: Markup ~> Markup
p =
  parent "p"

-- | A thematic break between paragraphs of a section or article or any longer content.
hr :: forall msg. Markup msg
hr =
  leaf "hr"

-- | Indictes that its content is preformattet and the format must be preserved.
pre_ :: Markup ~> Markup
pre_ =
  parent "pre"

-- | Represents content that is quoted from another source
blockquote :: Markup ~> Markup
blockquote =
  parent "blockquote"

-- | Ordered list of items (li)
ol :: Markup ~> Markup
ol =
  parent "ol"

-- | Unordered list of items (li)
ul :: Markup ~> Markup
ul =
  parent "ul"

-- | An item of an enumeration list.
li :: Markup ~> Markup
li =
  parent "li"

-- | Definition List, a list of terms and their definitions
dl :: Markup ~> Markup
dl =
  parent "dl"

-- | A term in a definitions list
dt :: Markup ~> Markup
dt =
  parent "dt"

-- | A definition in a definition list
dd :: Markup ~> Markup
dd =
  parent "dd"

-- | A figure illustrated as part of the document
figure :: Markup ~> Markup
figure =
  parent "figure"

-- | Legend for a figure.
figcaption :: Markup ~> Markup
figcaption =
  parent "figcaption"

-- | Generic container with no special meaning.  Also div.
div_ :: Markup ~> Markup
div_ =
  parent "div"

-- | Generic container with no special meaning.
-- |
-- | This clashes with a function from the prelude,
-- | so either use div_ or "import Prelude hiding (div)"
div :: Markup ~> Markup
div = div_



-- TEXT LEVEL ELEMENTS



-- | Represents a link to another resource.
a :: Markup ~> Markup
a =
  parent "a"

-- | Represents emphasized text, like a stress accent.
em :: Markup ~> Markup
em =
  parent "em"

-- | Represents especially important text.
strong :: Markup ~> Markup
strong =
  parent "strong"

-- | Represents a side comment e.g. text like a disclaimer
-- | or copyright.
-- | Not essential to comprehension of the document.
small :: Markup ~> Markup
small =
  parent "small"

-- | Represents content that is no longer accurate or relevant.
s :: Markup ~> Markup
s =
  parent "s"

-- | Represents the title of a work.
cite :: Markup ~> Markup
cite =
  parent "cite"

-- | An inline quotation.
q :: Markup ~> Markup
q =
  parent "q"

-- | A term whose definition is contained in its nearest ancestore.
dfn :: Markup ~> Markup
dfn =
  parent "dfn"
-- | An abbreviation or acronym, the expansion can be given in the title attribute.

abbr :: Markup ~> Markup
abbr =
  parent "abbr"

-- | A data and time value, the machine-readable representation can be give in the datetime attribute.
time :: Markup ~> Markup
time =
  parent "time"

-- | Represents computer code.
code :: Markup ~> Markup
code =
  parent "code"

-- | Represents a variable
var :: Markup ~> Markup
var =
  parent "var"

-- | Represents output of a program or computer.
samp :: Markup ~> Markup
samp =
  parent "samp"

-- | Represents user input, often from the keyboard.
kbd :: Markup ~> Markup
kbd =
  parent "kbd"

-- | Subscript
sub :: Markup ~> Markup
sub =
  parent "sub"

-- | Superscript
sup :: Markup ~> Markup
sup =
  parent "sup"

-- | Represents text in some alternate voice or mood.
-- |
-- | Used with bootstrap et. al. for icons
i :: Markup ~> Markup
i =
  parent "i"

-- | Text to which attention should be drawn for utilarian purposes.
-- |
-- | It's bold, the non-semantic strong
b :: Markup ~> Markup
b =
  parent "b"

-- | Underline with no predefined semantics
u :: Markup ~> Markup
u =
  parent "u"

-- | Text highlighted for reference purposes, i.e. for relevance in another context.
mark :: Markup ~> Markup
mark =
  parent "mark"

-- | Ruby annotations, ???
ruby :: Markup ~> Markup
ruby =
  parent "ruby"

-- | Ruby annotations, ???
rt :: Markup ~> Markup
rt =
  parent "rt"

-- | Ruby annotations, ???
rp :: Markup ~> Markup
rp =
  parent "rp"

-- | Content with bidirectional text formatting.
bdi :: Markup ~> Markup
bdi =
  parent "bdi"

-- | Represents bidirectionality of its children in order to override the unicode bidirectional algorithm.
bdo :: Markup ~> Markup
bdo =
  parent "bdo"

-- | Text with no specific meaning.
span :: Markup ~> Markup
span =
  parent "span"

-- | Line break.
br :: forall msg. Markup msg
br =
  leaf "br"

-- | Line break opportunity
wbr :: Markup ~> Markup
wbr =
  parent "wbr"




-- EDITS





-- | Defines an addition to the document.
ins :: Markup ~> Markup
ins =
  parent "ins"

-- | Defines a deletion from the document.
del :: Markup ~> Markup
del =
  parent "del"




-- EMBEDDED CONTENT



-- | An image (URL given by src attribute)
img :: Markup ~> Markup
img =
  parent "img"

-- | Embedded HTML document
iframe :: Markup ~> Markup
iframe =
  parent "iframe"

-- | Integration point for external application or content.
embed :: Markup ~> Markup
embed =
  parent "embed"

-- | Represents external resource that may be processed by a plugin
object :: Markup ~> Markup
object =
  parent "object"

-- | Parameters for plugins invoked by object.
param :: Markup ~> Markup
param =
  parent "param"

-- | Represents a video
video :: Markup ~> Markup
video =
  parent "video"

-- | Represents a sound or audio stream.
audio :: Markup ~> Markup
audio =
  parent "audio"

-- | Allows specification of alternative media resources for video or audio.
source :: Markup ~> Markup
source =
  parent "source"


-- | Timed text tracks for audio or video content
track :: Markup ~> Markup
track =
  parent "track"

-- | Bitmap area for graphics rendering.
canvas :: Markup ~> Markup
canvas =
  parent "canvas"

-- | Mathematical formule
math :: Markup ~> Markup
math =
  parent "math"

-- | HTML Table
table :: Markup ~> Markup
table =
  parent "table"

-- | Caption for a table
caption :: Markup ~> Markup
caption =
  parent "caption"

-- | Colgroup for a table
colgroup :: Markup ~> Markup
colgroup =
  parent "colgroup"

-- | Represents a column of a table.
col :: Markup ~> Markup
col =
  parent "col"

-- | Table body - contains the data rows
tbody :: Markup ~> Markup
tbody =
  parent "tbody"


-- | Table header - contains headers rows explaining the data
thead :: Markup ~> Markup
thead =
  parent "thead"

-- | Table footer
tfoot :: Markup ~> Markup
tfoot =
  parent "tfoot"

-- | A table row
tr :: Markup ~> Markup
tr =
  parent "tr"

-- | A cell in a a table row.
td :: Markup ~> Markup
td =
  parent "td"

-- | A cell in a table header.
th :: Markup ~> Markup
th =
  parent "th"

--  | A submittable form
form :: Markup ~> Markup
form =
  parent "form"

-- | Represents a set of controls in (usually) a form
fieldset :: Markup ~> Markup
fieldset =
  parent "fieldset"

-- | The caption for a fieldset.
legend :: Markup ~> Markup
legend =
  parent "legend"

-- | The caption for a form control
label :: Markup ~> Markup
label =
  parent "label"

-- | Typed data field allowing the user to edit the data.
input :: forall msg. Markup msg
input =
  leaf "input"

-- | A button that can be clicked.
button :: Markup ~> Markup
button =
  parent "button"

-- | Control that allows section among a set of options
select :: Markup ~> Markup
select =
  parent "select"

-- | A predefined set of options for other controls.
datalist :: Markup ~> Markup
datalist =
  parent "datalist"

-- | A set of options, logically grouped.
optgroup :: Markup ~> Markup
optgroup =
  parent "optgroup"

-- | A option in a select tag, or a suggestion in a datalist
option :: Markup ~> Markup
option =
  parent "option"

-- | A multiline text edit control.
textarea :: forall msg. Markup msg
textarea =
  leaf "textarea"

-- | A key-pair generator control.
keygen :: Markup ~> Markup
keygen =
  parent "keygen"

-- | Represents the result of a computation.
output :: Markup ~> Markup
output =
  parent "output"

-- | Represents the completion progress of a task.
progress :: Markup ~> Markup
progress =
  parent "progress"

-- | Represents a scalar measurement with a known range.
meter :: Markup ~> Markup
meter =
  parent "meter"

-- | Represents a widget from which the user can obtaion additional input or controls.
details :: Markup ~> Markup
details =
  parent "details"

-- | A summary for a given details.
summary :: Markup ~> Markup
summary =
  parent "summary"

-- | A command the user can invoke.
menuitem :: Markup ~> Markup
menuitem =
  parent "menuitem"

-- | A list of invokable commands
menu :: Markup ~> Markup
menu =
  parent "menu"
