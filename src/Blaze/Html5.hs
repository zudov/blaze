{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Blaze.Html5
  (-- * Re-exports
    module Text.Blaze
  , module Text.Blaze.Html5
  , module Text.Blaze.Html5.Attributes
   -- * Wrapper for class attribute
  , ClassAttributeValue(..)
  , toClassValue
   -- * Attribute combinators
  , (!.)
  , (!#)
   -- * Common operations
   -- ** Linking
  , css
  , js
  , linkTo
   -- ** Forms
  , postForm
   -- ** Intercalation
  , linesToHtml
  , htmlIntercalate
  , htmlCommasAnd
  , htmlCommas
  ) where

import           Data.String (IsString,fromString)
import           Data.Monoid
import           Text.Blaze
import           Text.Blaze.Html5 hiding (map,style,title)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Attributes hiding (span,label,cite,form,summary,min)
import           Text.Blaze.Internal (Attributable,stringValue)


-- | Class attribute.
(!.) :: (Attributable h) => h -> ClassAttributeValue -> h
e !. classes = e ! class_ (unClassAttributeValue classes)

-- | Id attribute.
(!#) :: (Attributable h) => h -> AttributeValue -> h
e !# idName = e ! A.id idName

-- | The html class attribute can have one or more space separated tokens.
--   @ClassAttributeValues@ are instances of @Monoid@ and therefore can be combined
--   with each other. Combined tokens are space separated. That allows more 
--   flexible usage of predefined class tokens.
--
--   Example (uses blaze-bootstrap):
--
--   > div !. (md6 <> sm12) $ "Example"
--
--   Result:
--
--   > <div class="col-md-6 col-sm-12">Example</div>
newtype ClassAttributeValue = 
	ClassAttributeValue { unClassAttributeValue :: AttributeValue }

instance IsString ClassAttributeValue where
	fromString = ClassAttributeValue . fromString

instance Monoid ClassAttributeValue where
	mempty = ClassAttributeValue mempty
	mappend (ClassAttributeValue c1) (ClassAttributeValue c2) =
		ClassAttributeValue $ c1 <> stringValue " " <> c2

toClassValue :: ToValue a => a -> ClassAttributeValue
toClassValue = ClassAttributeValue . toValue

-- | Render the lines as HTML lines.
linesToHtml :: [Html] -> Html
linesToHtml = htmlIntercalate br

-- | Intercalate the given things.
htmlIntercalate :: Html -> [Html] -> Html
htmlIntercalate _ [x] = x
htmlIntercalate sep (x:xs) = do x; sep; htmlIntercalate sep xs
htmlIntercalate _ []  = mempty

-- | Show some HTML comma-separated with “and” inbetween to be grammatical.
htmlCommasAnd :: [Html] -> Html
htmlCommasAnd [x] = x
htmlCommasAnd [x,y] = do x; " and "; y
htmlCommasAnd (x:xs) = do x; ", "; htmlCommasAnd xs
htmlCommasAnd []  = mempty

-- | Comma-separate some HTML.
htmlCommas :: [Html] -> Html
htmlCommas = htmlIntercalate ", "

-- | Link to a CSS stylesheet.
css :: AttributeValue -> Html
css uri = link ! rel "stylesheet" ! type_ "text/css" ! href uri

-- | Link to a javascript file.
js :: AttributeValue -> Html
js uri = script ! type_ "text/javascript" ! src uri $ mempty

-- | Create a link.
linkTo :: AttributeValue -> Html -> Html
linkTo url = a ! href url

-- | Create a form with method = \"POST\" that posts to the given url.
postForm :: String -> Html -> Html
postForm uri = form ! action (toValue uri) ! method "post"
