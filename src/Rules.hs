{-# Language OverloadedStrings #-}
module Rules (rules, Rule(..)) where

import Text.XML (Document, parseText_, def)
import Text.XML.Cursor
import Database.Migration (Key, Comic, Page(..))
import Data.Maybe (listToMaybe)
import Data.Text (Text)

type Parser = Key Comic -> Document -> Page

data Rule = Rule { ruleComic :: Text
                 , ruleParser :: Parser }

xkcd :: Parser
xkcd comic doc = Page comic (listToMaybe previous) (listToMaybe img) (listToMaybe next)
    where
        cursor = fromDocument doc
        previous = cursor $// element "a" >=> "rel" `attributeIs` "prev" >=> attribute "href"
        img = cursor $// element "div" >=> "id" `attributeIs` "comic" &/ element "img" >=> attribute "src"
        next = cursor $// element "a" >=> "rel" `attributeIs` "next" >=> attribute "href"

rules :: [Rule]
rules = [ Rule "https://xkcd.com" xkcd ]

