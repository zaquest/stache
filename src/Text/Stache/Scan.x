{
module Text.Stache.Scan (scan, Token(..), Path) where

import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Stache.Types
import Codec.Binary.UTF8.String (encodeChar)
}

$alpha = [a-zA-Z]
$nocurl = . # \{

@path = $alpha [$alpha 0-9\_\.]*

tokens :-

  "{{{" @path "}}}"             { mkTokenRaw        }
  "{{#if" $white+ @path "}}"    { mkTokenIfOpen     }
  "{{#elif" $white+ @path "}}"  { mkTokenElif       }
  "{{#else}}"                   { mkTokenElse       }
  "{{/if}}"                     { mkTokenIfClose    }
  "{{#each" $white+ @path "}}"  { mkTokenEachOpen   }
  "{{/each}}"                   { mkTokenEachClose  }
  "{{" @path "}}"               { mkTokenEscaped    }
  [$nocurl \n]+                 { mkTokenLit        }

{

type Byte = Word8

data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  Text)         -- current input string

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case T.uncons s of
    Nothing -> Nothing
    Just (c, s') -> let p' = alexMove p c
                        (b:bs) = encodeChar c
                     in p' `seq` Just (b, (p', c, bs, s'))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _ = AlexPn (a+1) l (c+1)

scan :: Text -> [Token]
scan str = go (alexStartPos, '\n', [], str)
  where go inp@(pos, _, _, str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column), _, _, _) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (T.take len str) : go inp'


data Token = TokenRaw Path
           | TokenIfOpen Path
           | TokenElif Path
           | TokenElse
           | TokenIfClose
           | TokenEachOpen Path
           | TokenEachClose
           | TokenEscaped Path
           | TokenLit Literal
  deriving (Show)

explode :: Text -> Path
explode = T.split (== '.')

middle :: Int -> Int -> Text -> Text
middle prefLen sufLen = T.dropEnd sufLen . T.drop prefLen

mkTokenRaw :: AlexPosn -> Text -> Token
mkTokenRaw _ = TokenRaw . explode . middle 3 3

mkOpen :: (Path -> Token) -> AlexPosn -> Text -> Token
mkOpen mkToken _ = mkToken . explode . (!! 1) . T.words . middle 3 2

mkTokenIfOpen :: AlexPosn -> Text -> Token
mkTokenIfOpen = mkOpen TokenIfOpen

mkTokenElif :: AlexPosn -> Text -> Token
mkTokenElif = mkOpen TokenElif

mkTokenElse :: AlexPosn -> Text -> Token
mkTokenElse _ _ = TokenElse

mkTokenIfClose :: AlexPosn -> Text -> Token
mkTokenIfClose _ _ = TokenIfClose

mkTokenEachOpen :: AlexPosn -> Text -> Token
mkTokenEachOpen = mkOpen TokenEachOpen

mkTokenEachClose :: AlexPosn -> Text -> Token
mkTokenEachClose _ _ = TokenEachClose

mkTokenEscaped :: AlexPosn -> Text -> Token
mkTokenEscaped _ = TokenEscaped . explode . middle 2 2

mkTokenLit :: AlexPosn -> Text -> Token
mkTokenLit _ = TokenLit

}
