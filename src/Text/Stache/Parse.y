{
module Text.Stache.Parse (parse) where

import Data.Text (Text)
import Text.Stache.Types
import Text.Stache.Scan (Token(..))
}

%name parseRev
%tokentype { Token }
%error { parseError }

%token
      raw              { TokenRaw $$        }
      if               { TokenIfOpen $$     }
      elif             { TokenElif $$       }
      else             { TokenElse          }
      endif            { TokenIfClose       }
      each             { TokenEachOpen $$   }
      endeach          { TokenEachClose     }
      escape           { TokenEscaped $$    }
      string           { TokenLit $$        }

%%

chunks : Template        { [$1]     }
       | chunks Template { $2 : $1  }

ifprod : if chunks          { [($1, reverse $2)]    }
       | ifprod elif chunks { ($2, reverse $3) : $1 }

Template : raw                      { Raw $1                                }
         | ifprod endif             { If (b2t $1)                           }
         | ifprod else chunks endif { IfElse (b2t $1) (T () (reverse $3))   }
         | each chunks endeach      { Each $1 (T () (reverse $2))           }
         | escape                   { Escape $1                             }
         | string                   { Lit $1                                }

{

b2t :: [(Path, [Base (Template ())])] -> [(Path, Template ())]
b2t = map (T () <$>) . reverse

parseError :: [Token] -> a
parseError toks = error $ "Parse error" ++ show toks

parse :: [Token] -> Template ()
parse = T () . reverse . parseRev

}
