module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser
import Control.Applicative
import Data.Char

parseProgram :: String -> Maybe [Expression]
parseProgram "" = Nothing
parseProgram codeText = 
    let  
        
        parseResult Nothing (Just (expr, text)) = loop (Just [expr]) text 
        parseResult acc (Just (expr, text)) = loop ((expr:) <$> acc) text
        parseResult acc Nothing = acc

        loop acc text = case runParser eof text of
                            Just ((), "") -> reverse <$> acc
                            Nothing -> parseResult acc $ runParser oneLine text
    in 
        loop Nothing codeText

{-| 
    Parse any whitespace at the end of the line
-}
lineEnding :: Parser [Char]
lineEnding = many (spot isSpace)

{-| 
    Parse one line until the 
-}
oneLine :: Parser Expression
oneLine = (definition <|> application) <* lineEnding

{-|
    Any alphanumeric character + '#' + '''
-}
caseInsensitiveAlfaNumeric :: Parser Char
caseInsensitiveAlfaNumeric = letter <|> spot isDigit <|> spot (`elem` "#'")

{-|
    A chunk of string that contains alphanumeric 
-}
caseInsensitiveName :: Parser String
caseInsensitiveName = some caseInsensitiveAlfaNumeric

{-| 
    Parse a variable

    Example:

    >>> runParser variable " y) false)"
    Just (Var { txt = "y" }, "). false)")
-}
variable :: Parser Expression
variable = Var <$> caseInsensitiveName


{-| 
    Parse a lambda expression

    Example:

    >>> runParser lambda "\x.\y.\z.t"
    Just (Lambda {
        arg = "x",
        body = Lambda {
            arg = "y",
            body = Lambda {
                arg = "z",
                body = Var { txt = "t" }
            }   
        }
    })
-}
lambda :: Parser Expression
lambda = Lambda <$> (token '\\' *> caseInsensitiveName)
                <*> (token '.' *> anyLambdaExpression)

{-|
    Parse an application expression

    Example:

    >>> runParser application "(x y) false)"
    Just (Application {
        expr1 = Var { txt = "x" },
        expr2 = Var { txt = "y"}
    }, " false")
-}
application :: Parser Expression
application = Application <$> (token '(' *> anyLambdaExpression)
                          <*> (token ' ' *> anyLambdaExpression <* token ')')  


{-|
    Parse any valid lambda-expression: variable OR lambda or application
-}
anyLambdaExpression :: Parser Expression
anyLambdaExpression = variable <|> lambda <|> application

{-|
    Parses a definition (i.e. top level variable bindings)  

    Example:

    >>> runParser definition "true=\x.\y.x"
    Just (Definition { 
        name = "true", 
        expr = Lambda { 
            arg = "x", 
            body = Lambda { 
                arg = "y", 
                body = Var { txt = "x" }
            }
        }
    })  
     
-}
definition :: Parser Expression
definition = Definition <$> (caseInsensitiveName <* token '=')
                        <*> anyLambdaExpression

