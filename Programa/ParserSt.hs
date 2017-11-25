module ParserSt where

import InterpLingImpHaskell
import StoreGen
{-
Um parser é um programa que toma um string de caracteres e produz alguma fo
de árvore que torna explícita a estrutura sintática de um string.

Exemplo: string = 2*3+4 tree= Plus (Times (L 2) (L3)) (L 4)

Portanto, 

type Parser = String -> Tree

Entretanto, um parser em geral, pode não consumir todo o string. Desta forma,

type Parser = String -> (Tree, String)

Além disso, um parser nem sempre sucede. Por exemplo, um parser para números
aplicado a um string. Poratnto, um parser pode ser do tipo:

type Parser = String -> [(Tree, String)]

com a convenção de que uma lista vazia denota um fracasso e uma lista unitária
representa sucesso.

Finalmente, diferentes parser (para números, identificadors, caracteres, etc) 
retornam diferentes tipos de árvores. Desta forma, deixamos o tipo específico
de árvore como um parâmetro.
-}

type Parser treeType = String -> [(treeType, String)]

{-
Parsers Elementares
-}

-- return v sempre sucede, retornando v sem consumir o string s
returnP:: a -> Parser a 
returnP v = \s -> [(v,s)]

retex01 = returnP 2 "abc"
retex02 = returnP 'a' "hello"

-- failure sempre fracassa, retornando a lista vazia
failure::Parser a 
failure = \s -> [] 

faiex01 = failure "hello"

-- fracassa se o string é vazio. Do contrário, retorna o primeiro caracter.
item::Parser Char
item = \s -> case s of 
	           "" -> []
	           (x:xs) -> [(x,xs)]

itex01 = item "hello"

-- parser application function
parse::Parser a -> String -> [(a,String)]
parse p inp = p inp 

parex01 = parse (returnP 2) "Hello"
parex02 = parse item "Hello"
parex03 = parse failure "Hello"

{-
  Composição ou Sequência de Parsers

  parser p fracassa se a aplicação do parser p ao string
  de entrada falha. De outra forma, aplica a função ao
  valor resultado para retornar um segundo parser, o qual
  aplicado ao string resultado retorna o valor final.
-}

(>=>)::Parser a -> (a -> Parser b) -> Parser b
p >=> f = \s -> case parse p s of 
	              []-> [] 
	              [(v,out)]-> parse (f v) out




par01 :: Parser(Char,Char)
par01 = item >=> \v1 -> 
        item >=> \v2 -> returnP (v1,v2)
        
        {-
    f = \v1 -> item >=> \v2 -> returnP (v1,v2)
    g= \v2 -> returnP('h',v2)
    parse par01 "hello"
    = par01 "hello" (def. parse)
    = (item >=> f) "hello" (def. parse01 e f)
    = (\s -> case parse item s of 
      []  -> []
      [(v,out)] -> parse (f v) out) "hello"  (def >=>)
    = case parse item "hello" of 
    [] -> []
    [(v,out)] -> parse (f v ) out 
    =  case [('h', "ello")]  of
    [] -> []
    [(v,out)] -> parse (f v ) out 
    = parse (f 'h')  "ello"  (def. >=> e parse item "hello"=['h',"ello"])
    = parse (item >=> \v2 -> returnP('h',v2))  "ello" (def de f e aplicação)
    = (item >=> \v2 -> returnP('h',v2))  "ello"  (def. parse)
    =  (\s -> case parse item s of 
      []  -> []
      [(v,out)] -> parse (g v) out) "ello"  (def >=>)
    =  case parse item "ello" of 
      []  -> []
      [(v,out)] -> parse (g v) out 
    =   case [('e',"llo")] of 
    []  -> []
    [(v,out)] -> parse (g v) out 
    = parse (g 'e')  "llo"
    = parse (returnP('h','e')) "llo"
    = returnP ('h','e') "llo"
    = [(('h','e'),"llo")]
-}
par02 = item >=> \v1 -> 
        item >=> \v2 -> 
        item >=> \v3 -> returnP(v1,v2,v3)

par03 = par01 >=> \(v1,v2) -> 
        item >=> \v3 -> returnP(v1,v2,v3)

exseq01 = parse par01 "Hello"
 
{-
Parser escolha: aplica o primeiro parser ao string de entrada e se
este falha, então aplica o segundo.
-} 

(+++):: Parser a -> Parser a -> Parser a 
p +++ q = \s -> case parse p s of 
	             [] ->  parse q s 
	             [(v,out)] -> [(v,out)]

exch01 = parse (item +++ returnP 'd') "abc"
exch02 = parse (failure +++ returnP 'd') "abc"
exch03 = parse (failure +++ failure) "abc"

{-
Parser derivados
-}

isDigit,isLower,isUpper,isAlpha::Char -> Bool 
isDigit c = elem c ['0'..'9']
isLower c = elem c ['a'..'z']
isUpper c = elem c ['A'..'Z']
isAlpha = \c -> isLower c || isUpper c
isAlphaNum = \c -> isAlpha c || isDigit c
isSpace = \c -> c == ' '



sat::(Char -> Bool) -> Parser Char
sat pred =  item >=> 
              \x -> if pred x then returnP x else failure

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphaNum = sat isAlphaNum
matchSpace = sat isSpace


char::Char -> Parser Char 
char x = sat (==x)

exsat01 = parse digit "123"
exsat02 = parse digit "abc"
exsat03 = parse (char 'a') "abc"


string:: String -> Parser String 
string [] = returnP [] 
string (x:xs) = char x >=> \c -> 
                string xs >=> \rst -> returnP (c:rst)

exstr01 = parse (string "alfio") "alfio martini"

{-
Repetition 
-}
-- many 0 ou mais
many, many1::Parser a -> Parser [a]
many p = many1 p +++ returnP [] 

-- many1 1 ou mais
many1 p = p >=> \v -> 
          many p >=> \vs -> returnP (v:vs)

exmany01 = parse (many digit) "123abc"
exmany02 = parse (many letter) "123abc"
      
-- verifica se a string comeca com um letra
ident::Parser String
ident = letter >=> \x -> many alphaNum >=> \xs -> returnP (x:xs)


-- read converte string para o tipo que queremos
-- (read "49")::Integer
nat::Parser Integer
nat = many1 digit >=> \xs -> returnP (read xs)

natS::Parser String
natS = many1 digit >=> \xs -> returnP xs

space::Parser ()
space = many matchSpace >=> \xs -> returnP ()

der01 = parse space "    alfio"
der02 = parse ident "12345    alfio"
der03 = parse (nat >=> \num -> 
        space >=> \sp -> 
        ident >=> \id -> returnP (num,id)) "12345    alfio"

{-
token t ignora espaços antes e depois de t
-}

token :: Parser a -> Parser a 
token t = space >=> \_ -> 
          t >=> \v ->
          space >=> \_ -> returnP v 

identifier:: Parser String
identifier = token ident 

natural::Parser Integer 
natural = token nat

symbol:: String -> Parser String 
symbol xs = token (string xs)


--javaId::Parser String
--javaId = (letter +++ underSc) >=> \c ->
--         (many (alphaNum +++ underSc)) >=> \xs ->
--        returnP (c:xs)


-- E := (E+E) | num 
parNum::Parser Integer
parNum = (symbol ("(") >=> \_ -> 
    parNum >=> \n1 -> 
    symbol ("+") >=> \_ ->
    parNum >=> \n2 -> 
    symbol (")") >=> \_ -> 
    returnP (n1+n2)) +++ (natural >=> \n -> returnP n)
    
-- E ::= (E+E) | (E-E) | (E * E) | Num ou E::= (E bop E) | Num


-- E ::= (E+E) | (E-E) | (E * E) | Num | Var
-- parser a) b) c), so que do tipo Parse ArtExp do trabalho 

{-
	Grammer for parseAriExp
	parseAritExp -> (parseAritExp foundSymbolArith parseAritExp) | varOrLiteral
	varOrLiteral -> natural | identifier
	foundSymbolArith -> "+"|"-"|"/"|"*"|"%"
-}
parseAritExp::Parser AritExp
parseAritExp = (symbol("(") >=> \_ ->
                parseAritExp>=> \n1 ->
                foundSymbolArith >=> \op ->
                parseAritExp >=> \n2 -> 
                symbol (")") >=> \_ -> 
                returnP (op n1 n2)) +++ (varOrLiteral >=> \n -> returnP n)
                
  where
      foundSymbolArith::Parser (AritExp -> AritExp -> AritExp) 
      foundSymbolArith = (symbol("+") >=> \_ -> returnP Add) 
                      +++(symbol("-") >=> \_ -> returnP Sub) 
                      +++(symbol("/") >=> \_ -> returnP Div) 
                      +++(symbol("*") >=> \_ -> returnP Mult) 
                      +++(symbol("%") >=> \_ -> returnP Mod')
                      -- faltou tipo absoluto
      varOrLiteral::Parser AritExp
      varOrLiteral = (natural >=> \n -> returnP (L n))+++(identifier >=> \atrib -> returnP (V atrib))

{-
	Grammer for parseLogicExp
	parseLogicExp -> (parseLogicExp foundLogicSymbol parseLogicExp) | boolLogic | (parseAritExp foundLogArithSym parseAritExp) | "!" parseLogicExp
	foundLogArithSym -> ">" | "<" | "=="
	foundLogicSymbol -> "&&" | "||"
-}

parseLogicExp::Parser BoolExp
parseLogicExp = (symbol("(") >=> \_ ->
                parseLogicExp>=> \n1 ->
                foundSymbolLogic >=> \op ->
                parseLogicExp >=> \n2 -> 
                symbol (")") >=> \_ -> 
                returnP (op n1 n2)) 
        				+++ (boolLogic >=> \n -> returnP n) 
        				+++(symbol("(") >=> \_ ->
        				parseAritExp >=> \n1 ->
        				foundLogArithSym >=> \op -> 
        				parseAritExp >=> \n2 -> 
        				symbol (")") >=> \_ ->
        				returnP (op n1 n2))
                +++(symbol ("!") >=> \_ ->
                parseLogicExp >=> \n1 ->
                returnP (No n1)) 
  where
      boolLogic::Parser BoolExp
      boolLogic = (symbol ("true") >=> \_ -> returnP (B True))
                  +++ (symbol ("false") >=> \_ -> returnP (B False))
                  
      foundSymbolLogic::Parser (BoolExp -> BoolExp -> BoolExp) 
      foundSymbolLogic = (symbol("&&") >=> \_ -> returnP And')
      				  +++(symbol("||") >=> \_ -> returnP Or')
      
      foundLogArithSym::Parser (AritExp -> AritExp -> BoolExp) 
      foundLogArithSym = (symbol(">") >=> \_ -> returnP Great)
            				  +++(symbol("<") >=> \_ -> returnP Less)
      					      +++(symbol("==") >=> \_ -> returnP Equal)

parseCommands::Parser Commands
parseCommands =   (atrib >=> \atrib -> returnP atrib)
                  +++(while >=> \while -> returnP while)
                  +++(doWhile >=> \doWhile -> returnP doWhile)
                  +++(choice >=> \choice -> returnP choice)
                  +++(returnP Nop)
    where
      atrib::Parser Commands
      atrib = (identifier >=> \name -> symbol(":=") >=> \_ -> parseAritExp >=> \exp -> returnP (Atrib name exp))
      while::Parser Commands
      while = (symbol("while") >=> \_ ->
              parseLogicExp >=> \expBool ->
              symbol("do") >=> \_ ->
              parseProgram >=> \comd ->
              symbol("od") >=> \_ ->
              returnP (While expBool comd))

      doWhile::Parser Commands
      doWhile = (symbol("do") >=> \_ ->
                parseProgram >=> \comd ->
                symbol("while") >=> \_ ->
                parseLogicExp >=> \expBool ->
                symbol("od") >=> \_ ->
                returnP (Dowhile comd expBool))

      choice::Parser Commands
      choice = (symbol("if") >=> \_ ->
                parseLogicExp >=> \expBool ->
                symbol("then") >=> \_ ->
                parseProgram >=> \comd1 ->
                symbol("else") >=> \_ ->
                parseProgram >=> \comd2 ->
                symbol("fi") >=> \_ ->
                returnP(Choice expBool comd1 comd2))
                +++(symbol("if") >=> \_ ->
                parseLogicExp >=> \expBool ->
                symbol("then") >=> \_ ->
                parseProgram >=> \comd ->
                symbol("fi") >=> \_ ->
                returnP(Choice expBool comd Nop))

parseProgram::Parser Commands
parseProgram = (parseCommands >=> \comd1 ->
               symbol(";") >=> \_ ->
               parseProgram >=> \comd2 ->
               returnP (Seq comd1 comd2))
               +++(parseCommands >=> \comd -> returnP comd)


{-
Exercicio:

Um parser para uma lista de naturais, onde espaços ocorrem livremente
antes e depois do colchentes, dos números e das vírgulas.

parseLNat " [ 1 , 234  , 56 ] " = [([1,234,56],"")]
-}
parseLNat::Parser [Integer]
parseLNat = (symbol ("[") >=> \_ ->
        natural >=> \n ->
        many(symbol(",") >=>  \_ -> natural) >=> \n2 ->
        symbol ("]") >=> \_ -> returnP(n:n2))

{-
    S1 -> [A]
    A -> Num B
    B -> & | , Num B
-}
s1:: Parser[Integer]
s1 = (symbol ("[") >=> \_ -> 
      a >=> \n -> symbol ("]") >=> \_ -> 
      returnP n)
    where
      a = (natural >=> \n ->
          b >=> \rst ->
          returnP (n:rst))
      b = (symbol(",") >=> \_ ->
          natural >=> \n ->
          b >=> \rst ->
          returnP (n:rst))
          +++ returnP []
     
 
recInteger::Parser Integer
recInteger = (natural >=> \n -> returnP n) 
                +++ 
            (symbol("(") >=> \_ -> 
            symbol("-") >=> \_ -> 
            natural >=> \n  -> 
            symbol(")") >=> \_ -> 
            returnP ((-n)))

