{-
    T1 - Programacao Funcional 2017/2
    Vinicius Cerutti
    Dimas Olympio
-}

module InterpLingImp where

import AvalLing
import ParserSt
import StoreGen

run:: String -> Store Var ValueVar -> IO()
run string memory = pretty_printing progAfterParse memory
    where
        progAfterParse = fst (head(parseProgram (string)))

-- Funcao para imprimir os valores das variaveis que foram
-- executados durante do programa, onde a impressao informa
-- o valor e a variavel antes e depois da execucao do programa

pretty_printing :: Commands -> Store Var ValueVar -> IO()
pretty_printing prog store = putStrLn (formatText memIniVariables memFinVariavles)
        where
            progVariables = getVars prog
            memProg = evalCommands prog store
            memFinVariavles = getVarMem progVariables memProg 
            memIniVariables = getVarMem progVariables store 

            -- Funcao que formata o texto para impressao
            -- onde primeiro se imprime a memoria antes de executar 
            -- o programa e depois separada por traços (---) a impressao
            -- da memoria depois da execucao do programa

            formatText:: [(Var,ValueVar)] -> [(Var,ValueVar)] -> String
            formatText storeI storeF = traceStr++"\nStoreInicial = "++storeIni++"\n"++traceStr++"\nStoreFinal = "++storeFin++"\n"++traceStr
                where
                    storeIni = show storeI
                    storeFin = show storeF
                    traceNumber = max (length storeIni) (length storeFin)
                    traceStr = copyChar '-' traceNumber

                    -- Funcao que copia o caractere informando n vezes
                    copyChar:: Char -> Int -> String
                    copyChar c num
                        | num <=0 = ""
                        |otherwise = copyChar c (num-1)++[c]

-- Funcao que retorna uma lista das variaveis 
-- utilizadas no programa
getVars:: Commands -> [Var]
getVars comd =  (nub . getVarsAux) comd
    where
        getVarsAux (Nop) = []
        getVarsAux (Atrib name val) = [name]
        getVarsAux (Seq comd1 comd2) =  (getVarsAux comd2)++(getVarsAux comd1)
        getVarsAux (Choice _ comd1 comd2) = (getVarsAux comd1)++(getVarsAux comd2)
        getVarsAux ( While _ comd) = getVarsAux comd
        getVarsAux ( Dowhile comd _ ) = getVarsAux comd

-- Funcao que retorna apenas uma lista da memoria
-- com os valores das variaveis informadas

getVarMem::[Var] -> Store Var ValueVar-> [(Var,ValueVar)]
getVarMem [] _ = []
getVarMem (name:xs) store = (name,number):getVarMem xs store
    where
        number = value store name