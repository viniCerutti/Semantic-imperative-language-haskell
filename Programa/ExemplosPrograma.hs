{-
    T1 - Programacao Funcional 2017/2
    Vinicius Cerutti
    Dimas Olympio
-}

module ExemplosPrograma where

import InterpLingImpHaskell
import StoreGen

{-
    Programa 01: Calculo do fatorial com apenas somas

    Em outra linguagem de programacao

      int n=4, m = 0; // n é o numero para se calcular o fatorial
      int r = 1;

      while (n > 0) {
        
        m = n;
        int t = 0;
        while(m > 0){
            t = t + r;
            m = m - 1;
        }
        r = t;
        n = n - 1;
      }

-}

memoryFat = initial

fatAtribN = Atrib "n" (L 4) -- numero que se deseja descobrir o fatorial
fatAtribM = Atrib "m" (L 0) -- variavel auxiliar para realizar a somas sucessivas
fatAtribR = Atrib "r" (L 1) -- variavel para guardar o resultado
fatBoolLoop1 = Great (V "n") (L 0) -- expressao n > 0 do primeiro laco
fatAtrib1Loop1 = Atrib "m" (V "n") -- m = n
fatAtrib2Loop1 = Atrib "t" (L 0) -- t = 0
fatBoolLoop2 = Great (V "m") (L 0) -- expressao m > 0 do segundo laco
fatAtrib1Loop2 = Atrib "t" (Add (V "t") (V "r")) -- t = t + r
fatAtrib2Loop2 = Atrib "m" (Sub (V "m") (L 1)) -- m = m - 1
fatAtrib3Loop1 = Atrib "r" (V "t") -- r = t
fatAtrib4Loop1 = Atrib "n" (Sub (V "n") (L 1)) -- n = n - 1

fatLoop1 = While fatBoolLoop1 (Seq fatAtrib1Loop1 (Seq fatAtrib2Loop1 (Seq fatLoop2 (Seq fatAtrib3Loop1 fatAtrib4Loop1)))) -- montagem do primeiro laco
fatLoop2 = While fatBoolLoop2 (Seq fatAtrib1Loop2 fatAtrib2Loop2) -- montagem do segundo laco


fatorial = Seq fatAtribN (Seq fatAtribM (Seq fatAtribR fatLoop1)) -- programa ao todo

resultFat = value (evalCommands fatorial memoryFat) "r"

{-
    Programa 02: Calculo da multiplicacao com apenas somas

    Em outra lingugem de programacao

      int n=3, m=3; // multiplicando e multiplicador da multiplicacao
      int r = 0;
     if(n >= 0){
        while(n > 0){
             r = r+m;
             n = n - 1;
        }
     }else{
          do{
             r = r-m;
             n = n + 1;
        }while(n < 0);
     }

-}

memoryMult = initial

multAtribN = Atrib "n" (L 3) -- numero multiplicando
multAtribM = Atrib "m" (L 3) -- numero multiplicador
multAtribR = Atrib "r" (L 0) -- variavel para guardar o resultado
multExp1If = Great (V "n") (L 0) -- expressao do if se n > 0
multExp2If = Equal (V "n") (L 0) -- expressao do if se n = 0    
multBoolLoop1 = Great (V "n") (L 0) -- expressao n > 0 do primeiro loop
multAtrib1Loop1 = Atrib "r" (Add (V "r") (V "m")) -- r = r + m
multAtrib2Loop1 = Atrib "n" (Sub (V "n") (L 1)) -- n = n - 1
multBoolLoop2 = Less (V "n") (L 0) -- expressao n < 0 do segundo loop
multAtrib1Loop2 = Atrib "r" (Sub (V "r") (V "m")) -- r = r + m
multAtrib2Loop2 = Atrib "n" (Add (V "n") (L 1)) -- n = n - 1


multLoop1 = While multBoolLoop1 (Seq multAtrib1Loop1 multAtrib2Loop1) -- montagem do primeiro Loop
multLoop2 = Dowhile (Seq multAtrib1Loop2 multAtrib2Loop2) multBoolLoop2 -- montagem do segundo Loop

multIf = Choice (Or' multExp1If multExp2If) multLoop1 multLoop2 -- montagem do If

multiplicao = Seq multAtribN (Seq multAtribM (Seq multAtribR multIf)) -- programa ao todo

resultMult = value (evalCommands multiplicao memoryMult) "r"

{-
    Programa 03: calculo da potencia 

    Em outra lingugem de programacao

     int b=-3, e=2; // base e expoente da potencia
     int r = 1;
     
     while(e > 0){
         r = b * r;
         e = e - 1;
     }
-}

memoryPot = initial

potAtribB = Atrib "b" (L 3) -- numero base
potAtribE = Atrib "e" (L 2) -- numero expoente
potAtribR = Atrib "r" (L 1) -- variavel para guardar o resultado
potBoolLoop = Great (V "e") (L 0) -- expressao e > 0 do loop
potAtrib1Loop = Atrib "r" (Mult (V "b") (V "r")) -- r = r * b
potAtrib2Loop = Atrib "e" (Sub (V "e") (L 1)) -- e = e - 1

potLoop = While potBoolLoop (Seq potAtrib1Loop potAtrib2Loop) -- montagem do loop

potencia = Seq potAtribB (Seq potAtribE (Seq potAtribE (Seq potAtribR potLoop))) -- programa ao todo

resultPot = value (evalCommands potencia memoryPot) "r"

{-
    Programa 04: calculo da divisão inteira 

    Em outra lingugem de programacao

     int a=12, b=3; // dividendo e divisor da divisão inteira
     int r = 0;
     
     r = a/b;
-}

memoryDiv = initial

divAtribB = Atrib "a" (L 12) -- numero dividendo
divAtribE = Atrib "b" (L 3) -- numero divisor
divAtribR = Atrib "r" (L 0) -- variavel para guardar o resultado
divInt = Div (V "a") (V "b") -- a/b

divisao = Seq divAtribB (Seq divAtribE (Atrib "r" divInt)) -- programa ao todo

resultDiv = value (evalCommands divisao memoryDiv) "r"

{-
    Programa 05: Calculo do MDC (maximo divisor comum)
    Foi utilizado o algoritmo de euclides
    
    Em outra lingugem de programacao

    int a=12, b = 9;
    a = abs(a);
    b = abs(b);
    
    while (b != 0){
       int t = b;
       b = a % b;
       a = t;
    }
-}

memoryMdc = initial

mdcAtribA = Atrib "test" (L 12) -- primeiro numero do mdc
mdcAtribB = Atrib "b" (L 9) -- segundo numero do mdc
mdcAtribAbs1 = Atrib "test" (Abs' (V "test")) -- a = abs(a);
mdcAtribAbs2 = Atrib "b" (Abs' (V "b")) -- a = abs(a);
mdcBoolLoop = No (Equal (V "b") (L 0)) -- b != 0
mdcAtrib1Loop = Atrib "t" (V "b") -- int t = b;
mdcAtrib2Loop = Atrib "b" (Mod' (V "test") (V "b")) -- b = a % b;
mdcAtrib3Loop = Atrib "test" (V "t") -- a = t;

mdcLoop = While mdcBoolLoop (Seq mdcAtrib1Loop (Seq mdcAtrib2Loop mdcAtrib3Loop)) -- montagem do Loop

mdc = Seq mdcAtribA (Seq mdcAtribB (Seq mdcAtribAbs1 (Seq mdcAtribAbs2 mdcLoop))) -- programa ao todo

resultMdc = value (evalCommands mdc memoryMdc) "test"