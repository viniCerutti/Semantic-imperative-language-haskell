{-
    T1 - Programacao Funcional 2017/2
    Vinicius Cerutti
    Dimas Olympio
-}

module ExemplosPrograma where

import InterpLingImp
import StoreGen
{-
    Programa 01: Calculo do fatorial

    Em outra linguagem de programacao

    int num = 4;
    int result = 1;

    while(num > 0){
      result = (result * num);
      num = (num-1);
    }
-}

fatorial = "num:=4; result:=1; while (num>0) do result:= (result*num); num:=(num-1) od"
memoryFat = initial

{-
    Programa 02: Calculo da multiplicacao com apenas somas

    Em outra lingugem de programacao

      int num1=3, num2=3; // multiplicando e multiplicador da multiplicacao
      int result = 0;
     if(num1 >= 0){
        while(num1 > 0){
             result = result+num2;
             num1 = num1 - 1;
        }
     }else{
          do{
             result = result-num2;
             num1 = num1 + 1;
        }while(num1 < 0);
     }

-}

multiplicacao = "num1:=(-4); num2:=3; result:= 0; if ((num1 > 0) || (num1 == 0))"++ 
				"then while (num1 > 0) do result:=(result+num2); num1:=(num1-1) od else"++
				"do result:=(result-num2); num1:=(num1+1) while (num1 < 0) od fi"

memoryMult = initial

{-
    Programa 03: calculo da potencia 

    Em outra lingugem de programacao

     int base=-3, expoente=2; // base e expoente da potencia
     int result = 1;
     
     while(expoente > 0){
         result = base * result;
         expoente = expoente - 1;
     }
-}

potencia = "base:=(-3); expoente:=2; result:=1; while (expoente > 0) do result:=(base * result); expoente:=(expoente-1) od"

memoryPot = initial

{-
    Programa 04: calculo da divisão inteira 

    Em outra lingugem de programacao

     int num1=12, num2=3; // dividendo e divisor da divisão inteira
     int result = 0;
     
     result = num1/num2;
-}

divisao = "num1:=12; num2:=3; result:=0; result:=(num1/num2);"
memoryDiv = initial


{-
    Programa 05: Calculo do MDC (maximo divisor comum)
    Foi utilizado o algoritmo de euclides
    
    Em outra lingugem de programacao

   int num1=12, num2 = 4;
   
    while (num2 != 0){
       int temp = num2;
       num2 = num1 % num2;
       num1 = temp;
    }
-}

mdc = "num1:=12; num2:=16; while (!(num2==0)) do temp:=num2; num2:=(num1%num2); num1:=temp od"
memoryMdc = initial
