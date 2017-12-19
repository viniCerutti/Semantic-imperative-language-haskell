# T2 - Programação Funcional #
# Descrição #

Trabalho II da Disciplina Programação Funcional - 2017/2 PUCRS Ministrada pelo professor Alfio Martini. Com objetivo de dar continuidade ao [trabalho I](https://github.com/viniCerutti/T1-Programacao-Funcional) que foi definido uma semântica para linguagem de programação imperativa. Neste segundo trabalho foi definido uma sintaxe para linguagem imperativa que compute algoritmos sobre inteiros com a linguagem Haskell. A linguagem deve ter comandos para modelar atribuição, sequência, escolha, comando neutro (que não faz nada), e pelo menos dois tipos de comandos para laços.

# Autores #
Dimas Olympio

Vinicius Cerutti

# Sobre os Arquivos #
### Programa (Pasta) ###
Pasta que contem os arquivos para executar o programa onde:
* AvalLing.hs - São os datatypes que definem a semântica e os avaliadores que os executam.
* ExemplosPrograma.hs - Apresenta exemplos de programas para serem executados de acordo com a semântica criada.
* InterpLingImp.hs - Arquivo que realiza a união do parser (sintaxe) com o avalidor (semântica), ou seja, o interpretador da linguagem criada.
* ParserSt.hs - Arquivo que realiza o parser de Strings(sintaxe) para a semântica criada.
* StoreGen.hs - Estrutura de Dados na forma de tuplas onde o primeiro elemento é o nome da váriavel (index) e segundo é o valor da váriavel.

 ### EnunciadoTarefa.pdf (arquivo Pdf) ###
 Enunciado do trabalho.
 
 # Programa para compilar e executar o Haskell #
 [GHCI 8.2.2](https://www.haskell.org/platform/)
