module Test where

import Test.HUnit
import GUIA

run = runTestTT testDigitoEnPosicion

testFibo = test[
    "fibo 1:"~:(fibo 1)~?=1,
    "fibo 3:"~:(fibo 3)~?=2,
    "fibo 5:"~:(fibo 5)~?=5
 ]

testParteEntera = test[
    "parte Entera 10.3:"~:(parteEntera 10.3)~?=10,
    "parte Entera 5.4:"~:(parteEntera 5.4)~?=5,
    "parte Entera 10.3:"~:(parteEntera 0.3)~?=0
 ]

testesDivisible = test[
    "Divisibles:"~:(esDivisible 10 2)~?=True,
    "No Divisible:"~:(esDivisible 10 3)~?=False
 ]

testSumaImpares = test[
    "Suma 3 impares:"~:(sumaImpares 3)~?=9,
    "Suma 5 impares:"~:(sumaImpares 5)~?=25
 ]

testtodosdigitosiguales = test[
    "digitos iguales 5555:"~:(todosDigitosIguales1 5555)~?=True,
    "digitos no iguales 3253:"~:(todosDigitosIguales1 3253)~?=False
 ]    

testIesimoDigito = test[
    "segundo digito 456:"~:(iesimoDigito 456 2)~?=5,
    " digito 5:"~:(iesimoDigito 5 1)~?=5

 ]

testSumaDigitos = test[
    "sumadigitos 123"~:(sumaDigitos 123)~?=6,
    "sumadigitos 430:"~:(sumaDigitos 430)~?=7
 ]

testEsCapicua = test[
    "esCapicua 1221"~:(esCapicua 1221)~?=True,
    "esCapicua 12321"~:(esCapicua 12321)~?=True,
    "esCapicua 1231"~:(esCapicua 1231)~?=False
 ]

testMenorDivisor = test[
    "menor Divisor 33"~:(menorDivisor 33)~?=3,
    "menor Divisor 54"~:(menorDivisor 54)~?=2,
    "menor Divisor 37"~:(menorDivisor 37)~?=37
 ]

testEsPrimo = test[
    "esPrimo 33"~:(esPrimo 33)~?=False,
    "esPrimo 17"~:(esPrimo 17)~?=True,
    "esPrimo 1"~:(esPrimo 1)~?=False
 ]

testCoprimos = test[
    "sonCoprimos 3 28"~:(sonCoprimos 3 28)~?=False,
    "sonCoprimos 13 23"~:(sonCoprimos 13 23)~?=True
 ]

testNesimoPrimo = test[
    "2do primo"~:(nEsimoPrimo 2)~?=3,
    "4to primo "~:(nEsimoPrimo 4 )~?=7
 ]

testMayorDigitoPar = test[
    "ejemplo 1"~:(mayorDigitoPar 368)~?=8
    ]

testSumaHasta = test[
    "suma hasta 10"~:(sumaHasta 10 )~?=55,
    "caso base 0"~:(sumaHasta 0 )~?=0
 ]

testFactorial = test[ 
    "factorial 5"~:(factorial 5)~?=120
 ]

testInvertir = test[
 "invertir 2004"~:(invertir 2004)~?=4002,
 "invertir 5"~:(invertir 5)~?=5
 ]  

testEsCapicua2 = test[
   "esCapicua2 303"~:(esCapicua2 303)~?=True,
    "esCapicua2 2"~:(esCapicua2 2)~?=True,
    "esCapicua2 2030"~:(esCapicua2 2030)~?=False
 ]  

testContarPares = test[
    "contarPares 303"~:(contarPares 303)~?=1,
    "contarPares 24680"~:(contarPares 24680)~?=5
 ]

testmaximoDigito = test[
    "maximo digito 396"~:(maximoDigito 396)~?=9,
    "maximo digito 789"~:(maximoDigito 789)~?=9
 ]

testContarMultiplos = test[
    "contarmultiplos de 3 v1"~:(contarMultiplos3 396)~?=3,
    "contarmultiplos de 3 v2"~:(contarMultiplos3 444)~?=0
 ]

testPotencia = test[
    "potencia 2 a la 4"~:(potencia 2 4)~?=16,
    "potencia 10 a la 3"~:(potencia 10 3)~?=1000,
    "potencia 2 a la 0"~:(potencia 2 0)~?=1
 ]

testSumaDivisores = test[
    "divisores de 10"~:(sumaDivisores 10)~?=18,
    "divisores de 13"~:(sumaDivisores 13)~?=14
 ]

testContarPrimosHasta = test[
    "contarPrimos 10"~:(contarPrimosHasta 10)~?=4
 ]

testTieneDigito = test[
   "tiene digito 303 3"~:(tieneDigito 303 3)~?=True,
   "tiene digito 303 4"~:(tieneDigito 303 4)~?=False
 ]

testDigitoEnPosicion = test[
    "pos 3 en 432"~:(digitoEnPosicion 432 3)~?=1
 ]