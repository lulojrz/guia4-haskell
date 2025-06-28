module GUIA where
--GUIA 4
--ejercicio 1
fibo::Integer ->Integer
fibo n | n==0 = 0
       | n==1 = 1
       |otherwise = fibo(n-1) + fibo(n-2)

--ejercicio 2
parteEntera :: Float->Integer
parteEntera n | n<1 = 0
              | otherwise = 1 + parteEntera (n-1)

--ejercicio 3 sin mod ni div
esDivisible :: Integer->Integer->Bool 
esDivisible x y | x== 0 = True
                | x < y =False
                | otherwise = esDivisible (x-y) y

--ejercicio 4
sumaImpares :: Integer->Integer 
sumaImpares n | n==0 = 0
              | otherwise= (2*n - 1) + sumaImpares (n-1)

--ejercicio 6
todosDigitosIguales1 :: Integer->Bool 
todosDigitosIguales1 n = mod n 11 ==0

--ejercicio 7
--si el numero de digito es igual a la cantidad de digitos de ese numero 
-- devuelve el ultimo digito
--por ejemplo 4567 tiene 4 digitos y pasamos como parametro 4, devolvera 7
-- en cambio si no lo es, vamos eliminando unidades, hasta que el parametro 
--sea igual a la cantidad de digitos
iesimoDigito::Integer->Integer->Integer
iesimoDigito n i| i== cantDigitos n = ultimoDigito n
                | otherwise = iesimoDigito (eliminarUnidades n) i

ultimoDigito::Integer->Integer
ultimoDigito n  = mod n 10

eliminarUnidades::Integer->Integer
eliminarUnidades n = div n 10

cantDigitos::Integer->Integer
cantDigitos n |  n< 10 =1
              | otherwise = 1+ cantDigitos(eliminarUnidades n)


--ejercicio 8
sumaDigitos :: Integer->Integer 
sumaDigitos n | n < 10  = n
              | otherwise = ultimoDigito(n) + sumaDigitos(eliminarUnidades n)

--ejercicio 9
primerDigito::Integer->Integer 
primerDigito n | n < 10  = n
               | otherwise = primerDigito(eliminarUnidades n  )

esCapicua::Integer->Bool
esCapicua n |0<= n && n<10 =True
            |10<= n && n<100 = primerDigito (n) == ultimoDigito(n)
            |otherwise = primero == ultimo && esCapicua (sacarPrimeroYultimo n)
             where primero = primerDigito n
                   ultimo = ultimoDigito n
    
sacarPrimeroYultimo :: Integer -> Integer
sacarPrimeroYultimo n = eliminarUnidades (mod n (10 ^(cantDigitos n-1)))


--ejercicio 16a
menorDivisor:: Integer->Integer
menorDivisor x = menorDivisoraux x 2

menorDivisoraux:: Integer->Integer->Integer
menorDivisoraux n cont  | mod n cont == 0 = cont
                        | otherwise = menorDivisoraux n (cont+1) 

--ejercicio 16b
esPrimo :: Integer->Bool
esPrimo  1 = False
esPrimo n = menorDivisor n == n

--ejercicio 16c
sonCoprimos:: Integer->Integer->Bool
sonCoprimos x y = menorDivisor x ==x && menorDivisor y == y

--ejercicio 16d
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 0 2
--le enviamos a la funcion auxiliar el n para saber el primo numero n, 0 para iniciar 
--contador y 2 que es el primer primo

nEsimoPrimoAux ::Integer -> Integer -> Integer -> Integer
--si el auxiliar es primo y el contador es igual a n-1, el nesimo primo sera el aux
--por ejemplo queremos el 1er primo, aux = 2 es primo, y cont = 0 = 1-1
--en cambio si el cont no es igual aumentamos ambos parametros
--si no es primo, solo aumentamos el contador
nEsimoPrimoAux n cont aux | esPrimo aux && cont == n-1 = aux 
                          | esPrimo aux = nEsimoPrimoAux n (cont+1) (aux +1)
                          | otherwise = nEsimoPrimoAux n cont (aux+1) 



--ejercicio 18
esPar :: Integer -> Bool
esPar n = mod n 2 == 0

sacarUltimo :: Integer -> Integer
sacarUltimo n = div n 10 

digito :: Integer -> Integer
digito n = mod n 10

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = mayorDigitoParAux n (-1)

mayorDigitoParAux :: Integer -> Integer -> Integer
mayorDigitoParAux n max | n == 0 = max
                        | esPar (digito n) && digito n > max = mayorDigitoParAux (sacarUltimo n) (digito n)
                        | otherwise = mayorDigitoParAux (sacarUltimo n) max 


--ejercicio 19
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 1

esSumaInicialDePrimosDesde :: Integer -> Integer -> Bool
esSumaInicialDePrimosDesde n i | sumaKprimos (i) > n = False
                               | sumaKprimos (i) < n = esSumaInicialDePrimosDesde n (i+1)
                               | otherwise = True

sumaKprimos :: Integer -> Integer
sumaKprimos 1 = 2
sumaKprimos n = sumaKprimos (n-1) + nEsimoPrimo n


--practicando recursion sobre enteros
sumaHasta::Integer-> Integer
sumaHasta 0 = 0
sumaHasta n = n + sumaHasta(n-1)

factorial::Integer-> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)


invertir::Integer-> Integer
-- en esta funcion sacamos el ultimo digito con la funcion digito y lo multiplicamos 
--por la cantidad de digitos -1, por ejemplo ingresamos 204, al 4 lo multiplicamos por 100 o sea 10**2
--luego sacamos el ultimo numero y llamamos de vuelta a la misma funcion
invertir 0 = 0
invertir n | n<10 = n
           | otherwise = (digito(n))* (10^cantDigitos(div n 10)) + invertir(sacarUltimo n )



esCapicua2::Integer->Bool
esCapicua2 n = invertir n == n


contarPares :: Integer -> Integer
contarPares 0 = 0
contarPares n  
               | esPar(digito n )==False = 0 + contarPares(sacarUltimo n)
               | esPar(digito n) == True = 1 + contarPares(sacarUltimo n)


maximoDigito :: Integer -> Integer
maximoDigito 0 = 0
maximoDigito n = maximoDigitoAux n 0

maximoDigitoAux :: Integer -> Integer->Integer
maximoDigitoAux n aux |n ==0 = aux
                      |digito(n)> aux = maximoDigitoAux (sacarUltimo n) (digito(n))
                      | otherwise = maximoDigitoAux(sacarUltimo n) aux
              
                      
contarMultiplos3 :: Integer -> Integer
contarMultiplos3 0 = 0
contarMultiplos3 n | multiplode3 (digito n) == True = 1+contarMultiplos3(sacarUltimo n)
                   | multiplode3 (digito n) == False = 0+contarMultiplos3(sacarUltimo n)

multiplode3:: Integer -> Bool
multiplode3 n  = mod n 3 == 0


mcd:: Integer-> Integer-> Integer
mcd x 0 = x
mcd x y = mcd y (mod x y)


potencia:: Integer-> Integer-> Integer
potencia x 0 = 1
potencia x pot = x * potencia x (pot-1)


esDivisor :: Integer-> Integer-> Bool
esDivisor x y = mod x y == 0

--sumamos los divisores excepto 0
sumaDivisores:: Integer-> Integer
sumaDivisores 1  = 1
sumaDivisores x = sumaDivisoresaux x 1

sumaDivisoresaux:: Integer-> Integer-> Integer
sumaDivisoresaux x cont | x < cont = 0
                        | mod x cont==0 = cont+ sumaDivisoresaux x (cont+1)
                        | mod x cont /= 0 = 0+ sumaDivisoresaux x (cont+1)


contarPrimosHasta :: Integer -> Integer
contarPrimosHasta 0 = 0
contarPrimosHasta n  | esPrimo(n)==True = 1+ contarPrimosHasta (n-1)
                     | esPrimo(n)==False = 0+ contarPrimosHasta (n-1)

tieneDigito :: Integer -> Integer -> Bool
tieneDigito 0 dig = False
tieneDigito n dig | digito(n)== dig = True
                  | otherwise = tieneDigito (sacarUltimo n ) dig


digitoEnPosicion :: Integer -> Integer -> Integer
digitoEnPosicion num dig = digitoEnPosicionaux num dig 0

digitoEnPosicionaux:: Integer -> Integer -> Integer -> Integer
digitoEnPosicionaux 0 dig cont = -1
digitoEnPosicionaux num dig cont |  digito(num)==dig = cont
                                | otherwise = 1 + digitoEnPosicion (sacarUltimo num) dig


