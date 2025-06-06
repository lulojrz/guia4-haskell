eliminarUnidades:: Int -> Int
eliminarUnidades n = div n 10

digitoUnidades:: Int -> Int
digitoUnidades n =  mod n 10

factorial :: Int -> Int
factorial n | n == 0 = 1
            | n >0 = n* factorial(n-1)

--ejercicio 1
fibonacci :: Int -> Int
fibonacci n | n==0 = 0
            | n == 1 = 1
            | otherwise = fibonacci(n-1) + fibonacci(n-2)

--ejercicio 2
parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              |  otherwise = 1 + parteEntera(n-1) 

--ejercicio 3 
esDivisible :: Int -> Int -> Bool
esDivisible _ 0 = False
esDivisible x y | x == 0 = True
                | x < 0 = False 
                | otherwise = esDivisible(x-y) y




--ejercicio 4 
sumaImpares :: Int -> Int
sumaImpares n | n == 0 = 0
              | otherwise = (2*n-1) + sumaImpares(n-1)




--ejercicio 5
medioFact :: Int -> Int
medioFact n | n==0 = 1
            | n< 0 = 0
            | n==1 = 1
            | n>1 = n * medioFact(n-2)

--ejercicio6

todoDigitos :: Int -> Bool
todoDigitos n | n> 10 = div n 1 == mod n 10
              | otherwise = todoDigitos(eliminarUnidades n)

--ejercicio 7
cantDigitos :: Int -> Int
cantDigitos n | n <10 = 1
              | otherwise = 1 + cantDigitos(eliminarUnidades n ) 

iesimoDigito :: Int -> Int -> Int
iesimoDigito x i | i == cantDigitos x = digitoUnidades x
                 | otherwise = iesimoDigito (eliminarUnidades x) i



--ejercicio 8

sumaDigitos :: Int -> Int
sumaDigitos n | n == 0 = 0
              | otherwise = mod (abs n ) 10 + sumaDigitos(eliminarUnidades n)






--ejercicio 10a

funcion10a:: Int -> Int
funcion10a x  | x == 0 = 1
              | x== 1 = 2
              | otherwise = (2^x) + funcion10a(x-1) + 1 


--ejercicio 10b

funcion10b :: Int -> Int  -> Int
funcion10b x y | x == 0 = 0
               | y == 0 = 0 
               | otherwise = x ^ y + funcion10b x (y-1) 

--ejercicio 10c

funcion10c:: Int -> Int  -> Int
funcion10c x y | x == 0 = 0
               | y == 0 = 0 
               | otherwise = funcion10b x (2*y)

--ejercicio 10d
funcion10d :: Int -> Int  -> Int
funcion10d x y = funcion10c x y - funcion10b x (y-1)

--ejercicio 11

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n >0 = n* factorial(n-1)

eAprox:: Integer -> Float
eAprox n |n == 0 = 1
         | otherwise = 1/ fromIntegral (factorial n) + eAprox (n - 1)

e:: Float
e = eAprox 10


--ejercicio 12
a:: Integer -> Float
a 1 = 2
a n = 2 + (1 / a n-1)

raizde2 :: Integer -> Float
raizde2 n = a (n) - 1


--ejercicio 13
f :: Integer -> Integer -> Integer
f i j | i==1 = fAux 1 j 
      | otherwise = fAux i j + f(i-1) j

fAux :: Integer -> Integer -> Integer
fAux i j | j == 1 = i 
         | otherwise = i^j + fAux i (j-1)
         


--ejercicio 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q a b | a==1 = sumaPotenciasAuxiliar q 1 b
                    | otherwise = sumaPotenciasAuxiliar q a b + sumaPotencias q (a-1) b

sumaPotenciasAuxiliar :: Integer -> Integer -> Integer -> Integer
sumaPotenciasAuxiliar q a b | b==1 = q^(a+b)
                            | otherwise = q^(a+b) + sumaPotenciasAuxiliar q a (b-1)


--ejercicio 15


sumaRacionales :: Integer -> Integer -> Float
sumaRacionales p q | p == 1 = sumaRacionalesAux 1 q
                   | otherwise = sumaRacionalesAux p q + sumaRacionales (p-1) q 

sumaRacionalesAux :: Integer -> Integer -> Float
sumaRacionalesAux p q | q == 1 = fromIntegral p 
                      | otherwise = (fromIntegral p) / (fromIntegral q) + sumaRacionalesAux p (q-1) 



--ejercicio 16a
menorDivisorAux::Integer -> Integer -> Integer
menorDivisorAux n m |  mod n m == 0 = m
                    | otherwise = menorDivisorAux n (m+1) 

menorDivisor:: Integer-> Integer
menorDivisor n = menorDivisorAux n 2

--ejercicio  16b
esPrimo::Integer -> Bool
esPrimo x | menorDivisor x == x = True 
          | otherwise= False 

--ejercicio 16c 
sonCoprimos:: Integer -> Integer -> Bool
sonCoprimos a b | menorDivisor a  == menorDivisor b  = True
                | otherwise = False

--ejercicio 16d
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 0 2


nEsimoPrimoAux ::Integer -> Integer -> Integer -> Integer
nEsimoPrimoAux n cont aux | esPrimo aux && cont == n-1 = aux 
                          | esPrimo aux = nEsimoPrimoAux n (cont+1) (aux +1)
                          | otherwise = nEsimoPrimoAux n cont (aux+1)  



--ejercicio 17
esFibonacci :: Integer -> Bool
esFibonacci n = aux n 0

aux :: Integer -> Integer -> Bool
aux n cont | n == fibonacci cont = True
           | fibonacci cont > n = False
           | otherwise = aux n (cont+1)
        

fibonacci :: Integer -> Integer
fibonacci n | n==0 = 0
            | n==1 = 1
            | otherwise = fibonacci(n-1) + fibonacci(n-2)




--ejercicio 18
esPar :: Int -> Bool
esPar n = mod n 2 == 0

sacarUltimo :: Int -> Int
sacarUltimo n = div n 10 

digito :: Int -> Int
digito n = mod n 10

mayorDigitoPar :: Int -> Int
mayorDigitoPar n = mayorDigitoParAux n (-1)

mayorDigitoParAux :: Int -> Int -> Int
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

--[20]
tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n m | (n == m) = sumaDivisores(n)
                 | otherwise = max (sumaDivisores (n)) (tomaValorMax (n+1) m)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresDesde n 1

sumaDivisoresDesde :: Integer -> Integer -> Integer
sumaDivisoresDesde n k | (k == n) = n 
                       | (k < n) && (mod n k == 0) = k + sumaDivisoresDesde n (k+1)
                       | (k > n) = 0
                       | otherwise = sumaDivisoresDesde n (k+1)


--21 
pitagoras :: Int -> Int -> Int -> Int --pensar el ejercicio como una doble sumatoria
pitagoras m n r | m == 0 = pitagorasAux m n r 
                | otherwise = pitagorasAux m n r + pitagoras (m-1) n r 

pitagorasAux :: Int -> Int -> Int -> Int
pitagorasAux m n r | n == 0 = pitagorasValor m n r 
                   | otherwise = pitagorasValor m n r + pitagorasAux m (n-1) r 

pitagorasValor :: Int -> Int -> Int -> Int
pitagorasValor p q r | p^2 + q^2 <= r^2 = 1
                     | otherwise = 0


