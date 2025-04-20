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


--ejercicio9  VERIFICAR QUE ES EL UNICO QUE NO CONVENCE

esCapicua :: Int -> Bool
esCapicua n | 0 <= n && n < 10 = True
            | 10 <= n && n < 100 = (iesimoDigito n 1) == (iesimoDigito n 2) 
            | otherwise = primero == ultimo && esCapicua (sacarPrimeroYultimo n)
               where primero = (iesimoDigito n 1)
                 ultimo = mod n 10


            
 --requiere que tenga al menos 3 digitos
  
 sacarPrimeroYultimo :: Int =
 > Int
 sacarPrimeroYultimo n = eliminarUnidades (mod n (10 ^(cantidadDeDigitos n=
 1)))



--ejercicio 10a

funcion10a:: Int -> Int
funcion10a x  | x == 0 = 1
              | x== 1 = 2
              | otherwise = (2^x) + funcion10a(x-1) + 1 


--ejercicio 10b


funcion10b :: Int -> Int  -> Int
funcion10b x y | x == 0 = 0
               | y == 0 = 1
               | otherwise = x ^ y + funcion10b x (y-1) 
