-- EJERCICIO 1. sonCoprimos

sonCoprimos :: Integer -> Integer -> Bool

maximoComunDivisor m n | abs n > abs m = maximoComunDivisor n m
                       | n==0 = abs m
                       | otherwise = maximoComunDivisor n (mod m n)
        
sonCoprimos m n = maximoComunDivisor m n == 1


--EJERCICIO 2: es2Pseudoprimo

es2Pseudoprimo :: Integer -> Bool

menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)
menorDivisor n = menorDivisorDesde n 2

esPrimo 1 = False
esPrimo n = n == menorDivisor n
                  
esAPseudoprimo :: Integer -> Integer -> Bool

esAPseudoprimo  a n | esPrimo a == False || esPrimo n == True = False
                    | mod (a ^ (n-1) - 1) n == 0 = True
                    | otherwise = False

es2Pseudoprimo n = esAPseudoprimo 2 n


--EJERCICIO 3: cantidad3Pseudoprimos

cantidad3Pseudoprimos :: Integer -> Integer

es3Pseudoprimo n = esAPseudoprimo 3 n

cantidad3PseudoprimosAux n | es3Pseudoprimo n == True = 1
                           | otherwise = 0
                           
cantidad3Pseudoprimos n | n == 1 = 0
                        | otherwise = cantidad3Pseudoprimos (n-1) + cantidad3PseudoprimosAux (n)


--EJERCICIO 4: kesimo2y3Pseudoprimo

kesimo2y3Pseudoprimo :: Integer -> Integer

es2y3Pseudoprimo n | es2Pseudoprimo n == False && es3Pseudoprimo n == False = False
                   | otherwise = es2Pseudoprimo n == es3Pseudoprimo n
 
siguiente2y3Pseudoprimo n | es2y3Pseudoprimo (n+1) = n+1
                          | otherwise = siguiente2y3Pseudoprimo (n+1)

kesimo2y3Pseudoprimo k | k == 1 = siguiente2y3Pseudoprimo k
                       | otherwise = siguiente2y3Pseudoprimo (kesimo2y3Pseudoprimo (k-1))


--EJERCICIO5: esCarmichael

esCarmichael :: Integer -> Bool

siguienteCoprimo n k | sonCoprimos n k == True = k
                     | otherwise = siguienteCoprimo n (k+1)

kesimoCoprimo n k | k == 1 = siguienteCoprimo n 2
                  | otherwise = siguienteCoprimo n ((kesimoCoprimo n (k-1))+1)

esCarmichaelAux k n | mod ((kesimoCoprimo n k) ^ (n-1) - 1) n == 0 = True  
                    | otherwise = False

esCarmichaelAuxAux k n | kesimoCoprimo n k >= n = True
                       | esCarmichaelAux k n == True = esCarmichaelAuxAux (k+1) n
                       | otherwise = False 

esCarmichael n | esPrimo n == True = False
               | otherwise = esCarmichaelAuxAux 1 n  