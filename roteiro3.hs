calcula_dist :: (Float,Float) -> (Float,Float) -> Float
calcula_dist (xa,xb) (ya,yb) = sqrt((xb-xa)^2 + (yb-ya)^2)

fatorial_1 :: Integer -> Integer
fatorial_1 x    | x < 2 = 1
                | otherwise = x * fatorial_1(x-1)

fatorial_2 :: Integer -> Integer
fatorial_2 x = product [1..x]

fibo :: Integer -> Integer
fibo x  | x == 0 = 0
        | x == 1 = 1
        | otherwise = fibo(x-2) + fibo(x-1)

n_tri :: Integer -> Integer
n_tri x | x == 0 = 0
        | otherwise = x * (x + 1) `div` 2

potencia2 :: Integer -> Integer
potencia2 x | x == 0 = 1
            | otherwise = 2^(x-1) * 2        


prod_i :: Integer-> Integer-> Integer
prod_i m n 
        | n < m = 0
        | m == n = m
        | m+1 == n = n*m
        | otherwise = m * prod_i (m+1) (n-1) * n

fatorial_prod_i :: Integer -> Integer
fatorial_prod_i x 
        | x<0 = 0
        | x==1||x==0 = x
        | otherwise = (prod_i 1 x)

resto_div :: Integer -> Integer -> Integer
resto_div m n 
        | m < n = m
        | otherwise = resto_div (m-n) n

div_inteira :: Integer -> Integer -> Integer
div_inteira m n 
        | m < n = m*n
        | otherwise = (resto_div m n)*n        

mdc_guardas :: Integer -> Integer -> Integer
mdc_guardas m n 
        | n == 0 = m
        | otherwise = mdc_guardas n (resto_div m n)

mdc_padrao :: Integer -> Integer -> Integer
mdc_padrao m 0 = m
mdc_padrao m n = mdc_padrao n (resto_div m n)

binomial :: Integer -> Integer -> Integer
binomial n k 
        | k == 0 || k == n = 1
        | k > n = undefined
        | otherwise = (binomial (n-1) k) + (binomial (n-1) (k-1))

binomial_2 :: Integer -> Integer -> Integer
binomial_2 n k = if(k==0 || k==n) then 1 else if(k>n) then undefined else (binomial (n-1) k) + (binomial (n-1) (k-1))

passo::(Integer,Integer) -> (Integer,Integer)
passo (x,y) = (y,x+y)

fibo_a::Integer -> (Integer,Integer)
fibo_a n 
  |n == 0 = (0,1)
  |otherwise = passo (fibo_a (n-1))

fibo2::Integer->Integer
fibo2 n = fst(fibo_a n)