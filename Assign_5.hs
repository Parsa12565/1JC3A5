{- Assignment 5
 - Name: Parsa Zanganeh
 - Date: 2020-12-07
 -}
module Assign_5 where

macid :: String
macid = "zanganep"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: computes an approximation of the definite integral of a function using the trapezoidal rule
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = definiteIntegralAux a b g n n
 where 
 definiteIntegralAux :: Double -> Double -> (Double -> Double) -> Integer -> Integer -> Double
 definiteIntegralAux a b g n i = 
  if i==1
   then (g (a+fromInteger i*x) + g (a+fromInteger (i-1)*x))/2*x
   else (g (a+fromInteger i*x) + g (a+fromInteger (i-1)*x))/2*x + definiteIntegralAux a b g n (i-1)
 x=(b-a)/fromInteger n

{- -----------------------------------------------------------------
 - arcsin1
 - -----------------------------------------------------------------
 - Description: computes an approximation of arcsin 1 using definiteIntegral
 -}
arcsin1 :: Integer -> Double
arcsin1 = definiteIntegral (-1) 1 g
 where g= \x->sqrt (1-(x**2))

{- -----------------------------------------------------------------
 - piApprox
 - -----------------------------------------------------------------
 - Description: uses arcsin1 to approximate pi within a tolerance
 -}
piApprox :: Double -> Double
piApprox tol = piApproxAux tol 1
 where
 piApproxAux tol n = 
  if abs ((2*arcsin1 n) - pi)<=tol
   then 2*arcsin1 n
   else piApproxAux tol (n+1)

{- -----------------------------------------------------------------
 - logApprox
 - -----------------------------------------------------------------
 - Description: approximates the value of log x by exploiting its definition as a definite integral
 -}
logApprox :: Double -> Double -> Double
logApprox x tol = logApproxAux x tol 2
 where
 logApproxAux x tol n =
  if abs (definiteIntegral 1 x g n - definiteIntegral 1 x g (n-1))<=tol
   then definiteIntegral 1 x g n
   else logApproxAux x tol (n+1)
 g=(1/)
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
Fuction: definiteIntegral
Property: definiteIntegralAux a b g n 1 == (g (a+fromInteger x) + g a)/2*x
Actual Test Result: Pass

Function: definiteIntegral
Test Case Number: 1
Input: 0 5 (\x -> 5-x) 10000
Excpected Output: 12.5
Actual Output: 12.5

Function: definiteIntegral
Test Case Number: 2
Input: (-1) 1 (\x -> x^2) 10000
Excpected Output: 0.66666
Actual Output: 0.6666666800000016

Function: definiteIntegral
Test Case Number: 3
Input: (-1) 1 (\x -> x^3) 10000
Excpected Output: 0.0
Actual Output: 8.044780119842443e-17

Fuction: arcsin1
Property: arcsin1 == definiteIntegral (-1) 1 g
Actual Test Result: Pass

Function: arcsin1
Test Case Number: 1
Input: 1
Excpected Output: 0.0
Actual Output: 0.0

Function: arcsin1
Test Case Number: 2
Input: 1000
Excpected Output: 1.57079632679
Actual Output: 1.57074373850107

Function: arcsin1
Test Case Number: 3
Input: 10000
Excpected Output: 1.57079632679
Actual Output: 1.570794663715288

Fuction: piApprox
Property: piApproxAux 4 1 == 2*arcsin1 1
Actual Test Result: Pass

Function: piApprox
Test Case Number: 1
Input: 1
Excpected Output: 3.14159265359
Actual Output: 2.5141574442188355

Function: piApprox
Test Case Number: 2
Input: 0.01
Excpected Output: 3.14159265359
Actual Output: 3.1316034730484

Function: piApprox
Test Case Number: 3
Input: 0.00001
Excpected Output: 3.14159265359
Actual Output: 3.1415826549115717

Fuction: logApprox
Property: logApproxAux 2 1 2 == definiteIntegral 1 1 g 2
Actual Test Result: Pass

Function: logApprox
Test Case Number: 1
Input: 1 0.000001
Excpected Output: 0.0
Actual Output: 0.0

Function: logApprox
Test Case Number: 2
Input: 10 0.000001
Excpected Output: 2.30258509299
Actual Output: 2.302703049626941

Function: logApprox
Test Case Number: 3
Input: (exp 1) 0.000001
Excpected Output: 1.0
Actual Output: 1.0000368301004259
-}