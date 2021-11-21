# feup-pfl-2021
Programação Funcional e Lógica

## IMPORTANT
In BigNumber representation, if the digit 0 appears at the beginning of the array, the BigNumber is negative

To represent the number 0 in BigNumber notation we use the empty array '[]' 

## Function

| Function             | fibRec |
| ---                  | ---    |
| __Usage Cases__          | We tested for each integer number till x and it does not work for negative numbers.        |
| __Function Description__ | The function adopts a __recursive strategy__ because every time it needs sum the previous two numbers of the sequence it calls the same function recursively with those two parameters. We just defined the first two elements of the sequence (0 and 1) and then it calculates the _nth_ one.       |

| Function             | fibLista |
| ---                  | ---    |
| __Usage Cases__          | We tested for each integer number till x and it does not work for negative numbers.        |
| __Function Description__ | The function adopts a __dynamic programming__ aprouch because he starts to calculate the finobaci numbers from 0 until the _nth_ number asked, dynamically acessing the previous calculated finobacci numbers to produce the next one.       |

| Function             | fibListaInfinita |
| ---                  | ---    |
| __Usage Cases__          | We tested for each integer number till x and it does not work for negative numbers.        |
| __Function Description__ | The function adopts a __dynamic infinite list__ strategy since he builds a infinite list with all the finobaci numbers. Thanks to haskell properties by asking the _nth_ number he self stops the infite list calculation on the asked index      |


| Function             | fibRecBN |
| ---                  | ---    |
| __Usage Cases__          | We tested for each BigNumber number till x and it does not work for negative numbers.        |
| __Function Description__ | Same as the integral version. The function adopts a __recursive strategy__ because every time it needs sum the previous two numbers of the sequence it calls the same function recursively with those two parameters. We just defined the first two elements of the sequence (0 and 1) and then it calculates the _nth_ one.       |

| Function             | fibListaBN |
| ---                  | ---    |
| __Usage Cases__          | We tested for each BigNumber number till x and it does not work for negative numbers.        |
| __Function Description__ | Same as the integral version. The function adopts a __dynamic programming__ aprouch because he starts to calculate the finobaci numbers from 0 until the _nth_ number asked, dynamically acessing the previous calculated finobacci numbers to produce the next one.       |

| Function             | fibListaInfinitaBN |
| ---                  | ---    |
| __Usage Cases__          | We tested for each BigNumber number till x and it does not work for negative numbers.        |
| __Function Description__ | Same as the integral version. The function adopts a __dynamic infinite list__ strategy since he builds a infinite list with all the finobaci numbers. Thanks to haskell properties by asking the _nth_ number he self stops the infite list calculation on the asked index      |


| Function             | scanner |
| ---                  | ---    |
| __Usage Cases__          | |
| __Function Description__ | |
| __Implementation__ | |

| Function             | output |
| ---                  | ---    |
| __Usage Cases__          | |
| __Function Description__ | |
| __Implementation__ | |


| Function             | somaBN |
| ---                  | ---    |
| __Usage Cases__          | We tested this function with all kinds of combinations like, positive + positive somaBN [1,0] [2,0] = [3,0], positive + negative somaBN [1,0] [0,2,0] = [0,1,0], sum of numbers that result in more decimal houses somaBN [8,8] [1,3] = [1,0,1], sum of numbers that result in the reduction of decimal houses somaBN [1,5] [0,1,3] = [2], sum with 0 somaBN [1,5] [] = [1,5] and sum resulting in 0 somaBN [0,1,3] [1,3] = []|
| __Function Description__ | This function takes two BigNumbers returning their sum |
| __Implementation__ | We start by inverting the BigNumber received to start the adiction operation with the units digit. Next we add the first element of each BigNumber array if the sum is bigger then 10, by making the `mod` of 10 we make sure that we only have single digit numbers in our array, by making the `div` of 10 and adding it to the next element, of one of the BigNumbers, we make sure that we dont lose the excess. All the patterns present are intended to cover all case scenarios if one of the BigNumbers is bigger then the other. In order to deal with the negative numbers we analyse the numbers given at the star and if the signals are opposites we treat the request as an subtration of BigNumbers |

| Function             | subBN |
| ---                  | ---    |
| __Usage Cases__          | We tested this function with all kinds of combinations like, positive - positive subBN [1,0] [2,0] = [0,1,0], positive - negative subBN [1,0] [0,2,0] = [3,0], negative - positive subBN [0,1,0,0] [2,0] = [0,1,2,0],  negative - negative subBN [0,1,0,0] [0,2,0] = [0,8,0], subtration with 0 subBN [1,5] [] = [1,5] and subtration resulting in 0 subBN [1,3] [1,3] = [] |
| __Function Description__ | This function takes two BigNumbers returning their subtration |
| __Implementation__ | We start by inverting the BigNumber received to start the operation. Next we verify wich of the given numbers is bigger, by making this step we reduce the number of patterns necessary and we can safely decide wich is the result signal. After these verification steps we start the operation, just like the sum, we make use of the `div` and `mod` operations to ensure that he only have single house digits in our number representation and the subtration is excess is correctly transported to the next digits. In this opperation when the signals, of the given numbers are different, we make use the sum function |

| Function             | mulBN |
| ---                  | ---    |
| __Usage Cases__          | We tested this function with all interesting combinations for multiplication, positive * positive mulBN [4,3] [2,3] = [9,8,9], positive * negative mulBN [0,1,2,3] [1,2,3] = [0,1,5,1,2,9], negative * negative mulBN [0,4,3] [0,1,2,1] = [5,2,0,3], also multiplication by 0 is covered in mulBN [2,3,4,2] [] = [] |
| __Function Description__ | This function takes two BigNumbers returning their multiplication |
| __Implementation__ | To implement this function we first start by inverting the two given BigNumbers. In order to make the multiplication operation we take an recursive aprouch since we sum, the result of the multiplication of all the digits of the first BigNumber with the first digit of the second BigNumber, with the value returned from the recursive call of the `mulBN` with the first BigNumber and the tail of the second BigNumber. To deal with the negative numbers we verify if the numbers have both the same signal, depending on the case the multiplication operation is made with possitive numbers and, at the end, a negative signal is added at the beggining of the BigNumber |


| Function             | divBN |
| ---                  | ---    |
| __Usage Cases__          | We tested this function with all the possibel combinations. Positive divided by positive divBN [1,2,3] [2,3] = ([5],[8]), positive divided by negative divBN [5,3,3] [0,5,3] = ([0,1,1],[0,5,0]), negative divided by positive divBN [0,7,3] [7] = ([0,1,1],[4]), negative divided by negative divBN [0,2,4,3] [0,8] = ([3,0],[0,3]). We also tested values where the remainder is zero divBN [2,5,6] [8] = ([3,2],[]), when the dividend is smaller then the divisor divBN [2,3] [5,3] = ([],[2,3]), when the dividend is zero divBN [] [4] = ([],[]) and lastely we tested the case when the divisor is zero divBN [5] [] = "Exception: divide by zero" |
| __Function Description__ | This function takes two BigNumbers returning a tuple with (divisão inteira) and the remainder. The division with negative numbers is made just like the `mod` and `div` operators from haskell|
| __Implementation__ | In order to implement this function we sum the divisor _n_ times until the sum is bigger then the dividend, the desired result is _n-1_ for the quocient and the sum minus the divisor for the remainder. In this function to deal with negative numbers and keep the same output as the haskell functions we had the need to make some sums and substrations to the final output of the division |

| Function             | safeDivBN |
| ---                  | ---    |
| __Usage Cases__          | We test this function with all the cases described in divBN safeDivBN [1,6] [3] = Just ([5],[1]). We also test it when the divisor is zero safeDivBN [1,6] [] = Nothing |
| __Function Description__ | This function takes two BigNumbers returning their division safely |
| __Implementation__ | The implementation of this function is just like the `divBN` since we make a call to it. The only difference is the existence of a pattern that checks if the divisor is zero, if so it returns nothing|


## Ex4
Int -> Int  tem limite poranto vai eventualmente dar overflow 

Integer -> Integer apresenta segurança contra overflow, portanto poderá representar qq numero. Tem restrição de memoria

BigNumber -> BigNumber os numeros são represantados em lista, portanto a representação é infinita. Tal como o Integer para quando a memoria acabar
