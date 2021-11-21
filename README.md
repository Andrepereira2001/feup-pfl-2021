# feup-pfl-2021
Programação Funcional e Lógica

# IMPORTANT
In BigNumber representation 0 in the beginning of a array tell us that the number is negative
The value 0 in BigNumber notation is an empty array '[]' 

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
| __Usage Cases__          | |
| __Function Description__ | This function takes two BigNumbers returning their multiplication |
| __Implementation__ | |

| Function             | divBN |
| ---                  | ---    |
| __Usage Cases__          | |
| __Function Description__ | This function takes two BigNumbers returning their division |
| __Implementation__ | |

| Function             | safeDivBN |
| ---                  | ---    |
| __Usage Cases__          | |
| __Function Description__ | This function takes two BigNumbers returning their division safely |
| __Implementation__ | |


## Ex4
Int -> Int  tem limite poranto vai eventualmente dar overflow 

Integer -> Integer apresenta segurança contra overflow, portanto poderá representar qq numero. Tem restrição de memoria

BigNumber -> BigNumber os numeros são represantados em lista, portanto a representação é infinita. Tal como o Integer para quando a memoria acabar
