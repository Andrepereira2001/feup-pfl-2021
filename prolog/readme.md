# Trabalho de Programação Lógica 2021

## Trabalho Five Field Kono - Grupo 3

André Diogo Bastos Pereira - up201905650
Matilde Jacinto Oliveira - up201906954

## Instalação e Execução 

## Descrição do jogo

Five Field Kono é um jogo de estratégia abstrata coreano, para dois jogadores, em que o objetivo é que o ganhe o jogador que conseguiu mover todas as suas peças nos locais de início das peças do adversário. 

De forma muito simplista, a lógica do jogo consiste em as jogadas sejam alternadas entre os dois jogadores, sendo que numa jogada o jogador é obrigado a mover uma das suas peças para uma posição que esteja na diagonal da atual e pertencente ao mesmo quadrado, podendo andar para trás. O jogo acaba quando um dos jogadores consegue que as suas peças fiquem todas nas posições das peças inicias do adversário.

![Board](./img/gameImage.png)

## Lógica de Jogo

### __Representação interna do estado de jogo__

* __B__: Black (peças pretas)

* __W__: White (peças brancas)

* __E__: Empty (lugares vazios)

* __Board__:
```
    [B,
    [B,B,B,B,B],
    [B,E,E,E,B],
    [E,E,E,E,E],
    [W,E,E,E,W],
    [W,W,W,W,W]]
```

Primeiro elemento da lista representa a próxima pessoa a jogar.

* __Move__: [[__x__,__y__], [__deltaX__, __deltaY__]]

    * __x__: 0-x
    * __y__: 0-x

    * __deltaX__: 1 ou -1
    * __deltaY__: 1 ou -1

    Exemplo: [ [0,0] , [1,1] ]

### __Visualização do estado de jogo__

### __Execução de jogadas__

`move(+GameState, +Move, -NewGameState).`

### __Final do jogo__
`game_over(+GameState, -Winner)`

### __Lista de jogadas válidas__

### __Avaliação do estado de jogo *__

### __Jogada do Computador__ 

## Conclusões 

### Bibliografia

https://en.wikipedia.org/wiki/Five_Field_Kono
