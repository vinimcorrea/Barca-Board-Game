# BARCA
### G10_02

**Barca_4**:

Isabel Maria Couto da Silva, 201904925, contribuição: 25%

Vinícius Corrêa, 202001417, contribuição: 75%

## Instalação e Execução
 
Para executar o jogo é necessário ter previamente instalado o SICStus.

Após ter a aplicação aberta, basta consultar o ficheiro project.pl, que tem todas as dependências necessárias, e executar o jogo fazendo ```play.``` na consola.

## Descrição do Jogo

O jogo [Barca](https://boardgamegeek.com/boardgame/69347/barca) é um jogo de tabuleiro para 2 pessoas. Este normalmente é jogado num tabuleiro de dimensões 10x10, que contém 4 "bebedouros" (*watering holes*). O objetivo do jogo é colocar 3 das suas peças de jogo no local de um "bebedouro".

Cada jogador dispõe de 6 peças para jogar, sendo elas: 2 leões, 2 elefantes e 2 ratos. Os ratos apenas se podem mover horizontalmente ou verticalmente (ortagonalmente), os leões apenas se podem mover diagonalmente e os elefantes podem-se mover, tanto ortogonalmente como diagonalmente.
Neste jogo, os leões têm medo dos elefantes, os elefantes têm medo dos ratos, e os ratos têm medo dos leões. Quando um animal tem medo de outro, estes não podem estar adjacentes um ao outro no tabuleiro de jogo.
Por exemplo, se um jogador mover a peça do seu leão para o lado da peça de um rato do seu adversãrio, o adversário é obrigado a mexer o seu rato na sua jogada seguinte.
É de ter em consideração que os animais do mesmo jogador não têm medo uma da outra, por exemplo o rato do jogador 1 não tem medodo leão do jogador 1.
Quando um jogador consegue colocar 3 dos seus animais em bebedouros, o jogo termina, sendo esse jogador o vencedor.

## Lógica do Jogo

### Representação interna do estado do jogo

Neste jogo, é suficiente ter a informação da disposição das peças no tabuleiro (e o respetivo tabuleiro) para se comnhecer o estado de jogo num dado momento. Deste modo, o GameState corresponde a uma matriz que corresponde ao tabuleiro com as peças nos respetivos locais.
O tabuleiro tem o caracter '-' que simboliza uma casa vazia, e, l, m que simbolizam o elefante, leão e rato do jogador das peças brancas, respetivamente. Para o jogador com as peças pretas as peças de jogo estão representadas por 'E', 'L', 'M', que, novamente, representam o elefante, leão e rato mas da cor preta. Ainda no tabuleiro de jogo, é possível observar quatro casas com o caracter 'O', que correspondem aos "bebedouros" (*watering holes*) do jogo.

Utilizando o predicado ```initial_board(+Board)``` é possível ver o tabuleiro inicial de jogo. No ficheiro *board.pl* é possível ver o tabuleiro com que o predicado initial_board inicializa o jogo.

### Visualização do estado do jogo

O predicado de visualização do estado de jogo é ```display_game(+GameState)```, que aceita um estado de jogo e mostra na consola o estado de jogo do tabuleiro nesse momento.

No menu principal do jogo existem várias opções:

- **1. Instructions** - Permite ver as regras do modo e o modo de jogar.

- **2. Play** - Permite avançar para um segundo menu que contém as seguintes opções:

    - **1. Human vs Human** - 2 jogadores jogam um contra o outro.

    - **2. Human vs Computer** - Jogador joga contra o computador. (1)*

    - **3. Computer vs Computer** - Computadr joga contra computador. (1)*

    - **0. Back** - Permite voltar ao menu anterior.

- **0. Quit** - Permite sair do jogo.

(1)* - Após a escolha de uma destas opções surge um novo menu para escolher a o nível de dificuldade do computador. Existem os níveis: **1. Level 1 (Random Moves)** e **2. Level 2 (Greedy Algrithm)**.

### Execução de Jogadas

Este foi, para nós, o predicado mais complicado de realizar, devido à complexidade das operações, pelo que foi dividido em várias funções, todas elas inseridas no ficheiro *moves.pl*.

O procedimento é o seguinte:

- Verificamos qual a peça que foi selecionada e vamos "buscá-la".

- Vemos qual é o animal da peça. Após isto: 

    - Se for um rato, apenas se pode movimentar ortagonalmente.

    - Se for um leão, apenas se pode movimentar diagonalmente.

    - Se for um elefante, tanto se pode movimentar ortagonalmente como diagonalmente.

### Lista de Jogadas Válidas

A lista das jogadas possíveis é retornada através do predicado ```animal_board_with_moves/4```, cujo segundo elemento corresponde ao animal que o jogador pretende mexer (*lion*, *mice* ou *elephant*). Esta listagem é possível, após também ser chamado o predicado ```valid_move/6```. Dada a complexidade desta listagem, tal como no ponto anterior, não nos foi possível utilizar o mesmo predicado que era sugerido no enunciado do projeto.

### Final do Jogo

O predicado ```game_over(+GameState, -Winner)``` determina o vencedor num dado estado de jogo, verificando se o jogador que ganhar tem 3 das suas peças num watering hole.

### Avaliação do Tabuleiro

Tendo em conta que o objetivo do jogo é colocar 3 das suas peças nos "bebedouros" presentes no tabuleiro de jogo, a função ```value(+GameState, +Player, -Value)``` é responsável por avaliar o estado de jogo, imprimindo na consola uma mensagem sempre que um jogador coloca uma peça num bebedouro, informando os jogadores de quantas peças no sítio correto o jogador em questão tem. Deste modo, esta função vai verificando como está o estado de jogo, informando os jogadores. 

### Jogada do Computador

Para a opção em que o computador tem o nível mais fácil de jogar, *Easy*, é utilizado o predicado ```easybot/3``` no ficheiro *bot.pl*. Neste predicado, é escolhida uma jogada random, tendo em conta as jogadas possíveis para a peça em questão.

Infelizmente, não conseguimos implementar o bot "inteligente" apesar das nossas tentativas.

## Conclusões

Tendo em conta o trabalho desenvolvido e descrito ao longo do presente relatório, concluímos que as únicas meta que não foram alcançadas são tamanho do tabuleiro ser variável e ser implementado o bot de dificuldade avançada. Posto isto, pensamos que todos os restantes objetivos obrigatórios foram concluídos. Estamos certos de que alguns dos predicados poderiam ser melhorados ou desenvolvidos de outra forma, mas ainda assim, estamos satisfeitos com o trabalho que foi desenvolvido e entregue.

## Bibliografia

- Documentação da UC

- https://boardgamegeek.com/boardgame/69347/barca
