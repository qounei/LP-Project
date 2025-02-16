% ist109324, Joao Agostinho

:- use_module(library(clpfd)).
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["puzzlesAcampar.pl"]. % Ficheiro dado.

/*---------------------------------------------------------------------------*/
/*------------------------------4.1 Consultas--------------------------------*/
/*---------------------------------------------------------------------------*/

% -------------------------------------------------------------------------------------
% vizinhaca(+(L, C), -Vizinhanca)
% Vizinhaca eh uma lista ordenada de cima para baixo e da esquerda para a direita com
% as coordenadas imediatamente ao lado da coordenada (L,C).
% -------------------------------------------------------------------------------------
vizinhanca((L,C),
[(L_superior, C),(L,C_anterior),
(L, C_posterior), (L_inferior,C)]):-
    L_superior is L - 1,
    L_inferior is L + 1,
    C_anterior is C - 1,
    C_posterior is C + 1.

% -------------------------------------------------------------------------------------
% vizinhaca(+(L, C), -VizinhancaAlargada)
% VizinhancaAlargada eh uma lista ordenada de cima para baixo e da esquerda para a
% esquerda para a direita com as coordenadas imediatamente ao lado e imediatamente
% diagonais da coordenada (L,C).
% -------------------------------------------------------------------------------------
vizinhancaAlargada((L,C), VizinhancaAlargada) :-
    findall((Linha, Coluna),(
        between(-1, 1, X), between(-1, 1, Y),
        Linha is L + X, Coluna is C + Y, (X, Y) \= (0, 0)
    ), VizinhancaAlargada).

% -------------------------------------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas)
% TodasCelulas eh uma lista ordenada de cima para baixo e da esquerda para a
% esquerda para a direita com as coordenadas do Tabuleiro.
% -------------------------------------------------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas):-
    length(Tabuleiro,Comprimento),
    findall((L,C), (
        between(1, Comprimento, L), between(1, Comprimento, C)
    ), TodasCelulas).

% -------------------------------------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas, +Objecto)
% TodasCelulas eh uma lista ordenada de cima para baixo e da esquerda para a
% esquerda para a direita com as coordenadas do Tabuleiro em que existe um
% objecto do tipo Objecto.
% -------------------------------------------------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
        length(Tabuleiro, Comprimento),
        % if else, em que caso o Objecto nao eh uma var encontra os objectos iguais
        (nonvar(Objecto) ->
            findall((L, C),(
                between(1, Comprimento, L), between(1, Comprimento, C),
                nth1(L, Tabuleiro, Linha), nth1(C, Linha, Objecto_L),
                Objecto == Objecto_L
            ),TodasCelulas)
        ;
        % Caso o Objecto eh uma var, procura-se no Tabuleiro outras vars
            findall((L, C),(
                    between(1, Comprimento, L), between(1, Comprimento, C),
                nth1(L, Tabuleiro, Linha), nth1(C, Linha, var)
            ),TodasCelulas)).

% -------------------------------------------------------------------------------------
% contaElementos(+Lista, +Objecto, -Contagem)
% Auxiliar - Calcula o numero de vezes(Contagem) que o Objecto aparece
% numa lista, utilizada no predicado ocontaElementosLinhas/3
% -------------------------------------------------------------------------------------
contaElementos(Lista, Objecto, Contagem) :-
    findall(X,(
        member(X, Lista), X = Objecto
    ), ListaDeElementos),
    length(ListaDeElementos, Contagem).

% -------------------------------------------------------------------------------------
% contaElementosLinhas(+Tabuleiro, +Objecto, -ContagemLinhas)
% Auxiliar - Calcula o numero de vezes que o Objecto aparece num Tabuleiro
% em cada uma das suas linhas e coloca numa lista(ContagemLinhas),
% utilizada no predicado calculaObjectosTabuleiro/3
% -------------------------------------------------------------------------------------
contaElementosLinhas(Tabuleiro,Objecto,ContagemLinhas):-
    length(Tabuleiro,Comprimento),
    todasCelulas(Tabuleiro, TodasCelulas, Objecto),
    findall(N,(
        between(1, Comprimento, L),
        contaElementos(TodasCelulas, (L,_), N)
        ),ContagemLinhas).

% -------------------------------------------------------------------------------------
% contaElementosLinhas(+Tabuleiro, -ContagemLinhas, -ContagemColunas,+Objecto)
% Calcula o numero de vezes que o Objecto aparece num Tabuleiro em cada
% das suas linhas e das suas colunas e coloca nas listas ContagemLinhas e
% ContagemColunas respetivamente.
% -------------------------------------------------------------------------------------
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    contaElementosLinhas(Tabuleiro,Objecto,ContagemLinhas),
    transpose(Tabuleiro,TabuleiroTrans),
    contaElementosLinhas(TabuleiroTrans,Objecto,ContagemColunas).

% -------------------------------------------------------------------------------------
% CelulaVazia(+Tabuleiro, +(L, C))
% Eh verdade se o objecto na coordenada (L, C) do Tabuleiro nao tem nada ou
% eh relva, ou se as coordenadas nao pertencem ao Tabuleiro.
% -------------------------------------------------------------------------------------
celulaVazia(Tabuleiro, (L, C)):- % Caso as coordenadas nao pertecam ao Tabuleiro
    length(Tabuleiro,Comprimento),
    (L > Comprimento; C > Comprimento;
    L < 1; C < 1),!.
celulaVazia(Tabuleiro, (L, C)):- % Caso nas coordenadas haja uma relva ou esteja vazio
    nth1(L, Tabuleiro, Linha), nth1(C, Linha, Objecto),
    (var(Objecto); Objecto == r).

/*---------------------------------------------------------------------------*/
/*---------------------4.2 Insercao de tendas e relva------------------------*/
/*---------------------------------------------------------------------------*/

% -------------------------------------------------------------------------------------
% insereObjectoCelula(+Tabuleiro, +TendaOuRelva, (L,C))
% Insere o objecto TendaOuRelva no Tabuleiro nas coordenadas (L,C).
% -------------------------------------------------------------------------------------
insereObjectoCelula(Tabuleiro ,_, (L,C)) :- % Caso as coordenadas nao pertecam
    length(Tabuleiro,Comprimento),          % ao Tabuleiro
    (L > Comprimento; C > Comprimento;
    L < 1; C < 1),!.
insereObjectoCelula(Tabuleiro ,_, (X,Y)) :- % Caso o Objecto nao eh uma var
    nth1(X, Tabuleiro, Linha), nth1(Y, Linha, Objecto), nonvar(Objecto).
insereObjectoCelula(Tabuleiro ,TendaOuRelva, (X,Y)) :- % Caso o Objecto eh uma var
    nth1(X, Tabuleiro, Linha), nth1(Y, Linha, Objecto), var(Objecto),
    Objecto = TendaOuRelva.

% -------------------------------------------------------------------------------------
% insereObjectoCelula(+Tabuleiro, +TendaOuRelva, (L,C1), (L,C2))
% Insere o objecto TendaOuRelva no Tabuleiro  (L, C1) e (L, C2) sao as coordenadas,
% na Linha L, entre as quais (incluindo).
% -------------------------------------------------------------------------------------
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    findall((L,C),(
        between(C1,C2,C)
    ),Posicoes),
    maplist(insereObjectoCelula(Tabuleiro,TendaOuRelva),Posicoes).

/*---------------------------------------------------------------------------*/
/*-----------------------------4.3 Estrategias-------------------------------*/
/*---------------------------------------------------------------------------*/

% -------------------------------------------------------------------------------------
% completaComRelva(+Tabuleiro, +Linhas, +ContagemLinhas)
% Auxiliar - Caso o numero de Tendas numa Linha do Tabuleiro for o igual
% ao numero de tendas que necessarias para essa linha do puzzle, o predicado
% insere relvas nas intersecoes livres dessa linha, utilizada no predicado relva/1.
% -------------------------------------------------------------------------------------
completaComRelva(Tabuleiro,Linhas,ContagemLinhas):-     % Inicializacao de uma funcao
    completaComRelva(Tabuleiro,Linhas,ContagemLinhas,1).% auxiliar para se realizar
completaComRelva(_,[],[],_):-!.                         % iteracao pelas Linhas
completaComRelva(Tabuleiro,[Tendas|RestoLinhas],[TendasContadas|RestoContagem],L):-
    length(Tabuleiro,Comprimento),
    (Tendas == TendasContadas -> % Caso o numero de Tendas na linha seja o  desejado
    insereObjectoEntrePosicoes(Tabuleiro,r,(L,1),(L,Comprimento));true),% insere relvas
    L_aux is L + 1,
    completaComRelva(Tabuleiro,RestoLinhas,RestoContagem,L_aux),!.

% -------------------------------------------------------------------------------------
% Relva(+Puzzle)
% Preenche com relva todas as linhas e colunas cujo numero de tendas ja atingiu
% o numero de tendas possivel
% -------------------------------------------------------------------------------------
relva((Tabuleiro,Linhas,Colunas)):-
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    completaComRelva(Tabuleiro,Linhas,ContagemLinhas,1),
    transpose(Tabuleiro,TabuleiroTrans),
    completaComRelva(TabuleiroTrans,Colunas,ContagemColunas).

% -------------------------------------------------------------------------------------
% Inacessiveis(+Tabuleiro)
% Preenche com relva todas as intersecoes inacessiveis
% -------------------------------------------------------------------------------------
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro,IntersecoesArvore,a),
    %encontrar as coordenadas que sao vizinhancas de arvores
    findall(X,(
        member(Y,IntersecoesArvore),vizinhanca(Y,X)
        ),VizinhacaArvores),
    flatten(VizinhacaArvores,Flatten_VizinhacaArv),
    % remove de todas as coordenadas, aquelas com arvores e as suas vizinhancas
    todasCelulas(Tabuleiro,TodasCelulas),
    subtract(TodasCelulas,IntersecoesArvore,Intersecoes),
    subtract(Intersecoes,Flatten_VizinhacaArv,Inacessiveis),
    % coloca relva nas coordenadas obtidas
    maplist(insereObjectoCelula(Tabuleiro,r),Inacessiveis).


% -------------------------------------------------------------------------------------
% completaComTendas(+Tabuleiro, +Linhas, +TendasLinhas, +VazioLinhas)
% Auxiliar -Coloca Tendas nas linhas em que o numero de tendas necessario de se
% colocar eh igual ao numero espacos vazios, utilizada no predicado aproveita/1
% -------------------------------------------------------------------------------------
completaComTendas(Tabuleiro,Linhas,TendasLinhas,VazioLinhas):-    % Inicializacao de
        completaComTendas(Tabuleiro,Linhas,TendasLinhas,VazioLinhas,1).   % iteracao
completaComTendas(_,[],[],[],_):-!.
completaComTendas(Tabuleiro,[Tendas|RestoLinhas],
    [TendasContadas|RestoContagem],[Vazias|RestoVazias],L):-
    length(Tabuleiro,Comprimento),
    (Tendas =:=  Vazias + TendasContadas-> %Se o numero de tendas necessario de se
    % colocar eh igual ao numero espacos vazios numa Linha
    %preenche a Linha com tendas
    insereObjectoEntrePosicoes(Tabuleiro,t,(L,1),(L,Comprimento));true),
    L_aux is L + 1,
    completaComTendas(Tabuleiro,RestoLinhas,RestoContagem,RestoVazias,L_aux),!.

% -------------------------------------------------------------------------------------
% aproveita(+Puzzle)
% Coloca Tendas nas linhas e colunas em que o numero de tendas necessario de se
% colocar eh igual ao numero espacos vazios
% -------------------------------------------------------------------------------------
aproveita((Tabuleiro,Linhas,Colunas)):-
    calculaObjectosTabuleiro(Tabuleiro, TendasLinhas, TendasColunas, t),
    calculaObjectosTabuleiro(Tabuleiro, VazioLinhas, VazioColunas,_),
    completaComTendas(Tabuleiro,Linhas,TendasLinhas,VazioLinhas),
    transpose(Tabuleiro,TabuleiroTrans),
    completaComTendas(TabuleiroTrans,Colunas,TendasColunas,VazioColunas).

% -------------------------------------------------------------------------------------
% limpaVizinhancas(+Puzzle)
% Coloca relvas nas vizinhancas alargadas das tendas
% -------------------------------------------------------------------------------------
limpaVizinhancas((Tabuleiro,_,_)):-
    todasCelulas(Tabuleiro, TodasTendas, t),
    findall(X,(
        member(Tenda,TodasTendas),
    vizinhancaAlargada(Tenda,X)
    ),VizinhancaTendas),
    flatten(VizinhancaTendas, RelvaAColocar),
    maplist(insereObjectoCelula(Tabuleiro,r),RelvaAColocar).

% -------------------------------------------------------------------------------------
% detetaTendas(+Tabuleiro,+Vizinhaca)
% Auxiliar- Eh verdade se a Vizinhaca tem pelo menos uma tenda,
% utilizada no predicado unicaHipotese/1
% -------------------------------------------------------------------------------------
detetaTendas(Tabuleiro,Vizinhanca):-
    member((L,C),Vizinhanca),
    nth1(L,Tabuleiro,Linha),
    nth1(C,Linha,Elemento),
    nonvar(Elemento), Elemento == t.

% -------------------------------------------------------------------------------------
% deteta1var(+Tabuleiro,+Vizinhaca)
% Auxiliar- Eh verdade se a Vizinhaca tem pelo apenas uma var,
% utilizada no predicado unicaHipotese/1
% -------------------------------------------------------------------------------------
deteta1var(Tabuleiro,Vizinhanca):-
    findall((L,C),(
        member((L,C),Vizinhanca),
        nth1(L,Tabuleiro,Linha),
        nth1(C,Linha,Elemento),
        var(Elemento)
    ),Livres),
    length(Livres,1).

% -------------------------------------------------------------------------------------
% unicaHipotese(+Tabuleiro,+Vizinhaca)
% Coloca uma tenda na vizinhanca das arvores que ainda nao tem uma respetiva tenda e
% que apenas tem uma posicao livre na sua vizinhanca
% -------------------------------------------------------------------------------------
unicaHipotese((Tabuleiro,_,_)):-
    %encontra todas as vizinhancas de arvores
    todasCelulas(Tabuleiro, TodasArvores, a),
    findall(Vizinhanca,(
        member(Arvore,TodasArvores),vizinhanca(Arvore,Vizinhanca)
    ),Vizinhancas),
    %Caso uma vizinhanca uma tenda e tenha apenas um espaco
    %livre, coloca nesse espaco uma tenda
    exclude(detetaTendas(Tabuleiro),Vizinhancas,VizinhancasSemTendas),
    include(deteta1var(Tabuleiro),VizinhancasSemTendas,VizinhancasProntas),
    flatten(VizinhancasProntas,Intersecoes),
    maplist(insereObjectoCelula(Tabuleiro,t),Intersecoes).

/*---------------------------------------------------------------------------*/
/*---------------------------4.4 Tentativa e Erro----------------------------*/
/*---------------------------------------------------------------------------*/

% -------------------------------------------------------------------------------------
% validavalida(+LArv, +LTen)
% eh verdade caso cada arvore tenha uma respetiva tenda
% -------------------------------------------------------------------------------------
valida([],[]):-!.
valida(LArv, LTen):-
    %escolhe uma arvore da Lista
    member(Arvore,LArv),
    vizinhanca(Arvore,Vizinhanca),
    %escolha de uma tenda que pertenca a vizinhaca da arvore
    member(Tenda,LTen),
    member(Tenda,Vizinhanca),
    %eliminacao das tendas e das arvores escolhidas e recursao sem elas
    delete(LArv,Arvore,NewLArv),
    delete(LTen,Tenda,NewLTen),
    valida(NewLArv,NewLTen),!.


% -------------------------------------------------------------------------------------
% resolve(+Puzzle)
% eh verdade caso cada arvore tenha uma respetiva tenda
% -------------------------------------------------------------------------------------
resolve((Tabuleiro,Linhas,Colunas)):-                     % Caso terminal quando
    relva((Tabuleiro,Linhas,Colunas)),                    % o valida executa e tambem
    calculaObjectosTabuleiro(Tabuleiro,Linhas,Colunas,t), % quando a cada linha e coluna
    todasCelulas(Tabuleiro,Arvores,a),                    % tem o numero certo de tendas
    todasCelulas(Tabuleiro,Tendas,t),
    valida(Arvores,Tendas),!.

resolve((Tabuleiro,Linhas,Colunas)):-
    todasCelulas(Tabuleiro,CelulasVaziasInicio,_),
    % Aplicacao das estrategias de modo a resolver o puzzle
    relva((Tabuleiro,Linhas,Colunas)),
    inacessiveis(Tabuleiro),
    aproveita((Tabuleiro,Linhas,Colunas)),
    limpaVizinhancas((Tabuleiro,Linhas,Colunas)),
    unicaHipotese((Tabuleiro,Linhas,Colunas)),
    todasCelulas(Tabuleiro,CelulasVaziasFinal,_),
    length(CelulasVaziasInicio, VaziasInicio),
    length(CelulasVaziasFinal, VaziasFinal),
    % Caso as estrategias nao tenham alterado o tabuleiro,
    % o numero de coordenadas vazias do inicio eh igual
    % ao do final, executa-se o experimentaTenda
    (VaziasInicio == VaziasFinal ->
    experimentaTenda(Tabuleiro);true),
    resolve((Tabuleiro,Linhas,Colunas)),!.

% -------------------------------------------------------------------------------------
% experimenta(+Puzzle)
% Auxiliar- Coloca uma tenda numa das intersecoes vazias do Tabuleiro
% -------------------------------------------------------------------------------------
experimentaTenda(Tabuleiro):-
    todasCelulas(Tabuleiro,CelulasVazias,_),
    member(X,CelulasVazias),
    insereObjectoCelula(Tabuleiro,t,X).
