-module(krypto).
-include_lib("eunit/include/eunit.hrl").
-import(lists,[seq/2] ).
-compile(export_all).


% Berechnet den größten gemeinsammen Teiler Zweier Zahlen
ggT(0,B) -> B;
ggT(A,0) -> A;
ggT(A, B) ->
    if  A == B -> A;
        A > B ->  ggT(A rem B, B);
        A < B ->  ggT(B rem  A, A)
    end.


multiplikativInverses(A,M) ->
    {_,X,_} = extggT(A,M),
    Erg = X rem M,
    if Erg < 0 -> Erg + M;
          true -> Erg
    end.

%%
%% Ausgabe des Tubels wenn B = 0
extggT(A,0) -> {A,1,0};

extggT(A,B) ->
    {G,U,X} = extggT(B, A rem B),
    Q = A div B, %Floor division
    {G,X,(U - (Q * X))}.


% Phi Funktion
% Berrechnet die Länge einer Liste L für die gilt:
% ∀ x ∈ {1, ..., M-1} | ggT(x,M) == 1
phi(M) ->
        length([ X || X <-  lists:seq(1,M-1), ggT(X,M) == 1 ]).

% Berechnet die Primfaktorzerlegung einer Zahl Num und gibt diese als Liste zurück
% Wenn Num größer als 1 und ein Integer ist   dann führe die Funktion factor aus
primfz(Num) when Num >1,is_integer(Num) ->factor(Num,2);
primfz(Num)-> Num.
% Hilfsfunktion für primfz
factor(Num,Count) when Count>Num+1->[];
factor(Num,Count)->case Num rem Count of
                     0 -> [Count|factor(Num div Count,Count)];
                     _ -> factor(Num,Count+1)
                   end.


% Implementierung schenelle Exponentiation
%
fastExponentiation(_,0)  -> 1; % Wenn B == 0
fastExponentiation(A,B) when B < 0 -> pow(1/A,-B);
fastExponentiation(A,B) when B rem 2 == 1 -> A * pow(pow(A,(B-1) div 2),2);
fastExponentiation(A,B) when B rem  2 == 0 -> pow(pow(A,B div 2),2).
%%
%%Schnelle Exponentiation mit modulo
fpow(A,1,M) -> A rem M;
fpow(A,2,M) -> A * A rem M;
fpow(A,B,M) ->
    B1 = B div 2,
    B2 = B - B1,
    P = fpow(A,B1,M),
    case B2 of
        B1 -> (P*P) rem M;
        _  -> (P*P*A) rem M
    end.

pow(_,0) -> 1; % Wenn B == 0
pow(A,1) -> A; % Wenn B == 1
pow(A,B) -> A * (pow(A, B-1)).


%% Erstellt eine Map für die Umrechnungen
%% Hierfür wird die Datei Map genutzt
build_map() -> {ok,Map} = file:consult("emap"),
                Result = maps:from_list(Map), Result.
build_map(A) -> {ok,Map} = file:consult(A),
                maps:from_list(Map).

% Wandelt einen String in die dazugehörige Zahlenkette um.
block(Input, List, K, Map) when length(Input) > K ->
    block(string:substr(Input, (K + 1) ), lists:append([List, toBlock(Input, K - 1, Map, 0, length(maps:to_list(Map)))]), K, Map);

block(Input,List, K, Map) when length(Input) == K ->
   % block(string:trim(Input, leading, string:slice(Input, 0, K)), lists:append([List, toBlock(Input, K - 1, Map, 0, length(maps:to_list(Map)))]), K, Map);
   block("", lists:append([List, toBlock(Input, K - 1, Map, 0, length(maps:to_list(Map)))]), K, Map);

block(Input,List,_,_) when length(Input) == 0 -> List;

block(Input,List,K,Map) when length(Input) < K ->
    block(string:concat(Input," "),List,K,Map).

%Berechnet die Zahl für einen Block der länge K. Ist ein Block kürzer als K wird mit * aufgefüllt
%toBlock(Input, K, Map, Result, Length) when length(Input) == 0 ->
%    if K /= 0 ->  toBlock("",K - 1, Map, Result + maps:get(" ", Map, "") * fastExponentiation(Length,K), Length);
%       true ->  [Result + maps:get(" ", Map, "")]
%    end;

toBlock(Input, K, Map, Result, Length) ->
    if K /= 0 ->
           Tail = tl(Input),NewK = K - 1, Char = string:slice(Input,0,1), TheChar = maps:get(Char,Map,""), Mult = fastExponentiation(Length,K),
           toBlock(Tail,NewK,Map,Result + (TheChar * Mult),Length);
           %toBlock(tl(Input), K - 1, Map, Result + (maps:get(string:slice(Input,0,1), Map, "") * fastExponentiation(Length, K)), Length) ;
         true -> [(Result + maps:get(string:slice(Input,0,1),Map,""))]
    end.

%%
%% Verschlüsselt die einzelnen Elemente einer Liste B mithilfe des Exponenten E und des Moduls M.
%% Die Variable R wird mit [] aufgerufen, enthält das Ergebnis und wird am Eende ausgegeben
%%
chiffre(B,E,M,R) when length(tl(B)) /= 0 ->
    chiffre(tl(B),E,M, lists:append(R, [fpow(hd(B),E,M)]));
chiffre(B,E,M,R) -> lists:append(R,[fpow(hd(B),E,M)]).


% Wandelt eine Zahlenkette in einen Sting um
decode(List, K, Map, Length, Result) ->
    Head = hd(List), Tail = tl(List), %io:format("K: ~p \n",[K]),
    if length(Tail) == 0 -> string:concat(Result, decodeNumber(Head, K - 1, Map, Length, ""));
            true -> decode(Tail, K, Map, Length, string:concat(Result, decodeNumber(Head, K - 1, Map, Length, "")))
    end.

decodeNumber(Input, K, Map, Length, Result) ->
    if K == 0 ->  X = [X || {X,Y} <- maps:to_list(Map), Y =:= trunc(Input)],
                  string:slice(X,0,1);
        true  ->  %io:format("Input: ~p \n",[Input]),
                  Res = Input div fastExponentiation(Length,K),
                  X = [X || {X,Y} <- maps:to_list(Map), Y =:= Res],
                  string:concat( string:slice(X,0,1) ,decodeNumber(Input - fastExponentiation(Length, K)  * Res, K - 1, Map, Length, ""))
    end.
%%
%% Entschlüsselt einen eingegebenen String
%% Input = Eingabe String
%% Block_Int = Blocklänge, mit der das Chiffrat erzeugt wurde
%% Exp = Exponent
%% Modul = Modul
%% Block_ext = Blocklänge, mit der die NAchricht ursprünglich verschlüsselt wurde
%%
entschluesseln(Input,Block_Int,Exp,Modul,Block_ext) ->
    Map = build_map(emap),
    Cif = block(Input,[],Block_Int,Map),
    io:format("Chiffre: ~p \n",[Cif]),
    Message = chiffre(Cif,Exp,Modul,[]),
    io:format("Block: ~p \n",[Message]),
    decode(Message,Block_ext,Map,length(maps:to_list(Map)),"").

%%
%% Veschlüsselt einen eingegebenen Text
%% Input = Eingabe String
%% Block_ext = Blocklänge, mit der der String nach außen hin unterteielt wird
%% Exp = Exponent
%% Modul = Modul
%%
verschluesseln(Input,Block_ext,Exp,Modul) ->
    Map = build_map(emap),
    Block = block(Input,[],Block_ext,Map),
    io:format("Block: ~p \n",[Block]),
    Cif = chiffre(Block, Exp, Modul,[]),
    io:format("Chiffre: ~p \n",[Cif]),
    Len = length(maps:to_list(Map)),
    IExp = calc_exponent(Modul,Len,Block_ext),
    io:format("interner Exponent: ~p \n",[IExp]),
    decode(Cif, IExp, Map, Len, "").


%%
%% Berechnet ein relativ primes element
%%
make_rel_prime(A,B) ->
    case ggT(B,A) == 1 of
        true  -> A;
        false -> make_rel_prime(A+1, B)
    end.

%%
%% Generiert nebenläufig zwei Primzahlen
%%
compute_primes(Len) ->
   spawn(krypto, computeprime, [Len, self()]),
   spawn(krypto, computeprime, [Len, self()]),
   receive
     A -> A
   end,
   receive
     B -> B
    end,
   {A,B}.

%%
%% Hilfsfunktion zum nebenläufigen Generieren von Primzahlen
%%
computeprime(Len,Pid) ->
    Result = make_prime(Len),
    Pid ! Result.

%% Generiert eine sichere Primzahl
make_prime(K) when K > 0 ->
    new_seed(),
    N = make(K),
    if N > 3 ->
           io:format("Generiere eine ~w stellige Primzahl ", [K]),
           MaxTries = N - 3,
           P1 = make_prime(MaxTries, N +1),
           io:format("~n", []),
           P1;
       true -> make_prime(K)
    end.

make_prime(0,_) -> exit(something_happend);
make_prime(K,P) ->
    io:format(".",[]),
    case is_prime(P) of
        true -> P;
        false -> make_prime(K-1, P+1)
    end.

make(N) -> new_seed(), make(N,0).
make(0,D) -> D;
make(N,D) -> make(N-1,D*10+(rand:uniform(10)-1)).

%% Generiert einen neuen Seed für einen Zufall. Bei Aufruf wird der neue Seed dem rand:uniform zur Vefügung gestellt.
new_seed() ->
    {_,_,X} = erlang:timestamp(),
    {H,M,S} =  time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed,{H1,M1,S1}). %Setzte neuen Seed für rand:uniform


is_prime(D) ->
    new_seed(),
    is_prime(D,100).
is_prime(D,Tests) ->
    N = length(integer_to_list(D)) - 1,
    is_prime(Tests, D, N).


is_prime(0,_,_) -> true;
is_prime(Test,N,Len) ->
    K = rand:uniform(Len),
    A = make(K),
    if A < N ->
           B = N-1,
           case fpow(A,(N-1) div 2, N) of
               1 -> is_prime(Test - 1, N, Len);
               B -> is_prime(Test - 1, N,Len);
               _ -> false
            end;
        true -> is_prime(Test, N, Len)
    end.


%%
%% Berechnet den Exponenten, mit dem ein verschlüsselter Block zu einem Text übersetzt werden kann
%% 26^k < n < 26^l
%%
calc_exponent(A,Len,LenB) ->
    B = fastExponentiation(Len,LenB),
    if B < A -> calc_exponent(A,Len,LenB + 1);
       B == A -> LenB + 1;
       true -> LenB
    end.



teilen(A,B,Result) when B > 0 ->
    io:format("A: ~p \n ",[A]), io:format("B: ~p \n",[B]),
    if A > B -> teilen(A-B,B,Result +1);
       A == B -> Result + 1;
       true -> Result
    end.

