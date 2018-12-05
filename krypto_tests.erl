-module(krypto_tests).
-compile(export_all).

%%
%% Tests für ggT
%%
ggT_1_test() -> 2 = krypto:ggT(10,4).
ggT_2_test() -> 1 = krypto:ggT(13,7).
ggT_3_test() -> 2 = krypto:ggT(2,4).
ggT_4_test() -> 5 = krypto:ggT(35,5).


%%
%% Tests für encode
%%
block_1_test() -> [402490982,575881924,515356846,583295881] = krypto:block("das ist ein Test", [], 5,krypto:build_map(emap)).
block_2_test() -> [] = krypto:block("",[],5,krypto:build_map(emap)).


%%
%% Tests für decode


%%
%% Tests für multiplikativ_inverses
%%
mulinf_1_test() ->  67 = krypto:multiplikativInverses(42,97).
mulinf_2_test() -> 9 = krypto:multiplikativInverses(3,13).


%%
%% Tests für fpow
%%
fpow_1_test() -> 46 = krypto:fpow(2,17,49).

%%
%%Tests für make_rel_prime
%%
relprime_1_test() -> 7 = krypto:make_rel_prime(5,10).

%%
%% Tests für make_prim
%%
make_prime_1_test() -> A = krypto:make_prime(5), [A] = [X || X <- lists:seq(2,A),krypto:ggT(X,A) /= 1 ].
make_prime_2_test() -> A = krypto:make_prime(5), [A] = [X || X <- lists:seq(2,A),krypto:ggT(X,A) /= 1 ].


calc_exponent_1_test() -> 16 = krypto:calc_exponent(364604440544187073800344395561,58,1).
