%% Copyright (c) 2013, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%% ====================================================================
%%%                    This is only a prototype!
%% ====================================================================
-module(gen_lib).

-export([integer/0, integer/1, integer/2,
         string/0, string/1,  string/2]).

-include_lib("eqc/include/eqc.hrl").

%% integer generator.
integer() ->
    int().

%% generates an integer according to the patttern.
integer(Pattern) when is_list(Pattern)->
    ?LET(S, string(Pattern),
         list_to_integer(S)).

%% generates an integer within the range.
integer(inf, inf) ->
    int();
integer(Min, inf) ->
    ?LET(N, int(),
         if abs(N)<Min -> abs(N)+Min;
            N<Min-> abs(N);
            true ->N                                  
         end);
integer(inf, Max) ->
    ?LET(N, int(),
         if N>Max andalso Max>=0->
                 Max-N;
            N>Max andalso Max<0 ->
                 Max-abs(N);
            true -> N
         end);
integer(Min, Max) ->
    choose(Min, Max).

 
%% generatea a printable string.
string() ->
    list(choose(32, 127)).

%% generatea a string whose length is within the range.
string(MinLen, MaxLen) ->
    ?LET(N, choose(MinLen, MaxLen),
         lists:foldl(fun(_X, Acc)->
                             ?LET(C, choose(32, 127),
                                  [C|Acc])
                     end, [],lists:seq(1,N))).

%% generatea a string according to a pattern.
string(Pattern) ->
    gen_with_pattern(Pattern).


gen_with_pattern(Pat) ->
    Pat1=preprocess_pat(Pat),
    case reg_exp:parse(Pat1) of
        {ok, ParseRes} ->
            gen_with_pattern_1(ParseRes);
        {error, Reason} ->
            {error, Reason}
     end.

preprocess_pat(Pat) ->
    lists:flatten([case C of 
                       127 -> "[0-9]";
                       C -> C
                   end||C<-Pat]).
    

gen_with_pattern_1(Pat) when is_integer(Pat) ->
    [Pat];
gen_with_pattern_1({'or', E1, E2}) ->
    oneof([gen_with_pattern_1(E1), gen_with_pattern_1(E2)]);
gen_with_pattern_1({concat, E1, E2}) ->
    ?LET({N1, N2}, {gen_with_pattern_1(E1), gen_with_pattern_1(E2)},
         N1++N2);
gen_with_pattern_1({kclosure, E1}) ->
    ?LET(L, list(gen_with_pattern_1(E1)),
         lists:append(L));
gen_with_pattern_1({pclosure, E1}) ->
    ?SUCHTHAT(S, 
              ?LET(L, list(gen_with_pattern_1(E1)),
                   lists:append(L)),
              S/=[]);
gen_with_pattern_1({char_class, Scope}) ->
    All=lists:append([case R of 
                          {S, E} ->lists:seq(S,E);
                          _ -> [R]
                      end||R<-Scope]),
    [eqc_gen:oneof(All)];
gen_with_pattern_1({repeat, N, E}) when N>1 ->
    ?LET({S1, S2}, 
         {gen_with_pattern_1({repeat,N-1, E}), gen_with_pattern_1(E)},
         S1++S2);
gen_with_pattern_1({repeat, _, E}) ->
    gen_with_pattern_1(E).
    
                                  
