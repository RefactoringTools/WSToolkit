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
-import(wsdl_dsl, [wsdlType/1, xint/0, pattern/2]).
-include_lib("eqc/include/eqc.hrl").

%% integer generator.
integer() ->
    wsdlType(xint()).

%% generates an integer according to the patttern.
integer(Pattern) when is_list(Pattern)->
    wsdlType(pattern(regexp_gen:from_string(Pattern), xint())).

%% generates an integer within the range.
integer(Min, Max) ->
    wsdlType(wsdl_dsl:minInclusive(Min, wsdl_dsl:maxInclusive(Max, xint()))).
 
%% generatea a printable string.
string() ->
    wsdlType(wsdl_dsl:string()).

%% generatea a string whose length is within the range.
string(MinLen, MaxLen) ->
    wsdlType(wsdl_dsl:minLength(MinLen, wsdl_dsl:maxLength(MaxLen, wsdl_dsl:string()))).

%% generatea a string according to a pattern.
string(Pattern) ->
    wsdlType(pattern(regexp_gen:from_string(Pattern), wsdl_dsl:string())).
