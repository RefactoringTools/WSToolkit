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
-module(write_data_gen).

-export([write_data_generators_to_file/3,
         write_data_generators/2]).

-export([test/0]).


%% -compile(export_all).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").

test() ->
    write_data_generators_to_file("../tests/bookstore_sample/booklist.xsd", "booklist.erl"),
    write_data_generators_to_file("../tests/bookstore_sample/book.xsd", "book.erl"),
    write_data_generators_to_file("../tests/bookstore_sample/vodkatv.xsd",
                                  "../tests/bookstore_sample/vodkatv_expanded.wsdl",
                                  "vodkatv.erl").
   

write_data_generators_to_file(XsdFile, OutFile) ->
    write_data_generators_to_file(XsdFile, none, OutFile).
write_data_generators_to_file(XsdFile, WsdlFile, OutFile) ->
    ModuleName = filename:basename(OutFile, ".erl"),
    Header = header(ModuleName),
    case write_data_generators(XsdFile, WsdlFile) of 
        {ok, Content} ->
            file:write_file(OutFile, list_to_binary(Header++Content));
        {error, Error} ->
            {error, Error}
    end.
            

header(ModuleName) ->
    "-module("++ModuleName++").\n\n"
    "-include_lib(\"eqc/include/eqc.hrl\").\n\n"
    "-compile(export_all).\n\n".

write_data_generators(XsdFile, none) ->
    Result = erlsom:compile_xsd_file(XsdFile, []),
    case Result of
        {ok, Model} ->
            {ok, write_data_generators_1(Model)};
        {error, Error} -> 
            {error, Error}
    end;

write_data_generators(XsdFile, WsdlFile) ->
    Result = erlsom:compile_xsd_file(XsdFile, []),
    case Result of
        {ok, Model} ->
            InputDataTypes = get_input_data_types(WsdlFile, Model),
            io:format("InputDataTypes:~p\n", [InputDataTypes]),
            {ok, write_data_generators_1(Model, InputDataTypes)};
        {error, Error} -> 
            {error, Error}
    end.

write_data_generators_1(#model{tps = Types}) ->
    Acc ="\n\n"
         "%%----------------------------------------------------------\n"
         "%% Data generators\n"
        "%%----------------------------------------------------------\n",
    Gens=write_data_generators_2(Types, Acc),
    Gens++util_funs().
write_data_generators_1(#model{tps = Types},InputDataTypes) ->
    Acc = "%%% Data Generators.\n\n",
    AllInputTypes = get_all_input_types(InputDataTypes, Types), 
    Gens=write_data_generators_2(AllInputTypes, Acc),
    Gens++util_funs().
  
write_data_generators_2(Types, Acc) ->
    Generators=[write_a_data_generator(T)
                ||T<-Types],
    Acc ++ lists:flatten(rm_duplicates(lists:append(Generators))).
        

write_a_data_generator(#type{nm = '_document'}) ->
    "";
write_a_data_generator(Type) ->
    write_a_data_generator_1(Type).

write_a_data_generator_1(#type{nm = Name, tp=Type, els = Elements, atts = Attributes}) ->
    Attrs= write_attributes(Attributes),
    Elems = write_elements(lists:reverse(Elements)),
    {AttrNames, AttrDataGens} = lists:unzip(Attrs),
    {ElemNames, ElemDataGens} = lists:unzip(Elems),
    Head = "gen_"++camelCase_to_camel_case(atom_to_list(Name))++"()->",
    DataGens = lists:reverse(AttrDataGens)++
        lists:reverse(ElemDataGens),
    Body=case Type of 
             choice ->
                 AttrGenStr=concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                           ||N<-AttrNames]),
                 ElemGenStr=concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                           ||N<-ElemNames]),
                 "\n   {"++AttrGenStr++", oneof("++ElemGenStr++")}.\n\n";                 
             _ ->case length(Attrs++Elems) of 
                     1->
                         GenStr=concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                               ||N<-AttrNames++ElemNames]),
                         GenStr++".\n\n";
                     _ ->
                         FieldNames = lists:reverse(AttrNames++ElemNames),
                         GenStr=concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                               ||N<-FieldNames]),
                         Fields=gen_param_string(FieldNames),
                         "\n   ?LET({"++Fields++"},\n"++
                             "        {"++GenStr++"},\n"++
                             "        {"++Fields++"}).\n\n"
                 end
         end,
    ComplexGen = Head++Body,
    [ComplexGen|DataGens].
    
    
   
write_elements(Elements)  ->
  write_elements(Elements, [], 0).
write_elements([], Acc, _) ->
  lists:reverse(Acc);
write_elements([Element | Tail], Acc, CountChoices) ->
    case write_an_element(Element, CountChoices) of
        {none, none, CountChoices2} ->
            write_elements(Tail, Acc, CountChoices2);
        {Tag, String, CountChoices2} ->
            write_elements(Tail, [{Tag, String}|Acc], CountChoices2)
    end.

write_an_element(#el{alts = Alternatives, mn=_Min, mx=1}, CountChoices)->
    write_alternatives(Alternatives, CountChoices, false);
write_an_element(#el{alts = Alternatives, mn=_Min, mx=Max}, CountChoices) 
  when Max==unbounded->
    write_alternatives(Alternatives, CountChoices, true);
write_an_element(#el{alts = Alternatives, mn=_Min, mx=Max}, CountChoices)->
    if (is_integer(Max) andalso Max >1) ->
            write_alternatives(Alternatives, CountChoices, Max);
       true -> 
            write_alternatives(Alternatives,CountChoices, false)
    end.


%% easy case: 1 alternative (not a choice), 'real' element (not a group)
write_alternatives([], CountChoices, _List) ->
    {"any_strict_but_none_defined", CountChoices};  %% ToFix!
write_alternatives([#alt{tag = '#any'}],CountChoices, List) ->
    if List ->   
            {"any :: [any()]\n", CountChoices}; %%TOFIX!!
       true ->
            {"any :: any()\n", CountChoices}  %%TOFIX!
    end;
write_alternatives([#alt{tag = Tag, rl = true, tp=Type}], 
                   CountChoices, List) when Tag==Type andalso List==false ->
    {none, none, CountChoices};
write_alternatives([_A=#alt{tag = Tag, rl = true, tp=Type}], 
                   CountChoices, List) ->
    case List of 
        true ->
            Tag1=list_to_atom(atom_to_list(Tag)++"_list"),
            {Tag1, write_name_without_prefix(Tag, List) ++
                 write_a_list_generator(Type)++
                 ".\n\n", CountChoices};
        false ->
            {Tag, write_name_without_prefix(Tag, List) ++
                 write_a_generator(Type)++
                 ".\n\n", CountChoices};
        Max when is_integer(Max) ->
            Tag1=list_to_atom(atom_to_list(Tag)++"List"),
            {Tag1, write_name_without_prefix(Tag, List) ++
                 write_a_sized_list_generator(Type, Max)++
                 ".\n\n", CountChoices}
    end;
write_alternatives([_A=#alt{tag = Tag, rl = false, tp = Type, mn=Min, mx=Max}],
                   CountChoices, _List) ->
    {Tag, write_name_without_prefix(Tag, false)++
         write_a_generator(Type, Min, Max) 
     ++ ".\n\n",CountChoices};
%% more than 1 alternative: a choice
write_alternatives([#alt{} | _Tail],CountChoices, _List) ->
    Acc = case CountChoices of
              0 ->
                  "choice";
              _ -> 
                  "choice" ++ integer_to_list(CountChoices)
          end,
  {Acc, CountChoices +1}.

write_attributes(Attributes) ->
    [write_an_attribute(A)||A<-Attributes].

write_an_attribute(_A=#att{nm = Name, tp=Type}) ->
    Head ="gen_"++camelCase_to_camel_case(atom_to_list(Name))++"()->",
    Body = write_a_generator(Type),
    Code=Head++Body++".\n\n",
    {Name, Code}.
  
write_a_generator(Type) ->
    write_gen(Type).

write_a_generator(Type, _Min, _Max) ->
    write_gen(Type). %% todo: to take the range into account.
write_name_without_prefix(Name, true) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()->"; 
write_name_without_prefix(Name, false) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"()->";
write_name_without_prefix(Name, _Max) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()->".
   
write_gen(bool) -> "bool()";
write_gen(integer) -> "int()"; 
write_gen(float) -> "real()"; 
write_gen(char) -> "list(char())";
write_gen({'#PCDATA', char}) ->
    "list(char())";
write_gen({'#PCDATA', bool}) ->
    "bool()";
write_gen({'#PCDATA', integer}) ->
    "int()";
write_gen({'#PCDATA', positiveInteger}) ->
    "integer(1,inf)";
write_gen({'#PCDATA', float}) ->
    "real()";
write_gen(TypeName) when is_list(TypeName) ->
    "gen_"++camelCase_to_camel_case(TypeName)++"()";
write_gen(TypeName)when is_atom(TypeName) ->
    "gen_"++camelCase_to_camel_case(atom_to_list(TypeName))++"()";
write_gen(_TypeName)->
    "string()".   %% need to be fixed.

write_a_list_generator(Type) ->
    "list("++write_a_generator(Type)++")".
  
write_a_sized_list_generator(Type, Size) ->
    "?SIZED("++integer_to_list(Size)++", list("++
        write_a_generator(Type)++"))".

%% transform camelCase atom to camel_case.
%%-spec(camelCase_to_camel_case(Name::string()) ->string()).
camelCase_to_camel_case(Name) when is_atom(Name) ->
    camelCase_to_camel_case(atom_to_list(Name));
camelCase_to_camel_case(Name) ->
    case Name of 
        [H|T] when (H >= 65) and (90 >= H)->
            camelCase_to_camel_case_1([H+32|T],[]);
        [H|T] when H==45 ->
            camelCase_to_camel_case_1([95|T],[]);
        _  ->
            camelCase_to_camel_case_1(Name,[])
    end.

camelCase_to_camel_case_1([], Acc) ->
    lists:reverse(Acc);
camelCase_to_camel_case_1([H|T], Acc)
  when  (H >= 65) and (90 >= H)->
    case Acc of 
        [95|_] ->
            camelCase_to_camel_case_1(T, [H + (97 - 65) |Acc]);
        _ ->
            camelCase_to_camel_case_1(T, [H + (97 - 65), 95|Acc])
    end;
camelCase_to_camel_case_1([H|T], Acc) when H==45->
    camelCase_to_camel_case_1(T, [95|Acc]);
camelCase_to_camel_case_1([H|T], Acc)->
    camelCase_to_camel_case_1(T, [H|Acc]).
    
    
gen_param_string([]) ->
    "";
gen_param_string([P]) ->
    if is_atom(P) ->
            to_upper(atom_to_list(P));
       true ->
            to_upper(P)
    end;
gen_param_string([H|T]) ->
    if is_atom(H) ->
            to_upper(atom_to_list(H))
                ++", "++gen_param_string(T);
       true ->
             to_upper(H)
                ++", "++gen_param_string(T)
    end.
to_upper([H|T]) -> 
    normalise([string:to_upper(H)|T]).

normalise([H|T]) ->
    case   (is_upper(H) or is_lower(H) or 
            is_digit(H) or (H == 64) or (H == 95)) of
        true ->
            [H|normalise(T)];
        false ->
            [95|normalise(T)]
    end;
normalise([]) ->[].


is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).


concat_string([]) ->
    "";
concat_string([P]) ->
    if is_atom(P) ->
           atom_to_list(P);
       true ->
            P
    end;
concat_string([H|T]) ->
    if is_atom(H) ->
            atom_to_list(H)
                ++", "++concat_string(T);
       true ->
            H++", "++concat_string(T)
    end.

rm_duplicates(Elems) ->
    rm_duplicates_1(Elems, []).

rm_duplicates_1([], Acc) ->
    lists:reverse(Acc);
rm_duplicates_1([E|Elems], Acc) ->
    case lists:member(E, Acc) of 
        true ->
            rm_duplicates_1(Elems, Acc);
        false ->
            rm_duplicates_1(Elems, [E|Acc])
    end.
util_funs() ->
    "integer(inf, inf) ->\n"
    "    int();\n"
    "integer(Min, inf) ->\n"
    "    ?SUCHTHAT(I, int(), I>=Min);\n"
    "integer(inf, Max) ->\n"
    "    ?SUCHTHAT(I, int(), I=<Max);\n"
    "integer(Min, Max) ->\n"
    "    ?SUCHTHAT(I, int(), I>=Min andalso I=<Max).\n".

get_input_data_types(WsdlFile, Model) ->
    {ok, Model1} = erlsom:compile_xsd_file("../priv/wsdl20.xsd"),
    Model2 = erlsom:add_xsd_model(Model1),
    Result=erlsom:parse_file(WsdlFile, Model2),
    case Result of
        {ok, Res} ->
            Choice = Res#'DescriptionType'.choice, 
            Interface=lists:keyfind('InterfaceType', 1, Choice),
            APIInterface=process_interface(Interface),
            lists:append([get_input_data_types_2(I, Model)
                          ||I<-APIInterface]);
        {error, Error} -> 
            throw({error, Error})
    end.

get_input_data_types_2({_APIName, Param, _Response}, #model{tps = Types})->
    case lists:member(Param, ["#none", '#none', 'none', "none"]) of 
        true ->
            [];
        _ ->
            ParamName=list_to_atom(lists:last(string:tokens(Param, [$:]))),
            DocType = lists:keyfind('_document',#type.nm, Types),
            DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),
            TypeName=case lists:keyfind(ParamName, #alt.tag, DocAlts) of 
                         false -> ParamName;
                         V -> V#alt.tp
                     end,
            [T||T<-Types, T#type.nm==TypeName]
    end.
    

process_interface(_Interface=#'InterfaceType'{choice=Choice}) ->
    [process_interface_operation(I)||
        I<-Choice,
        element(1, I)=='InterfaceOperationType'].
process_interface_operation(_I=#'InterfaceOperationType'{name=Name, choice=Choice})->
    InputType = lists:keyfind('InterfaceOperationType-input', 1, Choice),
    OutputType= lists:keyfind('InterfaceOperationType-output', 1, Choice),
    In=case InputType of 
           false ->
               none;
           #'InterfaceOperationType-input'{element=E1} ->
               E1
       end,
    Out=case OutputType of 
           false ->
               none;
           #'InterfaceOperationType-output'{element=E2} ->
               E2
       end,
    {Name, In, Out}.

get_all_input_types(InputDataTypes, AllTypes)-> 
    get_all_input_types(InputDataTypes, AllTypes, []).

get_all_input_types([], _, Acc) ->
    rm_duplicates(lists:reverse(Acc));
get_all_input_types([T|Ts], AllTypes, Acc) ->
    case  get_direct_dependent_types(T, AllTypes) of
        {[], _} ->
            %% This should not happen!
            get_all_input_types(Ts, AllTypes, Acc);
        {[T1], Deps}  ->
            NewTypes = Deps -- Acc,
            get_all_input_types(NewTypes++Ts, AllTypes, [T1|Acc])
    end.

get_direct_dependent_types(T, AllTypes) -> 
    T1=[Type||Type<-AllTypes, Type#type.nm==T],
    case T1 of
        [] -> {[], []};
        [T2]->{T1,get_direct_dependent_types_1(T2)}
    end.

get_direct_dependent_types_1(#type{nm = _Name, tp=_Type, 
                                   els = Elements, 
                                   atts = Attributes}) ->
    Attrs= [A#att.nm||A<-Attributes],
    Elems = [A#alt.tag||E<-Elements, A<-E#el.alts],
    Attrs ++ Elems.
              
