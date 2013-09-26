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

-export([write_data_generators_to_file/2,
         write_data_generators_to_file/3,
         write_data_generators/1,
         write_data_generators/2]).

-export([ test1/0, test2/0, test3/0]).

-compile(export_all).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").


-include_lib("eqc/include/eqc.hrl").

%%@private
test0()->
    write_data_generators_to_file("../tests/convert_cooking/ConvertCooking.xsd", 
                                  "convert_cooking.erl").

%%@private
test1()->
    write_data_generators_to_file("../tests/bookstore_sample/complex_example.xsd", 
                                  "complex.erl").
%%@private
test2() ->
    write_data_generators_to_file("../tests/bookstore_sample/booklist.xsd", 
                                  "booklist.erl").
%%@private
test3()->
    write_data_generators_to_file("../tests/bookstore_sample/book.xsd",
                                  "book.erl").
%%@private
test4() ->
    write_data_generators_to_file("../tests/vodkatv_sample/vodkatv.xsd",
                                  "../tests/vodkatv_sample/vodkatv_expanded.wsdl",
                                  "vodkatv.erl").

%%@doc. Generates QuickCheck data generators according to the 
%%      type specification given in the XSD schema. If the 
%%      WSDL specification is provided, type generators are created 
%%      only for input types, otherwise type generators are created
%%      for both input and output types. The data generators created 
%%      are written to file `OutFile'.
-spec write_data_generators_to_file(XsdFile::file:filename(), 
                                    WsdlFile::file:filename()|none,
                                    OutFile::file:filename()) 
                                   -> ok | {error, Error::term()}.
write_data_generators_to_file(XsdFile, WsdlFile, OutFile) ->
    ModuleName = filename:basename(OutFile, ".erl"),
    Header = header(ModuleName),
    case write_data_generators(XsdFile, WsdlFile) of 
        {ok, Content} ->
            file:write_file(OutFile, list_to_binary(Header++Content));
        {error, Error} ->
            {error, Error}
    end.

%%@doc. For use when the WSDL file is not available.
%%@see write_data_generators_to_file/3.
-spec write_data_generators_to_file(XsdFile::file:filename(), 
                                    OutFile::file:filename()) 
                                   -> ok | {error, Error::term()}.
write_data_generators_to_file(XsdFile, OutFile) ->
    write_data_generators_to_file(XsdFile, none, OutFile).



%%@doc. Generates QuickCheck data generators according to the 
%%      type specification given in the XSD schema.
%%      The same as `write_data_generators_to_file/3', apart for 
%%      that this function returns the data generators as a 
%%      string.
-spec write_data_generators(XsdFile::file:filename(),
                            WsdlFile::file:filename()|none)
                           -> {ok, string()} | {error, Error::term()}.
write_data_generators(XsdFile, WsdlFile) ->
    case gen_xsd_model:gen_xsd_model(XsdFile) of 
        {ok, Model} ->
            io:format("Model:~p\n", [Model]),
            case WsdlFile of 
                none ->
                    {ok, write_data_generators_1(Model)};
                _ ->
                    InputDataTypes = get_input_data_types(WsdlFile, Model),
                    {ok, write_data_generators_1(Model, InputDataTypes)}
            end;
        {error, Error} ->
            {error, Error}
    end.

%%@doc.For use when the WSDL file is not available.
%%@see `write_data_generators/2'.
-spec write_data_generators(XsdFile::file:filename())
                           -> {ok, string()} | {error, Error::term()}.
write_data_generators(XsdFile) ->
    write_data_generators(XsdFile, none).

header(ModuleName) ->
    "-module("++ModuleName++").\n\n"
    "-include_lib(\"eqc/include/eqc.hrl\").\n\n"
    "-compile(export_all).\n\n".

write_data_generators_1(#model{tps = Types}) ->
    Acc ="\n\n"
         "%%----------------------------------------------------------\n"
         "%% Data generators\n"
        "%%----------------------------------------------------------\n",
    write_data_generators_2(Types, Acc).

write_data_generators_1(#model{tps = Types},InputDataTypes) ->
    Acc = "%%% Data Generators.\n\n",
    AllInputTypes = get_all_input_types(InputDataTypes, Types), 
    write_data_generators_2(AllInputTypes, Acc).
    
  
write_data_generators_2(Types, Acc) ->
    Generators=[write_a_data_gen(T)
                ||T<-Types],
    Acc ++ lists:flatten(rm_duplicates(lists:append(Generators))).
        

write_a_data_gen(#type{nm = '_document'}) ->
    "";
write_a_data_gen(Type) ->
    case ws_lib:is_simple_type(Type) of
        true ->
            write_a_simple_gen(Type);
        false ->
            write_a_complex_gen(Type)
    end.
write_a_simple_gen(_T=#type{nm = _Name, els = Elements, mn=_Min, mx=_Max,atts = _Attributes}) ->
    {_ElemNames, ElemDataGens} = lists:unzip(write_elements(Elements)),
    ElemDataGens.

write_a_complex_gen(T=#type{nm = Name, tp=Type, els = Elements, atts = Attributes}) ->
    io:format("T:~p\n", [T]),
    Attrs= write_attributes(Attributes),
    Elems = write_elements(lists:reverse(Elements)),
    {AttrNames, AttrDataGens} = lists:unzip(Attrs),
    {ElemNames, ElemDataGens} = lists:unzip(Elems),
    Head = "gen_"++camelCase_to_camel_case(atom_to_list(Name))++"()",
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
                         concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                        ||N<-AttrNames++ElemNames]);
                     _ ->
                         FieldNames = lists:reverse(AttrNames++ElemNames),
                         GenStr=concat_string(["gen_"++camelCase_to_camel_case(N)++"()"
                                               ||N<-FieldNames]),
                         Fields=gen_param_string(FieldNames),
                         "\n   ?LET({"++Fields++"},\n"++
                             "        {"++GenStr++"},\n"++
                             "        {"++Fields++"})"
                 end
         end,
    ComplexGen = Head++"->"++Body++".\n\n",
    case Head==Body of 
        true -> DataGens;
        _ ->
            [ComplexGen|DataGens]
    end.
    
  
write_elements(Elements)  ->
  write_elements(Elements, []).
write_elements([],Acc) ->
    lists:reverse(Acc);
write_elements([Element|Tail], Acc) ->
    case write_an_element(Element) of
        {none, none} ->
            write_elements(Tail, Acc);
        {Tag, String} ->
            write_elements(Tail, [{Tag, String}|Acc]);
        _Others ->
            write_elements(Tail, Acc)
    end.

    
write_an_element(#el{alts = Alternatives, mn=Min, mx=Max})->
    write_alternatives(Alternatives,{Min, Max}).

%% easy case: 1 alternative (not a choice), 'real' element (not a group)
write_alternatives([], _MinMax) ->
    {none, none};  %% ToFix!
write_alternatives([#alt{tag = '#any'}],_MinMax) ->
    {none, none};
write_alternatives([_A=#alt{tag = Tag, rl = true, tp=Type}], 
                   {_Min, Max}) when Tag==Type andalso Max==1 ->
    {none, none}; %%Checkthis!
write_alternatives([A=#alt{tag = Tag, tp=_Type}], 
                   {Min, Max}) ->
    IsList = (Max == unbounded) orelse (Max>1),
    case IsList of 
        true ->
            Tag1=list_to_atom(atom_to_list(Tag)++"_list"),
            {Tag1, write_name_without_prefix(Tag, IsList) ++
                 write_a_generator(A, {Min, Max})++
                 ".\n\n"};
        false ->
            {Tag, write_name_without_prefix(Tag, IsList) ++
                 write_a_generator(A, {Min, Max})++
                 ".\n\n"}
    end;
%% more than 1 alternative: a choice
write_alternatives([#alt{} | _Tail], _List) ->
    {none, none}. %%TO finished!

write_attributes(Attributes) ->
    [write_an_attribute(A)||A<-Attributes].

write_an_attribute(_A=#att{nm = Name, tp=Type}) ->
    Head ="gen_"++camelCase_to_camel_case(atom_to_list(Name))++"()->",
    Body = write_gen(Type, []),
    Code=Head++Body++".\n\n",
    {Name, Code}.

write_a_generator(A=#alt{}, 
                  {ElemMin, ElemMax}) ->
    Gen=write_a_generator_1(A),
    case {ElemMin, ElemMax} of 
        {1, 1} ->
            Gen;
        {1, unbounded} ->
            "eqc_gen:non_empty(eqc_gen:list("++Gen++")";
        {1, ElemMax} when is_integer(ElemMax) ->
            "eqc_gen:non_empty(eqc_gen:resize("
                ++integer_to_list(ElemMax)++", eqc_gen:list("++Gen++")))";
        {0,1}->
            "eqc_gen:oneof(none, "++Gen++")";
        {0, unbounded} ->
            "eqc_gen:list("++Gen++")";
        {0,ElemMax} when is_integer(ElemMax)->
            "eqc_gen:resize("
                ++integer_to_list(ElemMax)++", eqc_gen:list("++Gen++"))"
    end.
            
write_a_generator_1(#alt{tag = _Tag, tp=Type, mn=_Min, mx=_Mix, anyInfo=Constraints}) 
  when is_list(Constraints) ->
    case lists:keyfind(enumerations,1,Constraints) of
        {enumerations, Enums} ->
            write_enum_type(Type, Enums);
        false ->
            write_gen(Type, Constraints)
    end;
write_a_generator_1(#alt{tag = _Tag, tp=Type, mn=_Min, mx=_Mix}) ->
    write_gen(Type,[]).

write_name_without_prefix(Name, true) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()->"; 
write_name_without_prefix(Name, false) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"()->";
write_name_without_prefix(Name, _Max) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()->".
   
write_enum_type(_Type, Enums) ->                         
    lists:flatten(io_lib:format("eqc_gen:oneof(~p)", [Enums])).
  
write_gen({'#PCDATA', bool}, _Constraints) ->
    "boolean()";
write_gen({'#PCDATA', char}, []) ->
     "gen_lib:string()";
write_gen(char, []) ->
    "gen_lib:string()";
write_gen(string, Constraints) ->
    case lists:keyfind(pattern, 1, Constraints) of 
        {pattern, Pattern} ->
            "gen_lib:string("++"\""++Pattern++"\")";
        false ->
            {MinLen, MaxLen}=
                case lists:keyfind(length, 1, Constraints) of
                    {length, Len} ->
                       {Len, Len};
                    false ->
                        case lists:keyfind(max_length, 1, Constraints) of
                            {max_length, Max} ->
                                case lists:keyfind(min_length, 1, Constraints) of 
                                    {min_length, Min} ->
                                        {Min, Max};
                                    false ->
                                        {1, Max}
                                end;
                            false ->
                                case lists:keyfind(min_length, 1, Constraints) of 
                                    {min_length, Min} ->
                                        {Min, inf};
                                    false ->
                                        {1, inf}
                                end
                        end
                end,
            case {MinLen, MaxLen} of 
                {1, inf} -> "gen_lib:string()";
                _ ->
                    lists:flatten(
                      io_lib:format(
                        "gen_lib:string(~p,~p)", [MinLen, MaxLen]))
            end
    end;  
write_gen(Type, Constraints) ->
    case is_numeric_type(Type) of 
        true->
            case Type of 
                {'#PCDATA', Type1}->
                    gen_numeric(Type1, Constraints);
                _ ->
                    gen_numeric(Type, Constraints)
            end;
        false ->
            write_gen(Type)
    end.

write_gen(TypeName) when is_list(TypeName) ->
    "gen_"++camelCase_to_camel_case(TypeName)++"()";
write_gen(TypeName)when is_atom(TypeName) ->
    "gen_"++camelCase_to_camel_case(atom_to_list(TypeName))++"()";
write_gen(_TypeName)->
    "string()".   %% need to be fixed.

is_numeric_type({'#PCDATA', Type}) ->
    is_numeric_type(Type);
is_numeric_type(Type) ->
    lists:member(
      Type,element(1,lists:unzip(numeric_types()))).

numeric_types() ->
    [{decimal, {inf, inf}},
     {float,   {inf, inf}},
     {integer, {inf, inf}},
     {double,  {inf, inf}},
     {positiveInteger,{1, inf}},
     {negativeInteger,{inf, -1}},
     {int, {-1 bsl 31, 1 bsl 31-1}},
     {long,{-1 bsl 63, 1 bsl 63-1}},
     {short,{-1 bsl 15, 1 bsl 15-1}},
     {nonNegativeInteger,{0, inf}},
     {nonPositiveInteger,{inf, 0}},
     {unsignedLong, {0,1 bsl 64-1}},
     {unsignedInt, {0,1 bsl 32-1}},
     {unsignedShort,{0,1 bsl 16-1}},
     {unsignedByte,{0,1 bsl 8-1}}].
            
gen_numeric(Type, Constraints) ->
    case lists:keyfind(pattern, 1, Constraints) of 
        {pattern, Pattern} ->
            "gen_lib:integer("++"\""++Pattern++"\")";
        false ->
            LowerBound=case lists:keyfind(min_inclusive, 1, Constraints) of 
                         {min_inclusive, Min} ->
                             {min_inclusive, Min};
                         false ->
                             case lists:keyfind(min_exclusive, 1, Constraints) of 
                                 {min_exclusive, Min} ->
                                     {min_exclusive, Min};
                                 false ->
                                     inf
                             end
                       end,
            UpperBound=case lists:keyfind(max_inclusive, 1, Constraints) of 
                         {max_inclusive, Max} ->
                               {max_inclusive, Max};
                           false ->
                             case lists:keyfind(max_exclusive, 1, Constraints) of 
                                 {max_exclusive, Max} ->
                                     {max_exclusive, Max};
                                 false ->
                                     inf
                             end
                       end,
            gen_numeric_1(Type, {LowerBound, UpperBound})
    end.
            
gen_numeric_1(decimal, {_LowerBound, _UpperBound}) ->
    "eqc_gen:float()";
gen_numeric_1(float, {_LowerBound, _UpperBound}) ->
    "eqc_gen:float()";
gen_numeric_1(double, {_LowerBound, _UpperBound}) ->
    "eqc_gen:float()";
gen_numeric_1(Type, {LowerBound, UpperBound}) ->
    NewLower = get_new_lower(Type, LowerBound),
    NewUpper = get_new_upper(Type, UpperBound),
    lists:flatten(
      io_lib:format(
        "gen_lib:integer(~p,~p)", [NewLower, NewUpper])).


get_new_lower(Type, LowerBound) ->
    {Type, {L,_U}}=lists:keyfind(Type, 1, numeric_types()),
    case LowerBound of 
        {min_inclusive, M} ->
            case L of 
                inf -> M;
                _ ->
                    lists:max([M, L])
            end;
        {min_exclusive, M1} ->
            case L of 
                inf -> M1+1;
                _ ->
                    lists:max([M1+1, L])
            end;
        inf ->
            L
    end.            
get_new_upper(Type, UpperBound) ->
    {Type, {_L,U}}=lists:keyfind(Type, 1, numeric_types()),
    case UpperBound of 
        {max_inclusive, M} ->
            case U of 
                inf -> M;
                _ ->
                    lists:min([M, U])
            end;
        {max_exclusive, M1} ->
            case U of 
                inf -> M1-1;
                _ ->
                    lists:min([M1-1, U])
            end;
        inf ->
            U
    end.  
    
write_a_list_generator(Type) ->
    "list("++write_a_generator(Type, {1, 1})++")".
  
write_a_sized_list_generator(Type, Size) ->
    "?SIZED("++integer_to_list(Size)++", list("++
        write_a_generator(Type, {1, 1})++"))".

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
    to_upper(atom_to_list(P));
gen_param_string([H|T]) ->
    to_upper(atom_to_list(H))
        ++", "++gen_param_string(T).
    
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
    P;
concat_string([H|T]) ->
    H++", "++concat_string(T).


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
    T1=[Type||Type<-AllTypes, Type#type.nm==T#type.nm],
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
              
 
