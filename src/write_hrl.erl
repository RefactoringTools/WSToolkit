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
%%
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%% ====================================================================
%%% Generate Erlang type definitions according to an .xsd file.
%%%
%%%                    THIS IS ONLY PROTOTYPE!
%% =====================================================================
-module(write_hrl).

-export([write_hrl_file/2]).

-export([test/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").

-compile(export_all).

%%@private
test() ->
   write_hrl_file("../tests/weather/weather.xsd", "weather.hrl").
   write_hrl_file("../tests/bookstore_sample/booklist.xsd", "booklist.hrl").
   write_hrl_file("../tests/bookstore_sample/person.xsd", "person.hrl").
   write_hrl_file("../tests/bookstore_sample/complex_example.xsd", "complex_example.hrl").
   write_hrl_file("../tests/bookstore_sample/extension.xsd", "extension.erl"). %% does not work.
   write_hrl_file("../tests/bookstore_sample/book.xsd", "book.hrl").
   write_hrl_file("../tests/vodkatv_sample/vodkatv.xsd", "vodkatv.hrl").

%%@doc Generate type definitions. This function takes an .xsd file as input, 
%%     generates the Erlang representation of types, and write the results to 
%%     `OutFile'. The `erlsom' library is used to parse .xsd files, however
%%     `erlsom' has certain limitations, for instance in Erlsom, all restrictions
%%     on simple types are ignored, and those types are treated as 'string'. As
%%     a result, some of the types generated might not be as accurate as needed.
-spec write_hrl_file(XsdFile::file:filename(), OutFile::file:filename()) 
                    -> ok | {error, term()}.
write_hrl_file(XsdFile, OutFile) ->
    Result = gen_xsd_model:gen_xsd_model(XsdFile),
    case Result of
        {ok, Model} ->
            write_hrl_file_1(Model, OutFile);
        {error, Error} -> 
            throw({error, Error})
    end.

write_hrl_file_1(#model{tps = Types}, OutFile) ->
    Acc = header(),
    AllTypes=[T#type.nm||T<-Types, 
                         not is_list(T#type.anyAttr) 
                             orelse lists:keyfind(is_simple_type,1,T#type.anyAttr)==false],
    Content=write_type_defs(Types, AllTypes, Acc),
    file:write_file(OutFile, list_to_binary(Content)).

header() -> 
    "%%% This is an automatically generated Erlang file.\n\n".

write_type_defs(Types, AllTypes, Acc) ->
    WrittenTypesWithDeps=[write_a_type_def(T, AllTypes)
                          ||T<-lists:reverse(Types)],
    OrderedTypes =re_order_types(WrittenTypesWithDeps),
    Acc ++ lists:flatten(OrderedTypes).

write_a_type_def(#type{nm = '_document'}, _AllTypes) ->
    {'_document', "", []};
write_a_type_def(Type, AllTypes)->
    case ws_lib:is_simple_type(Type) of 
        true ->
            write_a_type_type(Type, AllTypes);
        false ->
             write_a_record_type(Type, AllTypes)
    end.

%% a simple type.         
write_a_type_type(_T=#type{nm = Name, els = Elements, mn=Min, mx=Max,atts = _Attributes}, AllTypes) ->
    [#el{alts =[Alt], mn=Min, mx=Max}]=Elements,
    {TypeDef, Comments} = write_alt_type(Alt,{1, 1}, AllTypes),
    WrittenType= "-type " ++ write_name(Name)++"()::"++TypeDef++"." ++ Comments++"\n\n",
    {Name, WrittenType, []}.
%% a complex type.
write_a_record_type(_T=#type{nm = Name, els = Elements, atts = Attributes}, AllTypes) ->
    Deps1 = lists:append([element_dep_types(E, AllTypes)||E<-Elements]),
    Deps2 = attribute_dep_types(Attributes, AllTypes),
    Deps = sets:to_list(sets:from_list(Deps1++Deps2))--[Name],
    Attrs= write_attributes(Attributes, AllTypes),
    Elems = write_elements(lists:reverse(Elements),AllTypes),
    WrittenType= "-record(" ++ write_name(Name) ++",\n"
        ++ spaces(8)++"{"++joinStrings(Attrs++Elems)
        ++ "}).\n\n",
    {Name, WrittenType, Deps}.

write_elements(Elements, AllTypes) ->
  write_elements(Elements, AllTypes, []).
write_elements([], _AllTypes, Acc) ->
  lists:reverse(Acc);
write_elements([Element], AllTypes, Acc) ->
    String= write_an_element(Element, AllTypes),
    [String|Acc];
write_elements([Element | Tail], AllTypes, Acc) ->
    String = write_an_element(Element, AllTypes),
    write_elements(Tail, AllTypes, [String|Acc]).

write_an_element(#el{alts = Alternatives, mn=Min, mx=Max}, AllTypes) ->
    write_alternatives(Alternatives, {Min, Max}, AllTypes).
    

%% easy case: 1 alternative (not a choice), 'real' element (not a group)
write_alternatives([], _,  _AllTypes) ->
    {"any_strict_but_none_defined", ""};
write_alternatives([#alt{tag = '#any'}], {_Min, Max},  _AllTypes) ->
    if Max==unbounded orelse Max>1 ->
            {"any :: [any()]", ""};
       true ->
            {"any :: any()", ""}
    end;
write_alternatives([A=#alt{tag = Tag, rl = true, tp=_Type}],  %%rl::real element?
                  {Min, Max}, AllTypes) ->
    {TypeDef, Comment} = write_alt_type(A, {Min, Max}, AllTypes),
    {write_name_without_prefix(Tag)++ "::"++TypeDef, Comment};

%% more than 1 alternative: a choice
write_alternatives(As=[#alt{}| _Tail], {Min, Max},AllTypes) ->
    AltTypeDefs = [write_alt_type(A, {Min, Max}, AllTypes)||A<-As],
    {TypeDefs, Comments} = lists:unzip(AltTypeDefs),
    UnionTypeDefs=write_union_type(TypeDefs),
    {"choice::"++UnionTypeDefs, lists:append(Comments)}.  
             

write_union_type([]) ->
    "";
write_union_type([T|Ts]) ->
    T++write_union_type_1(Ts).

write_union_type_1([]) ->
    "";
write_union_type_1([T|Ts]) ->
    "|"++T++write_union_type_1(Ts).

element_dep_types(#el{alts = Alternatives}, AllTypes) ->
    lists:append([alternative_dep_types(A, AllTypes)
                  ||A<-Alternatives]).    

alternative_dep_types(#alt{tag = _Tag, rl = true, tp=Type}, AllTypes) ->
    case lists:member(Type, AllTypes) of 
        true -> [Type];
        _ -> []
    end;
alternative_dep_types(_, _) ->
    [].

write_attributes(Attributes, AllTypes) ->
  [writeAttribute(A, AllTypes)||A<-Attributes].
              
writeAttribute(_A=#att{nm = Name, tp=Type}, AllTypes) -> 
    TypeDef=write_name(Name) ++"::"++ 
        write_type(Type, undefined, AllTypes),
    {TypeDef, ""}.

attribute_dep_types(Attrs, AllTypes)->
    [A#att.tp||A<-Attrs, lists:member(A#att.tp, AllTypes)].

joinStrings([]) ->
  "";

joinStrings([{StringA, Comment}]) ->
    case Comment of
        "" -> StringA++"\n"++spaces(8);
        _ ->
            StringA ++ "  " ++ "%% " ++ Comment ++ "\n"++spaces(8)
    end;
joinStrings([{StringA, Comment}|Tl]) ->
    NewStr=case Comment of
               "" -> StringA ++ ",\n" ++ spaces(9);
               _ ->
                   StringA ++ ",  " ++ "%% " ++ Comment ++ "\n" ++ spaces(9)
           end,
    NewStr++joinStrings(Tl).


spaces(N) ->
    lists:append(lists:duplicate(N, " ")).

write_name_without_prefix(Name) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    case reserved_word(Name) of
        true ->
            "'"++L++"'";
        false -> lists:flatten(io_lib:format("~p", [list_to_atom(L)]))
    end.
write_alt_type(A=#alt{tag = _Tag, tp=Type, mn=_Min, mx=_Mix, anyInfo=Constraints}, 
               {_ElemMin, _ElemMax}, AllTypes) 
  when is_list(Constraints)->
    Cs=[{C,V}||{C, V}<-Constraints, 
               not lists:member(C, [max_inclusive, min_inclusive,
                                    max_exclusive, min_exclusive,
                                    enumerations])],
    Comments = write_constraint_comments(Cs),
    TypeDef=case lists:keyfind(enumerations,1,Constraints) of
                {enumerations, Enums} ->
                    write_enum_type(Type, Enums);
                false ->
                    write_type(Type, Constraints, AllTypes)
            end,
    {TypeDef, Comments};
write_alt_type(_A=#alt{tag =_Tag, tp=T, anyInfo=Constraints},  {Min, Max}, AllTypes)->
    Type=write_type(T, Constraints, AllTypes),
    case {Min, Max} of  %% Min, Max are from elements attributed.
        {1, 1} ->
            {Type, ""};
        {1, unbounded}->
            {"nonempty_list("++Type++")", ""};
        {1, Max} when is_integer(Max) ->
            {"nonempty_list("++Type++")",  
             "MaxOccurs:" ++integer_to_list(Max)};
        {0, 1} ->
            {"none|"++Type, ""};
        {0, unbounded} ->
            {"none|nonempty_list("++Type++")", ""};
        {0, Max} when is_integer(Max) ->
            {"none|nonempty_list("++Type++")",  "%% MaxOccurs:" ++integer_to_list(Max)}
    end.
            
write_type({'#PCDATA', char}, _Constraints, _AllTypes) ->
    "string()";
write_type({'#PCDATA', bool}, _, _AllTypes) ->
    "boolean()";
write_type({'#PCDATA', Type}, Constraints, AllTypes) ->
    write_type(Type, Constraints, AllTypes);
write_type(integer, undefined, _AllTypes) ->
    "integer()";
write_type(integer, Constraints, _AllTypes) ->
    case lists:keyfind(min_inclusive,1, Constraints) of
        {min_inclusive, Min} ->
            case lists:keyfind(max_inclusive,1,Constraints) of 
                {max_inclusive, Max}->
                    io_lib:format("~p..~p", [Min, Max]);
                false ->
                    "integer()" %%TODO: COMMENT IS NEEDED.
            end;
        false -> "integer()"   %%TODO: COMMENT IS NEEDED.
    end;
write_type(positiveInteger, undefined, _AllTypes) ->
    "pos_integer()";
write_type(positiveInteger, Constraints, _AllTypes) ->
    case lists:keyfind(min_inclusive,1, Constraints) of
        {min_inclusive, Min} ->
            case lists:keyfind(max_inclusive,1,Constraints) of 
                {max_inclusive, Max}->
                    io_lib:format("~p..~p", [Min, Max]);
                false ->
                    "pos_integer()"
            end;
        false -> "pos_integer()"
    end;
write_type(negativeInteger, _Constraints, _AllTypes) ->
    "neg_integer()";
write_type(decimal, _, _) ->
    "float()";
write_type(double, _, _) ->
    "float()";
write_type(int, _, _) ->
    "-1 bsl 31 .. 1 bsl 31-1";
write_type(long, _, _) ->
    "-1 bsl 63 .. 1 bsl 63-1";
write_type(short, _, _) ->
    "-1 bsl 15 .. 1 bsl 15-1";
write_type(nonNegativeInteger, _, _) ->
    "non_neg_integer()";
write_type(nonPositiveInteger, _, _) ->
    "0|neg_integer()";
write_type(unsignedLong, _, _) ->
    "0 .. 1 bsl 64-1";
write_type(unsignedInt, _, _) ->
    "0..1 bsl 32-1";
write_type(unsignedShort, _, _) ->
    "0..1 bsl 16-1";
write_type(unsignedByte, _, _) ->
    "byte()";
write_type(Type, _, AllTypes) ->
    case lists:member(Type, AllTypes) of 
        true ->
            "#"++write_name(Type)++"{}";
        false ->
            write_name(Type) ++ "()"
    end.

write_name(bool) ->
    "boolean";
write_name(char) ->
    "string";
write_name(Name) ->
    L = [_H|_]= atom_to_list(Name),
    case reserved_word(Name) of
        true ->
            "'"++L++"'";
        false ->
            lists:flatten(io_lib:format("~p", [list_to_atom(L)]))
    end.

write_enum_type(string, [E|Enums]) ->
    "'"++E++"'"++write_enum_type_1(string, Enums);
write_enum_type(Type, [E|Enums]) ->
    E++write_enum_type_1(Type, Enums).
         
write_enum_type_1(_Type, []) ->
    "";
 %% use atom instead of string as strings are not allowed in type defs in Erlang.
write_enum_type_1(string, [E|Es]) -> 
    "| "++"'"++E++"'"++ write_enum_type_1(string, Es);
write_enum_type_1(_, [E|Es]) ->
    "| "++E ++ write_enum_type_1(integer, Es).


write_constraint_comments([]) ->
    "";
write_constraint_comments(Cs) ->
    write_constraint_comments(Cs, ["   %% Constraints: "]).

write_constraint_comments([], Acc)->
    lists:flatten(lists:reverse(Acc));
write_constraint_comments([{C, V}|Cs], Acc) ->
    Str=io_lib:format("~p:~p;", [C, V]),
    write_constraint_comments(Cs,[Str|Acc]).

reserved_word(W) ->
    lists:member(W, reserved_words()).

reserved_words()->
    ['after','begin','case','try','catch',
     'andalso','orelse','end','fun','if',
     'let','of','query','receive','when',
     'bnot','not','div','rem','band',
     'and', 'bor', 'bxor', 'bsl',
     'bsr', 'or', 'xor'].
     

re_order_types(TypesWithDeps) ->
    re_order_types(TypesWithDeps, []).
re_order_types([], Acc) ->
    lists:reverse(Acc);
re_order_types([H={TypeName, WrittenType, Deps}|Others], Acc) ->
    {Others1, Others2} = lists:partition(
                           fun({T, _, _}) -> 
                                   lists:member(T, Deps)
                           end, Others),
    Deps1 = lists:append([Ds||{_, _, Ds}<-Others1]),
    case Others1==[] orelse lists:member(TypeName, Deps1) of  %% recursive dependence.
        true ->
            re_order_types(Others, [WrittenType|Acc]);
        false -> re_order_types(Others1++[H|Others2], Acc)
    end.
    
