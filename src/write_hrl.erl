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

-export([test_bookstore/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").

%%@private
test_bookstore() ->
    write_hrl_file("../tests/bookstore_sample/booklist.xsd", "booklist.hrl"),
    write_hrl_file("../tests/bookstore_sample/book.xsd", "book.hrl").

%%@doc Generate type definitions. This function takes an .xsd file as input, 
%%     generates the Erlang representation of types, and write the results to 
%%     `OutFile'. The `erlsom' library is used to parse .xsd files, however
%%     `erlsom' has certain limitations, for instance in Erlsom, all restrictions
%%     on simple types are ignored, and those types are treated as 'string'. As
%%     a result, some of the types generated might not be as accurate as needed.
-spec write_hrl_file(XsdFile::file:filename(), OutFile::file:filename()) 
                    -> ok | {error, term()}.
write_hrl_file(XsdFile, OutFile) ->
    Result = erlsom:compile_xsd_file(XsdFile, []),
    case Result of
        {ok, Model} ->
            write_hrl_file_1(Model, OutFile);
        {error, Error} -> 
            throw({error, Error})
    end.

write_hrl_file_1(#model{tps = Types}, OutFile) ->
    Acc = header(),
    AllTypes=[T#type.nm||T<-Types],
    Content=write_types(Types, AllTypes, Acc),
    file:write_file(OutFile, list_to_binary(Content)).

header() -> 
    "%%% This is an automatically generated Erlang file.\n\n".

write_types(Types, AllTypes, Acc) ->
    WrittenTypesWithDeps=[write_a_record_type(T, AllTypes)
                          ||T<-lists:reverse(Types)],
    OrderedTypes =re_order_types(WrittenTypesWithDeps),
    Acc ++ lists:flatten(OrderedTypes).

write_a_record_type(#type{nm = '_document'}, _) ->
  {'_document', "", []};
write_a_record_type(Type, AllTypes) ->
    write_a_record_type_1(Type, AllTypes).

write_a_record_type_1(_T=#type{nm = Name, els = Elements, atts = Attributes}, AllTypes) ->
    Deps1 = lists:append([element_dep_types(E, AllTypes)||E<-Elements]),
    Deps2 = attribute_dep_types(Attributes, AllTypes),
    Deps = sets:to_list(sets:from_list(Deps1++Deps2))--[Name],
    Attrs= write_attributes(Attributes, AllTypes),
    Elems = write_elements(lists:reverse(Elements), AllTypes),
    WrittenType= "-record(" ++ write_name(Name) ++",\n"
        ++ "           {anyAttribs :: any(),\n" 
        ++ "            "++joinStrings(Attrs++Elems)
        ++ "\n           }).\n\n",
    {Name, WrittenType, Deps}.

write_elements(Elements, AllTypes) ->
  write_elements(Elements, AllTypes, [], 0).
write_elements([], _AllTypes, Acc, _) ->
  lists:reverse(Acc);
write_elements([Element], AllTypes, Acc, CountChoices) ->
    {String, _} = write_an_element(Element, AllTypes, CountChoices),
    [String|Acc];
write_elements([Element | Tail], AllTypes, Acc, CountChoices) ->
    {String, CountChoices2} = write_an_element(Element, AllTypes, CountChoices),
    write_elements(Tail, AllTypes, [String|Acc], CountChoices2).

write_an_element(#el{alts = Alternatives, mn=_Min, mx=Max}, AllTypes, CountChoices) ->
    if (is_integer(Max) andalso Max >1) orelse Max==unbound ->
            write_alternatives(Alternatives, AllTypes, CountChoices, true);
       true ->
            write_alternatives(Alternatives, AllTypes, CountChoices, false)
    end.


%% easy case: 1 alternative (not a choice), 'real' element (not a group)
write_alternatives([], _AllTypes, CountChoices, _List) ->
    {"any_strict_but_none_defined", CountChoices};
write_alternatives([#alt{tag = '#any'}], _AllTypes, CountChoices, List) ->
    if List ->
            {"any :: [any()]\n", CountChoices};
       true ->
            {"any :: any()\n", CountChoices}
    end;
write_alternatives([_A=#alt{tag = Tag, rl = true, tp=Type}], 
                   AllTypes, CountChoices, List) ->
    if List ->
            {write_name_without_prefix(Tag) ++write_list_type(Type,AllTypes), CountChoices};
       true ->
            {write_name_without_prefix(Tag) ++write_type(Type,AllTypes), CountChoices}
    end;
write_alternatives([_A=#alt{tag = Tag, rl = false, tp = {_,_Type}, mn=_Min, mx=_Max}],
                   _AllTypes, CountChoices, _List) ->
    {write_name_without_prefix(Tag) ++ "'\n", CountChoices};
write_alternatives([_A=#alt{rl = false, tp=Tp}], _AllTypes, CountChoices, _List) ->
    {write_name_without_prefix(Tp)++"\n",  CountChoices};
%% more than 1 alternative: a choice
write_alternatives([#alt{} | _Tail], _AllTypes, CountChoices, _List) ->
    Acc = case CountChoices of
              0 ->
                  "choice";
              _ -> 
                  "choice" ++ integer_to_list(CountChoices)
          end,
  {Acc, CountChoices +1}.

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
              
writeAttribute(#att{nm = Name, tp=Type}, AllTypes) -> 
    write_name(Name) ++ write_type(Type, AllTypes).

attribute_dep_types(Attrs, AllTypes)->
    [A#att.tp||A<-Attrs, lists:member(A#att.tp, AllTypes)].

joinStrings([]) ->
  "";
joinStrings([StringA]) ->
  StringA++"            ";
joinStrings([StringA|Tl]) ->
  StringA ++ ",\n            "++ joinStrings(Tl).

write_name_without_prefix(Name) ->
    L=[H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    case reserved_word(Name) of
        true ->
            "'"++L++"'";
        false when H >= $a andalso H =< $z -> 
            L;
        false -> "'"++L++"'"
    end.
   
write_type({'#PCDATA', char}, _AllTypes) ->
    " :: string()";
write_type({'#PCDATA', bool}, _AllTypes) ->
    " :: boolean()";
write_type({'#PCDATA', Type}, AllTypes) ->
    write_type(Type, AllTypes);
write_type(Type, AllTypes) ->
    case lists:member(Type, AllTypes) of 
        true ->
            " :: #"++write_name(Type)++"{}";
        false ->
            " :: "++ write_name(Type) ++ "()"
    end.

write_list_type({'#PCDATA', char}, _AllTypes) ->
    " :: [string()]";
write_list_type({'#PCDATA', bool}, _AllTypes) ->
    " :: [boolean()]";
write_list_type({'#PCDATA', Type}, AllTypes) ->
    write_list_type(Type, AllTypes);
write_list_type(Type, AllTypes) ->
    case lists:member(Type, AllTypes) of 
        true ->
            " :: [#"++write_name(Type)++"{}]";
        false ->
            " :: ["++ write_name(Type) ++ "()]"
    end.

write_name(bool) ->
    "boolean";
write_name(char) ->
    "string";
write_name(Name) ->
    L = [H|_]= atom_to_list(Name),
    case reserved_word(Name) of
       true ->
            "'"++L++"'";
       false when H >= $a andalso H =< $z -> 
            L;
       false -> "'"++L++"'"
    end.

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
    
%% TODO: to handle choice, group and any.
