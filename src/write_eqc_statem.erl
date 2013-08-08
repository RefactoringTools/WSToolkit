%%% ====================================================================
%%%                    This is only a prototype!
%% ====================================================================
-module(write_eqc_statem).

-export([write_eqc_statem/5]).

-export([test/0]).

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
%% -compile(export_all).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").

test() ->
    write_eqc_statem(
      "../tests/bookstore_sample/booklist_expanded.wsdl", 
      "../tests/bookstore_sample/booklist.xsd",
      none,
      "booklist_sut",
      "booklist_test.erl"),
     write_eqc_statem(
      "../tests/bookstore_sample/book-0321396855_expanded.wsdl", 
      "../tests/bookstore_sample/book.xsd",
      none,
      "book_sut",
      "book_test.erl"),
    write_eqc_statem(
      "../tests/bookstore_sample/vodkatv_expanded.wsdl", 
      "../tests/bookstore_sample/vodkatv.xsd",
      none, %%"vodkatv.hrl",
      "vaa_sut",
      "test_sut.erl").

write_eqc_statem(WsdlFile, XsdFile,HrlFile, SUT, OutFile) ->
    {ok, Model} = erlsom:compile_xsd_file("../priv/wsdl20.xsd"),
    Model1 = erlsom:add_xsd_model(Model),
    Result=erlsom:parse_file(WsdlFile, Model1),
    case Result of
        {ok, Res} ->
            {ok, DataModel} = erlsom:compile_xsd_file(XsdFile), 
            Choice = Res#'DescriptionType'.choice, 
            write_eqc_statem_1(Choice, DataModel, WsdlFile, XsdFile,HrlFile, SUT, OutFile);
        {error, Error} -> 
            throw({error, Error})
    end.

write_eqc_statem_1(Choice, DataModel, WsdlFile, XsdFile,HrlFile, SUT, OutFile) ->
    Interface=lists:keyfind('InterfaceType', 1, Choice),
    APIInterface=process_interface(Interface),
    write_eqc_statem_2(APIInterface, DataModel, WsdlFile, 
                       XsdFile,HrlFile, SUT, OutFile).
    

write_eqc_statem_2(APIInterface, DataModel, WsdlFile, 
                   XsdFile, HrlFile, SUT, OutFile) ->
    Commands =gen_commands(APIInterface, DataModel, []),
    PreConds = gen_preconditions(APIInterface, DataModel, []),
    PostConds=gen_postconditions(APIInterface, DataModel, []),
    NextState=gen_next_states(APIInterface, DataModel, []),
    AdaptorFuns = gen_adaptor_funs(APIInterface, DataModel, []),
    UtilFuns=util_funs(),
    {ok, DataGens}= write_data_gen:write_data_generators(XsdFile, WsdlFile),
    io:format("DataGens:~p\n", [DataGens]),
    Heading=create_heading(HrlFile, SUT, OutFile),
    Content = Heading ++ Commands ++PreConds ++ 
        PostConds ++ NextState++ AdaptorFuns++ 
        DataGens ++ UtilFuns,
    file:write_file(OutFile, list_to_binary(Content)).
   
                   
gen_commands([],_DataModel, Acc)->
    Cmds=lists:append(lists:reverse(Acc)),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% command\n"
    "%%----------------------------------------------------------\n"
    "command(_S)->\n"
    "    oneof([\n"
        ++Cmds++
    "      ]).\n\n";    
gen_commands([API],DataModel,Acc) ->
    Str=gen_a_command(API, DataModel)++"\n",
    gen_commands([], DataModel, [Str|Acc]);
gen_commands([A|As], DataModel, Acc) ->
    Str=gen_a_command(A, DataModel)++",\n",
    gen_commands(As, DataModel, [Str|Acc]).

gen_a_command({APIName, Param, _Response}, Model) ->
    APIName1=camelCase_to_camel_case(APIName),
    case lists:member(Param, ["#none", '#none', 'none', "none"]) of 
        true ->
            "      {call, ?MODULE, "++APIName1++", []}";
        _ ->
            ParamName = list_to_atom(lists:last(string:tokens(Param, [$:]))),
            Type = fetch_input_type(ParamName, Model),
            GenStrs = write_a_generator(Type),
            Params = concat_string(GenStrs),
            "      {call, ?MODULE, "++APIName1++", ["++Params++"]}"
    end.
           

fetch_input_type(ParamName, _Model=#model{tps = Types}) ->
    DocType = lists:keyfind('_document',#type.nm, Types),
    DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),
    TypeName=case lists:keyfind(ParamName, #alt.tag, DocAlts) of 
                 false -> ParamName;
                 V -> V#alt.tp
             end,
    case [T||T<-Types, T#type.nm==TypeName] of 
        [Type] ->
            Type;
        _ ->Msg="Could not find type definition for "
                ++ atom_to_list(ParamName),
            throw({error, Msg})
    end.
    
    
    
gen_preconditions([], _, Acc)->
    CondCs=lists:append(lists:reverse(Acc)),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% precondition\n"
    "%%----------------------------------------------------------\n"
    ++CondCs++".\n\n"; 
gen_preconditions([API],DataModel,Acc) ->
    Str=gen_a_precondition(API, DataModel),
    gen_preconditions([], DataModel, [Str|Acc]);
gen_preconditions([A|As], DataModel, Acc) ->
    Str=gen_a_precondition(A, DataModel)++";\n",
    gen_preconditions(As, DataModel, [Str|Acc]).  


gen_a_precondition({APIName, ParamType, _Response}, DataModel)->
    APIName1=camelCase_to_camel_case(APIName),
    ParamStr = case  lists:member(ParamType, ["#none", '#none', 'none', "none"]) of
                   true -> "";
                   false ->
                       ParamName = list_to_atom(
                                     lists:last(
                                       string:tokens(ParamType, [$:]))),
                       FieldNames = get_param_field_names(ParamName, DataModel),
                       gen_param_string(FieldNames, true)
               end,
    "precondition(_S, {call, ?MODULE, "++APIName1 ++ ", ["++ParamStr++"]})->\n"
    "    true".



gen_postconditions([], _, Acc)->
    CondCs=lists:append(lists:reverse(Acc)),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% postcondition\n"
    "%%----------------------------------------------------------\n"
    ++CondCs++".\n\n"; 
gen_postconditions([API],DataModel,Acc) ->
    Str=gen_a_postcondition(API, DataModel),
    gen_postconditions([], DataModel, [Str|Acc]);
gen_postconditions([A|As], DataModel, Acc) ->
    Str=gen_a_postcondition(A, DataModel)++";\n",
    gen_postconditions(As, DataModel, [Str|Acc]).  


gen_a_postcondition({APIName, ParamType, _Response}, DataModel)->
    APIName1=camelCase_to_camel_case(APIName),
    ParamStr = case  lists:member(ParamType, ["#none", '#none', 'none', "none"]) of
                   true -> "";
                   false ->
                       ParamName = list_to_atom(
                                     lists:last(
                                       string:tokens(ParamType, [$:]))),
                       FieldNames = get_param_field_names(ParamName, DataModel),
                       gen_param_string(FieldNames, true)
               end,
    "postcondition(_S, {call, ?MODULE, "++APIName1 ++ ", ["++ParamStr++"]}, _Result)->\n"
    "    true".


gen_adaptor_funs([], _, Acc)->
    Funs=lists:append(lists:reverse(Acc)),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% adaptor functions\n"
    "%%----------------------------------------------------------\n"
    ++Funs++"\n\n"; 
gen_adaptor_funs([API],DataModel,Acc) ->
    Str=gen_an_adaptor_fun(API, DataModel),
    gen_adaptor_funs([], DataModel, [Str|Acc]);
gen_adaptor_funs([A|As], DataModel, Acc) ->
    Str=gen_an_adaptor_fun(A, DataModel),
    gen_adaptor_funs(As, DataModel, [Str|Acc]).  


gen_an_adaptor_fun({APIName, ParamType, _Response}, DataModel)->
    APIName1=camelCase_to_camel_case(APIName),
    ParamStr = case  lists:member(ParamType, ["#none", '#none', 'none', "none"]) of
                   true -> "";
                   false ->
                       ParamName = list_to_atom(
                                     lists:last(
                                       string:tokens(ParamType, [$:]))),
                       FieldNames = get_param_field_names(ParamName, DataModel),
                       gen_param_string(FieldNames, false)
               end,
    APIName1++"("++ParamStr++")->\n"++
        "      ?SUT:"++APIName1++"("++ParamStr++").\n\n".

    

gen_next_states([], _, Acc)->
    CondCs=lists:append(lists:reverse(Acc)),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% next_state\n"
    "%%----------------------------------------------------------\n"
    ++CondCs++".\n\n"; 
gen_next_states([API],DataModel,Acc) ->
    Str=gen_a_next_state(API, DataModel),
    gen_next_states([], DataModel, [Str|Acc]);
gen_next_states([A|As], DataModel, Acc) ->
    Str=gen_a_next_state(A, DataModel)++";\n",
    gen_next_states(As, DataModel, [Str|Acc]).  


gen_a_next_state({APIName, ParamType, _Response}, DataModel)->
    APIName1=camelCase_to_camel_case(APIName),
    ParamStr = case  lists:member(ParamType, ["#none", '#none', 'none', "none"]) of
                   true -> "";
                   false ->
                       ParamName = list_to_atom(
                                     lists:last(
                                       string:tokens(ParamType, [$:]))),
                       FieldNames = get_param_field_names(ParamName, DataModel),
                       gen_param_string(FieldNames, true)
               end,
    "next_state(S, _R, {call, ?MODULE, "++APIName1 ++ ", ["++ParamStr++"]})->\n"
    "    S".

write_a_generator(#type{nm = _Name, tp=_Type, 
                        els = Elements, 
                        atts = Attributes}) ->
    Attrs= write_attributes(Attributes),
    Elems = write_elements(Elements),
    Attrs++Elems.

write_elements(Elements)  ->
  write_elements(Elements, []).
write_elements([], Acc) ->
  lists:reverse(Acc);
write_elements([Element | Tail], Acc) ->
    case write_an_element(Element) of
        none->
            write_elements(Tail, Acc);
        String ->
            write_elements(Tail, [String|Acc])
    end.

write_an_element(#el{alts = Alternatives, mn=_Min, mx=1})->
    write_alternatives(Alternatives, false);
write_an_element(#el{alts = Alternatives, mn=_Min, mx=Max}) 
  when Max==unbounded->
    write_alternatives(Alternatives, true);
write_an_element(#el{alts = Alternatives, mn=_Min, mx=Max})->
    if (is_integer(Max) andalso Max >1) ->
            write_alternatives(Alternatives,Max);
       true -> 
            write_alternatives(Alternatives,false)
    end.


%% easy case: 1 alternative (not a choice), 'real' element (not a group)
write_alternatives([],  _List) ->
    none;  %% ToFix!
write_alternatives([#alt{tag = '#any'}],_List) ->
    none; 
write_alternatives([_A=#alt{tag = Tag, rl = true, tp=_Type}], List) ->
    case List of 
        true ->write_name_without_prefix(Tag, List);
        false ->
            write_name_without_prefix(Tag, List); 
        Max when is_integer(Max) ->
            write_name_without_prefix(Tag, List)
    end;
write_alternatives([_A=#alt{tag = Tag, rl = false, tp = _Type, mn=_Min, mx=_Max}],
                    _List) ->
    write_name_without_prefix(Tag, false);

%% more than 1 alternative: a choice
write_alternatives([#alt{} | _Tail],_List) ->
    "".  %%TODO: to be completed!.

write_attributes(Attributes) ->
    [write_an_attribute(A)||A<-Attributes].

write_an_attribute(_A=#att{nm = Name, tp=_Type}) ->
    "gen_"++camelCase_to_camel_case(atom_to_list(Name))++"()".
    
write_name_without_prefix(Name, true) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()"; 
write_name_without_prefix(Name, false) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"()";
write_name_without_prefix(Name, _Max) ->
    L=[_H|_] = erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),
    "gen_"++camelCase_to_camel_case(L)++"_list()".
   
create_heading(HrlFile,SUT, OutFile)->
    BaseName = filename:basename(OutFile, ".erl"),
    IncludeHrl = case HrlFile of
                      none -> "";
                      _ -> "-include(\""++HrlFile++"\").\n"
                  end,
    "-module("++BaseName++").\n\n" ++ IncludeHrl++
        "-include_lib(\"eqc/include/eqc.hrl\").\n"
        "-include_lib(\"eqc/include/eqc_statem.hrl\").\n\n"
        "-define(SUT, "++SUT++").\n\n"
        "%%Prop\n"
        "-export([prop_state_machine/0]).\n\n"
        "-compile(export_all).\n\n"  
        "%% eqc callbacks \n"
        "-export([initial_state/0, command/1, precondition/2, \n"
        "         postcondition/3, next_state/3]).\n\n"
        "-record(state, {}).\n\n\n"
        "%%========================================================\n"
        "%% Prop\n"
        "%%========================================================\n"
        "prop_state_machine() ->\n"
        "    ?SETUP(\n"
        "      fun setup/0,\n"
        "      ?FORALL(\n"
        "         Cmds, commands(?MODULE),\n"
        "         begin\n"
        "           setup(),\n"
        "           {_H, _S, Res} = run_commands(?MODULE, Cmds),\n"
        "           teardown(),\n"
        "           Res==ok\n"
        "         end)).\n\n"
        "%%=========================================================\n"
        "%% eqc callbacks\n"
        "%%=========================================================\n"
        "%%---------------------------------------------------------\n"
        "%% initial_state\n"
        "%%---------------------------------------------------------\n"
        "initial_state()->\n"
        "   #state{}.".
           

util_funs() ->
    "%%---------------------------------------------------------------\n"
    "%% Utilities\n"
    "%%---------------------------------------------------------------\n"
    "setup() -> \n"
    "   fun teardown/0.\n\n"
    "teardown() ->\n"
    "   ok.".
              

get_param_field_names(TypeName, _DataModel=#model{tps = Types}) ->
    DocType = lists:keyfind('_document', #type.nm,Types),
    DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),
    Type=case lists:keyfind(TypeName, #alt.tag, DocAlts) of 
             false -> TypeName;
             V -> V#alt.tp
         end,
    #type{nm=Type, els=Elems, atts=Attrs}=lists:keyfind(Type, #type.nm, Types),
    Names1 = get_element_names(Elems),
    Names2 = get_attr_names(Attrs),
    Names1++Names2.
         
get_element_names(Elems) ->
    lists:append([get_element_name(E)||E<-Elems]).

get_element_name(#el{alts = Alternatives}) ->
    [get_element_name_1(Alt)||Alt<-Alternatives].
    
%%TODO: need to complete!!!
get_element_name_1(#alt{tag = Tag, rl = true, tp=_Type, mx=_Max}) ->
    Tag.


get_attr_names(Attrs) ->
    [get_attr_name(A)||A<-Attrs].
 
get_attr_name(#att{nm = Name}) ->
   Name.

   
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
    

%% transform camelCase atom to camel_case.
-spec(camelCase_to_camel_case(Name::string()) ->string()).
camelCase_to_camel_case(Name) ->
    case Name of 
        [H|T] when (H >= 65) and (90 >= H)->
            camelCase_to_camel_case_1([H+32|T],[]);
        _ ->
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
camelCase_to_camel_case_1([H|T], Acc) ->
    camelCase_to_camel_case_1(T, [H|Acc]).
    

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


gen_param_string([], _) ->
    "";
gen_param_string([P], WithUnderScore) ->
    if is_atom(P) ->
            to_upper(atom_to_list(P), WithUnderScore);
       true ->
            to_upper(P, WithUnderScore)
    end;
gen_param_string([H|T], WithUnderScore) ->
    if is_atom(H) ->
            to_upper(atom_to_list(H), WithUnderScore)
                ++", "++gen_param_string(T, WithUnderScore);
       true ->
             to_upper(H, WithUnderScore)
                ++", "++gen_param_string(T,WithUnderScore)
    end.
to_upper([H|T],WithUnderScore) -> 
    case WithUnderScore of 
        true ->
            "_"++normalise([string:to_upper(H)|T]);
        false ->
            normalise([string:to_upper(H)|T])
    end.

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
