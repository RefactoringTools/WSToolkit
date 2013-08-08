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
%%% Generate the sut.erl file according to a 'model'; to be used with erlsom.
%%%
%%%                    This is only a prototype!
%% ====================================================================
-module(gen_refac_script).

-export([gen_test_model_refacs/2, gen_composite_refac/3]).

-export([test/0, test1/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").

test() ->
    Cmds=gen_test_model_refacs({"../priv/vodkatv_v0.xsd",  "../priv/vodkatv_v0.wsdl"},
                               {"../priv/vodkatv.xsd",  "../priv/vodkatv_expanded.wsdl"}),
    gen_composite_refac("vodkatv_sut_gen_0.erl", Cmds, "refac_evolve_api.erl").


test1() ->
    Cmds=gen_test_model_refacs(
           {"../priv/vodkatv.xsd",  "../priv/vodkatv_expanded.wsdl"},
           {"../priv/vodkatv_v0.xsd",  "../priv/vodkatv_v0.wsdl"}),
    gen_composite_refac("vodkatv_sut_gen.erl", Cmds, "refac_evolve_api.erl").

   


gen_composite_refac(TestFile, RefacCmds, OutFile) ->
    Header = gen_header(OutFile),
    CompositeRefac = gen_composite_refacs(TestFile, RefacCmds, ""),
    Header++lists:flatten(CompositeRefac).


gen_header(OutFile) ->
    BaseName=filename:basename(OutFile, ".erl"),
    "\n-module("++BaseName++").\n\n"++
     "-export([composite_refac/1, input_par_prompts/0, select_focus/1]).\n\n"
     "-include_lib(\"wrangler/include/wrangler.hrl\").\n\n"
     "-behaviour(gen_composite_refac).\n\n"
     "input_par_prompts() ->[].\n\n"
     "select_focus(_Args) -> {ok, none}.\n\n"
     "composite_refac(_Args=#args{current_file_name=File})->\n".

gen_composite_refacs(_TestFile, [], Acc)->
    "         ?atomic([\n"
        ++append_strs(lists:reverse(Acc))
        ++"                ]).\n\n";
gen_composite_refacs(TestFile, [{rm_operation, OpName}|Refacs], Acc) ->
    Str=lists:flatten(io_lib:format("                 {refactoring, rm_operation, [File,\"~s\", [File], 'emacs']}", [OpName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{rm_argument, OpName, Index, _ArgName}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 {refactoring, rm_op_arg, [File,~s,~p, [File], 'emacs']}", 
                          [OpName, Index])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{add_operation, OpName, FieldNames}|Refacs], Acc) ->
    Str = gen_add_operation_refac_cmd(TestFile, {OpName, FieldNames}),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{add_argument, OpName, Index, ArgName, _ArgType}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(add_argument, [File,~s,~p,~p])", 
                          [OpName, Index, ArgName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]).


gen_add_operation_refac_cmd(_TestFile, {OpName, FieldNames}) ->
    Args=make_args(FieldNames),
    GeneratorArgs = make_generators(FieldNames),
    StateIndex=1,
    StateCode="next_state(S, _R, {call, ?MODULE, "++OpName++", "++Args++"}) -> S;\n",
    PreCondIndex=1,
    PreCondCode="precondition(S, {call, ?MODULE, "++OpName++", "++Args++"})-> true;\n",
    PostCondIndex=1,
    PostCondCode="postcondition(S, {call, ?MODULE, "++OpName++", "++Args++"}, Result)-> true;\n",
    CmdsIndex=1,
    CmdsCode="{call, ?MODULE, "++OpName++", "++ GeneratorArgs++"},\n",
    io_lib:format("                 ?refac_(add_operation, [~p,~p,
                                               ~p,~p,
                                               ~p,~p,
                                               ~p,~p])", 
                  [StateIndex, StateCode, PreCondIndex, PreCondCode, 
                   PostCondIndex, PostCondCode, CmdsIndex, CmdsCode]).
    
gen_test_model_refacs({XsdFile1, WsdlFile1}, {XsdFile2, WsdlFile2}) ->         
    {{_, {_,TypeChanges}, _}, APIChanges} = ws_diff:ws_diff({XsdFile1, WsdlFile1}, {XsdFile2, WsdlFile2}),
    {ok, DataModel} = erlsom:compile_xsd_file(XsdFile1),
    gen_refacs(TypeChanges, APIChanges, DataModel, []).

gen_refacs(_TypeChanges, [], _DataModel,Refacs)->
    lists:reverse(lists:flatten(Refacs));
gen_refacs(TypeChanges, [{'*', _}|Others], DataModel, Refacs) ->
    gen_refacs(TypeChanges, Others, DataModel, Refacs);
gen_refacs(TypeChanges, [{d, {APIName, _, _, _, _}}|Others], DataModel, Refacs) ->
    APIName1 = camelCase_to_camel_case(APIName),
    gen_refacs(TypeChanges, Others, DataModel, [[{rm_operation, [APIName1]}]|Refacs]);
gen_refacs(TypeChanges, [{i, {APIName, InType, OutType, _Method, _URI}}|Others], DataModel, Refacs) ->
    NewRefac=generate_add_op_refac_cmd(APIName, InType, OutType, DataModel),
    gen_refacs(TypeChanges, Others, DataModel, [NewRefac|Refacs]);
gen_refacs(TypeChanges, [{'*!', {APIName, InType, OutType, _Method, _URI}}|Others], DataModel, Refacs) ->
    NewRefac=generate_interface_refac_cmd(APIName, TypeChanges, InType, OutType),
    gen_refacs(TypeChanges, Others, DataModel, [NewRefac|Refacs]);
gen_refacs(TypeChanges, [{s, {APIName1, InType1, OutType1, _Method1, _URI1},
                          {_APIName2, _InType2, _OutType2, _Method2, _URI2}}
                         |Others], DataModel, Refacs) ->
    NewRefac=generate_add_op_refac_cmd(APIName1, InType1, OutType1,DataModel),
    gen_refacs(TypeChanges, Others, DataModel, [NewRefac|Refacs]).

    
generate_add_op_refac_cmd(APIName, InType, _OutType,DataModel) ->
    APIName1 = camelCase_to_camel_case(APIName),
    FieldNames=
        case lists:member(InType, ["#none", '#none', 'none', "none"]) of 
            true ->
                [];
            false ->
                get_param_fields(InType, DataModel)                 
        end,
    [{add_operation, APIName1, FieldNames}].
               
generate_interface_refac_cmd(APIName, TypeChanges, InType, _OutType) ->
    APIName1 = camelCase_to_camel_case(APIName),
    case lists:keyfind(InType, 1, TypeChanges) of 
        false -> [];
        {InType, ParaChanges} ->
            generate_interface_refac_cmd_1(APIName1, ParaChanges, 1, [])
    end.
    
generate_interface_refac_cmd_1(_APIName, [], _Index, Acc) ->
    lists:reverse(Acc);
generate_interface_refac_cmd_1(APIName, [{'*', {_, _}}|Others], Index, Acc) ->
    generate_interface_refac_cmd_1(APIName, Others, Index+1, Acc);
generate_interface_refac_cmd_1(APIName, [{d, {ParName, _Type}}|Others], Index, Acc) ->
    Cmd = [{rm_argument, APIName, Index, ParName}],
    generate_interface_refac_cmd_1(APIName, Others, Index, [Cmd|Acc]);
generate_interface_refac_cmd_1(APIName, [{i, {ParName, Type}}|Others], Index, Acc) ->
    Cmd = [{add_argument, APIName, Index, ParName, Type}],
    generate_interface_refac_cmd_1(APIName, Others, Index+1, [Cmd|Acc]);
generate_interface_refac_cmd_1(APIName, [{s, {ParName, Type}, {ParName1, Type1}}|Others],
                               Index,Acc) ->
    case Type == Type1 of 
        true -> 
            Cmd=[{rename_var, APIName, ParName, Index, ParName1}],
            generate_interface_refac_cmd_1(APIName, Others, Index+1, [Cmd|Acc]);
        false ->
            case ParName==ParName1 of
                true ->
                    Cmd=[{change_type, APIName, ParName, Index, Type1}],
                    generate_interface_refac_cmd_1(APIName, Others, Index+1, [Cmd|Acc]);
                false ->
                    Cmd1 = {rm_argument, APIName, ParName,Index},
                    Cmd2 = {add_argument, APIName, ParName, Index},
                    generate_interface_refac_cmd_1(APIName, Others, Index+1, 
                                                   [[Cmd2, Cmd1]|Acc])
            end
    end.

get_param_fields(TypeName, _DataModel=#model{tps = Types}) ->
    DocType = lists:keyfind('_document', #type.nm,Types),
    DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),
    Type=case lists:keyfind(TypeName, #alt.tag, DocAlts) of 
             false -> TypeName;
             V -> V#alt.tp
         end,
    #type{nm=Type, els=Elems, atts=Attrs}=lists:keyfind(Type, #type.nm, Types),
    Attrs ++Elems.

get_element_name(#el{alts = Alternatives}) ->
    [get_element_name_1(Alt)||Alt<-Alternatives].
    
%%TODO: need to complete!!!
get_element_name_1(#alt{tag = Tag, rl = true, tp=_Type, mx=_Max}) ->
    Tag.

get_attr_name(#att{nm = Name}) ->
   Name.

make_args(Fs) ->
    "["++make_args_1(Fs)++"]".

make_args_1([])->
    "";
make_args_1([F]) ->
    get_field_name(F);
make_args_1([F|Fs]) ->
    get_field_name(F)++","++make_args_1(Fs).



make_generators(Fs) ->
    "["++make_generators_1(Fs)++"]".
make_generators_1([]) ->
    "";
make_generators_1([F]) -> 
    get_generator_name(F)++"()";
make_generators_1([F|Fs]) ->
    get_generator_name(F)++"(),"++make_generators_1(Fs).

get_generator_name(F) ->    
    FName=case is_record(F, att) of 
              true->
                  camelCase_to_camel_case(
                    atom_to_list(get_attr_name(F)));
              _ ->
                  camelCase_to_camel_case(
                    atom_to_list(hd(get_element_name(F)))) %% assumption here: this is only one alternative.
          end,
    "gen_"++FName.

append_strs([])->
    "";
append_strs([S]) ->
    S++"\n";
append_strs([S|Ss]) when S==""->
    append_strs(Ss);
append_strs([S|Ss]) ->
    S++",\n"++append_strs(Ss).


get_field_name(F) ->    
    case is_record(F, att) of 
        true->
            to_upper(atom_to_list(get_attr_name(F)));
        _ ->
            to_upper(atom_to_list(hd(get_element_name(F)))) %% assumption here: this is only one alternative.
    end.

to_upper([H|T]) -> [string:to_upper(H)|T].
   
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
    
