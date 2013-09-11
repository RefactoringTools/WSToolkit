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
%%%                   Refactoring script generation.
%%%
%%%                    This is only a prototype!
%% ====================================================================
-module(gen_refac_script).

-export([gen_refac_script/3]).

-export([test/0, test1/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").

%%@private
test() ->
    gen_refac_script(
           {"../tests/bookstore_sample/vodkatv_v0.wsdl",
            "../tests/bookstore_sample/vodkatv_v0.xsd"},
           {"../tests/bookstore_sample/vodkatv_expanded.wsdl", 
            "../tests/bookstore_sample/vodkatv.xsd"},
           "refac_script_test.erl").
%@private
test1() ->
    gen_refac_script({"../tests/bookstore_sample/vodkatv_expanded.wsdl", 
                      "../tests/bookstore_sample/vodkatv.xsd"},
                     {"../tests/bookstore_sample/vodkatv_v0.wsdl",
                      "../tests/bookstore_sample/vodkatv_v0.xsd"},
                     "refac_script_test1.erl").
   
%%@doc Infer the API changes between two versions of the web service 
%%     specification, and generate a refactoring script that can be 
%%     applied to the existing `eqc_statem' test model. The current 
%%     implementation ignores type changes.
-spec gen_refac_script({OldWsdl::file:filename(), Oldxsd::file:filename()},
                       {NewWsdl::file:filename(), NewXsd::file:filename()},
                       OutFile::file:filename()) ->
                              {ok, [term()]}|{error, term()}.
gen_refac_script({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}, OutFile) ->         
    {ok, APIChanges} = ws_diff:ws_diff({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}),
    RefacCmds=gen_refac_cmds(APIChanges, [], []),
    Content=gen_composite_refac(none, RefacCmds, OutFile),
    file:write_file(OutFile, list_to_binary(Content)).

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
    Str=lists:flatten(io_lib:format("                 ?refac_(refac_rm_op, [File,\"~s\", [File]])", [OpName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{rm_argument, OpName, Index, _ArgName}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(refac_rm_op_arg, [File,~p,~p, [File]])", 
                          [OpName, Index])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{rename_operation, OldOpName, Arity, NewOpName}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(refac_rename_fun, [File,{~p,~p}, ~p, [File]])", 
                          [OldOpName, Arity, NewOpName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{rename_argument, OpName, Arity, Index, NewParName}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(refac_rename_op_arg, [File,~p,~p,~p, ~p, [File]])", 
                          [OpName, Arity, Index, NewParName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{add_operation, OpName, FieldNames}|Refacs], Acc) ->
    Str=io_lib:format("                 ?refac_(refac_add_op, [File, ~p,~p, [File]])",
                      [OpName, FieldNames]),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{add_argument, OpName, Index, ArgName, _ArgType}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(refac_add_op_arg, [File,~p,~p,~p, [File]])", 
                          [OpName, Index, ArgName])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{swap_op_argument, OpName, NewOrder}|Refacs], Acc) ->
    Str = lists:flatten(io_lib:format(
                          "                 ?refac_(refac_swap_op_arg, [File,~p,~p,[File]])", 
                          [OpName, NewOrder])),
    gen_composite_refacs(TestFile, Refacs, [Str|Acc]);
gen_composite_refacs(TestFile, [{type_change, _OpName, _ParName,_ParIndex, _ParType}|Refacs], Acc) ->
    gen_composite_refacs(TestFile, Refacs, Acc).

%% gen_test_model_refacs({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}) ->         
%%     {ok, APIChanges} = ws_diff:ws_diff({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}),
%%     {ok, DataModel} = erlsom:compile_xsd_file(XsdFile1),
%%     gen_refac_cmds(APIChanges, DataModel, []).

gen_refac_cmds([], _DataModel, Refacs) ->
    lists:reverse(lists:flatten(Refacs));
gen_refac_cmds([{'*', _}|Others], DataModel, Refacs) ->
    gen_refac_cmds(Others, DataModel, Refacs);
gen_refac_cmds([{api_deleted, {APIName, _, _, _}}|Others], DataModel, Refacs) ->
    APIName1 = camelCase_to_camel_case(APIName),
    gen_refac_cmds(Others, DataModel, [[{rm_operation, [APIName1]}]|Refacs]);
gen_refac_cmds([{api_added, {APIName, Input, Output, _Method}}|Others],
               DataModel, Refacs) ->
    NewRefac=generate_add_op_refac_cmd(APIName, Input, Output, DataModel),
    gen_refac_cmds(Others, DataModel, [NewRefac|Refacs]);
gen_refac_cmds([{api_renamed, {APIName, InType, _OutType, _Method},
                              {NewAPIName, _, _, _}}|Others], DataModel, Refacs) ->
    NewRefac=generate_rename_op_refac_cmd(APIName, length(InType), NewAPIName),
    gen_refac_cmds(Others, DataModel, [NewRefac|Refacs]);
gen_refac_cmds([{api_parameter_changed, _OldInterface={APIName, Input, _, _},
                 _NewInterface, {ParaChanges, _OutPutChanges}}
                             |Others], DataModel, Refacs) ->
    NewRefacs=generate_interface_refac_cmds(APIName,ParaChanges, length(Input)),
    gen_refac_cmds(Others, DataModel, NewRefacs ++ Refacs).

    
generate_add_op_refac_cmd(APIName, InType, _OutType, _DataModel) ->
    APIName1 = camelCase_to_camel_case(APIName),
    FieldNames=
        case lists:member(InType, ["#none", '#none', 'none', "none"]) of 
            true ->
                [];
            false ->
                [to_upper(atom_to_list(ParName))||{ParName, _Type}<-InType]                 
        end,
    [{add_operation, APIName1, FieldNames}].

generate_rename_op_refac_cmd(OldAPIName,  Arity, NewAPIName)->           
    OldAPIName1 = camelCase_to_camel_case(OldAPIName),
    NewAPIName1 = camelCase_to_camel_case(NewAPIName),
    [{rename_operation, OldAPIName1, Arity, NewAPIName1}].

generate_interface_refac_cmds(APIName, ParaChanges, NumOfArgs) ->
    APIName1 = camelCase_to_camel_case(APIName),
    case lists:keyfind(moved, 1, ParaChanges) of 
        false ->
            generate_interface_refac_cmd_1(APIName1, ParaChanges, 1, NumOfArgs, []);
        _->
            SwapArgCmd = generte_swap_arg_cmd(APIName1, ParaChanges),
            Cmds = generate_interface_refac_cmd_1(APIName1, ParaChanges, 1, NumOfArgs, []),
            [SwapArgCmd|Cmds]
    end.
      
generte_swap_arg_cmd(APIName, ParaChanges)->
    ParaChanges=[C||C<-ParaChanges, element(1, C)/=api_parameter_added],
    Cs = lists:zip(ParaChanges, lists:seq(1, length(ParaChanges))),
    NewOrder = [case C of 
                    {{moved, _, Index},_} -> Index;
                    {_, Index} -> Index
                end||C<-Cs],
    {swap_op_argument, APIName, NewOrder}.
    
generate_interface_refac_cmd_1(_APIName, [], _Index, _NumOfArgs, Acc) ->
    Acc;
generate_interface_refac_cmd_1(APIName, [{'unchanged', {_, _}}|Others], Index, NumOfArgs, Acc) ->
    generate_interface_refac_cmd_1(APIName, Others, Index+1, NumOfArgs, Acc);
generate_interface_refac_cmd_1(APIName, [{'moved', _, _}|Others], Index, NumOfArgs, Acc) ->
    generate_interface_refac_cmd_1(APIName, Others, Index+1, NumOfArgs, Acc);
generate_interface_refac_cmd_1(APIName, [{api_parameter_deleted, {ParName, _Type}}|Others], Index, NumOfArgs, Acc) ->
    Cmd = [{rm_argument, APIName, NumOfArgs-Index+1, ParName}],
    generate_interface_refac_cmd_1(APIName, Others, Index+1, NumOfArgs, [Cmd|Acc]);
generate_interface_refac_cmd_1(APIName, [{api_parameter_added, {ParName, Type}}|Others], Index, NumOfArgs, Acc) ->
    Cmd = [{add_argument, APIName, NumOfArgs-Index+1, to_upper(atom_to_list(ParName)), Type}],
    generate_interface_refac_cmd_1(APIName, Others, Index, NumOfArgs, [Cmd|Acc]);
generate_interface_refac_cmd_1(APIName, [{api_parameter_renamed, {_ParName, _Type}, {NewParName, _}}|Others], 
                               Index, NumOfArgs, Acc) ->
    Cmd = [{rename_argument, APIName, NumOfArgs, Index,  
            to_upper(atom_to_list(NewParName))}],
    generate_interface_refac_cmd_1(APIName, Others, Index+1, NumOfArgs, [Cmd|Acc]);
generate_interface_refac_cmd_1(APIName, [{api_parameter_type_changed, {ParName, _Type}, {ParName, NewType}}|Others],
                               Index, NumOfArgs, Acc) ->
    Cmd=[{type_change, APIName, ParName, Index, NewType}],
    generate_interface_refac_cmd_1(APIName, Others, Index+1, NumOfArgs, [Cmd|Acc]).
  
append_strs([])->
    "";
append_strs([S]) ->
    S++"\n";
append_strs([S|Ss]) when S==""->
    append_strs(Ss);
append_strs([S|Ss]) ->
    S++",\n"++append_strs(Ss).

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
    
