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
-module(ws_diff).

-export([ws_diff/2]).

-export([test/0]).

-include("../include/erlsom_parse.hrl").
-include("../include/erlsom.hrl").
-include("../include/wsdl20.hrl").

-compile(export_all).

gen_diff() ->
    gen_diff("./2013-05-05", "./2014-02-17").

gen_diff(Dir1, Dir2) ->
    ws_diff({Dir1++"/vodkatv.wsdl", Dir1++"/vodkatv.xsd"},
            {Dir2++"/vodkatv.wsdl", Dir2++"/vodkatv.xsd"}).

%%@private
test() ->
    ws_diff({"../tests/vodkatv_sample/vodkatv_v0.wsdl",
             "../tests/vodkatv_sample/vodkatv_v0.xsd"},
            {"../tests/vodkatv_sample/vodkatv_expanded.wsdl", 
             "../tests/vodkatv_sample/vodkatv.xsd"}).

%%@doc This funtions tries to infer the API changes between two versions
%%     of the web service specification. It takes the WSDL and XSD specification
%%     of both versions, and reports what has been changed from the first 
%%     version to the second version.So far, the changes this tool is able to 
%%     report include: the adding/removing of WS APIs, renaming of APIs, 
%%     adding/removing of API parameters, renaming of API parameters, as 
%%     well as parameter type changes. The accuracy of this tool is still 
%%     to be evaluated!
-spec ws_diff({OldWsdl::file:filename(), Oldxsd::file:filename()},
              {NewWsdl::file:filename(), NewXsd::file:filename()}) ->
                     {ok, [term()]}|{error, term()}.
ws_diff({OldWsdl, OldXsd}, {NewWsdl, NewXsd}) ->
    {ok, OldTypes, OldAPIs}=analyze_model(OldXsd, OldWsdl),
    {ok, NewTypes, NewAPIs}=analyze_model(NewXsd, NewWsdl),
    TypeChanges = levenshtein_dist(OldTypes, NewTypes),
    APIChanges =levenshtein_dist(OldAPIs, NewAPIs),
    APIChanges1=analyze_api_changes(APIChanges),
    TypeChanges1 = analyze_type_changes(TypeChanges),
    {ok, APIChanges1, TypeChanges1}.


analyze_model(XsdFile, WsdlFile) ->
    LibDir = code:lib_dir('WSToolkit'),
    XsdDir =LibDir++"/priv/wsdl20.xsd",
    {ok, Model} = erlsom:compile_xsd_file(XsdDir),
    Model1 = erlsom:add_xsd_model(Model),
    Result=erlsom:parse_file(WsdlFile, Model1),
    case Result of
        {ok, Res} ->
            {ok, DataModel} = gen_xsd_model:gen_xsd_model(XsdFile),
            #model{tps=Types0} = DataModel,
            Types =[{T#type.nm, T#type.els, T#type.atts}
                    ||T<-Types0, T#type.nm/='_document'],
            Choice = Res#'DescriptionType'.choice, 
            Interface=lists:keyfind('InterfaceType', 1, Choice),
            Binding = lists:keyfind('BindingType', 1, Choice),
            APIs=process_interface_and_binding(Interface, Binding, DataModel),
            {ok, Types, APIs};
        {error, Error} -> 
            throw({error, Error})
    end.

analyze_api_changes(Changes) ->
    Res=analyze_api_changes_1(Changes, []),
    analyze_api_changes_2(Res).
analyze_api_changes_1([], Acc)->
    lists:reverse(Acc);
analyze_api_changes_1([{'*',E}|Others], Acc) ->
    analyze_api_changes_1(Others, [{'*',E}|Acc]);
analyze_api_changes_1([{i, E}|Others], Acc) ->
    case lists:member({d, E}, Others) of 
        true ->
            Others1=lists:keyreplace(E, 2, Others, {'*', E}),
            analyze_api_changes_1(Others1, Acc);
        false -> 
            analyze_api_changes_1(Others, [{i, E}|Acc])
    end;
analyze_api_changes_1([{d, E}|Others], Acc) ->
    case lists:member({i, E}, Others) of 
        true ->
            Others1=lists:keyreplace(E, 2, Others, {'*', E}),
            analyze_api_changes_1(Others1, Acc);
        false -> 
            analyze_api_changes_1(Others, [{d, E}|Acc])
    end;
analyze_api_changes_1([{s, E1, E2}|Others], Acc) ->
    analyze_api_changes_1([{d, E1}, {i, E2}|Others], Acc).
        

analyze_api_changes_2(Changes) ->
    Deletes = [{d, E}||{d, E}<-Changes],
    Inserts = [{i, E}||{i, E}<-Changes],
    DistMatrix=[{{d,E1}, {i, E2}, calc_api_dist(E1, E2)}
            ||{d, E1}<-Deletes, {i, E2}<-Inserts],
    ParaChanges = [{{d, E1},{i, E2}}||
                      {{d, E1},{i, E2}, 
                       {0,0, _I,_O}}<-DistMatrix],
    Renames = [{{d, E1}, {i, E2}}||
                  {{d, E1},{i, E2}, {1,0,0,0}}<-DistMatrix],
    FakeInserts = element(2, lists:unzip(ParaChanges)) ++
        element(2, lists:unzip(Renames)),
    Changes1 = Changes -- FakeInserts,
    analyze_api_changes_2(Changes1, {ParaChanges, Renames}, []).
 
analyze_api_changes_2([], _InterfaceChanges, Acc) ->
    lists:reverse(Acc);
analyze_api_changes_2([{'*', _E}|Others], InterfaceChanges, Acc) ->
   analyze_api_changes_2(Others, InterfaceChanges, Acc);
analyze_api_changes_2([{d, E}|Others], InterfaceChanges={ParaChanges, Renames}, Acc)->
    case lists:keyfind({d, E}, 1, ParaChanges) of 
        {{d, E={N, I, O, _}}, {i, E1={N1, I1, O1, _}}} ->
            Res = analyze_input_output_change({N, I, O}, {N1, I1, O1}),
            analyze_api_changes_2(Others, InterfaceChanges, [{changed, E, E1, Res}|Acc]);
        false ->
            case lists:keyfind({d,E}, 1, Renames) of 
                {{d, E}, {i, E1}} ->
                    analyze_api_changes_2(
                      Others, InterfaceChanges, 
                      [{api_renamed, E, E1}|Acc]);
                false ->
                    analyze_api_changes_2(Others, InterfaceChanges, [{api_deleted, E}|Acc])
            end
    end;
analyze_api_changes_2([{i,E}|Others], InterfaceChanges, Acc) ->
    analyze_api_changes_2(Others,  InterfaceChanges, [{api_added, E}|Acc]).
       
    
analyze_input_output_change({_APIName1, Input1, Output1}, 
                            {_APIName2, Input2, Output2}) ->
    InputChanges =levenshtein_dist(Input1, Input2),
    InputChanges1 = analyze_input_output_changes(InputChanges, Input1,  in),
    OutputChanges =levenshtein_dist(Output1, Output2),
    OutputChanges1 = analyze_input_output_changes(OutputChanges, Output1, out),
    {InputChanges1,  OutputChanges1}.
                          

analyze_input_output_changes(Changes, Original, Type) ->
    Changes1=analyze_input_output_changes_0(Changes, []),
    Changes2=analyze_input_output_changes_1(Changes1,Original,[]),
    analyze_input_output_changes_2(Changes2, Type).
  

analyze_input_output_changes_0([], Acc) ->
    lists:reverse(Acc);
analyze_input_output_changes_0([{s, E1, E2}|Others], Acc) ->
    analyze_input_output_changes_0([{d, E1}, {i, E2}|Others], Acc);
analyze_input_output_changes_0([E|Others], Acc) ->
    analyze_input_output_changes_0(Others, [E|Acc]).

analyze_input_output_changes_1([], _Original, Acc) ->
    lists:reverse(Acc);
analyze_input_output_changes_1([{'*',E}|Others], Original, Acc) ->
    analyze_input_output_changes_1(Others, Original,[{'*',E}|Acc]);
analyze_input_output_changes_1([{i, E}|Others], Original, Acc) ->
    case lists:member({d, E}, Others) of 
        true ->
            Others1 = Others--[{d,E}],
            Index = length(lists:takewhile(fun(Elem)-> Elem/=E end, Original))+1,
            analyze_input_output_changes_1(Others1, Original, [{m, E, Index}|Acc]);
        false -> 
            analyze_input_output_changes_1(Others, Original, [{i, E}|Acc])
    end;
analyze_input_output_changes_1([{d, E}|Others], Original, Acc) ->
    case lists:member({i, E}, Others) of 
        true ->
            Index = length(lists:takewhile(fun(Elem)-> Elem/=E end, Original))+1,
            Others1=lists:keyreplace(E, 2, Others, {m, E, Index}),
            analyze_input_output_changes_1(Others1, Original, Acc);
        false -> 
            analyze_input_output_changes_1(Others, Original, [{d, E}|Acc])
    end;
analyze_input_output_changes_1([{m, E, Index}|Others], Original, Acc) ->
    analyze_input_output_changes_1(Others, Original, [{m, E, Index}|Acc]);
analyze_input_output_changes_1([{s, E1, E2}|Others], Original, Acc) ->
    analyze_input_output_changes_1([{d, E1}, {i, E2}|Others], Original, Acc).
        

analyze_input_output_changes_2(Changes, Type) ->
    Deletes = [{d, E}||{d, E}<-Changes],
    Inserts = [{i, E}||{i, E}<-Changes],
    DistMatrix=[{{d,E1}, {i, E2}, calc_api_dist(E1, E2)}
            ||{d, E1}<-Deletes, {i, E2}<-Inserts],
    ParaChanges = [{{d, E1},{i, E2}}||
                      {{d, E1},{i, E2}, {0,1}}<-DistMatrix],
    Renames = [{{d, E1}, {i, E2}}||
                  {{d, E1},{i, E2}, {1, 0}}<-DistMatrix],
    FakeInserts = element(2, lists:unzip(ParaChanges)) ++
        element(2, lists:unzip(Renames)),
    Changes1 = Changes -- FakeInserts,
    analyze_input_output_changes_2(Changes1, {ParaChanges, Renames}, Type, []).
 
analyze_input_output_changes_2([], _InterfaceChanges, _Type, Acc) ->
    lists:reverse(Acc);
analyze_input_output_changes_2([{'*', E}|Others], InterfaceChanges, Type, Acc) ->
    analyze_input_output_changes_2(Others, InterfaceChanges, Type, [{unchanged, E}|Acc]);
analyze_input_output_changes_2([{m, E, Index}|Others], InterfaceChanges, Type, Acc) ->
    analyze_input_output_changes_2(Others, InterfaceChanges, Type, [{moved, E, Index}|Acc]);
analyze_input_output_changes_2([{d, E}|Others], InterfaceChanges={ParaChanges, Renames}, Type, Acc) ->
    Changes = ParaChanges++Renames,
    case lists:keyfind({d,E}, 1, Changes) of 
        false when Type==in->
            analyze_input_output_changes_2(Others, InterfaceChanges, Type, [{deleted, E}|Acc]);
        false when Type==out->
            analyze_input_output_changes_2(Others, InterfaceChanges, Type,[{deleted, E}|Acc]);
        {{d, E}, {i, E1}} when Type==in->
            case lists:member({{d, E}, {i, E1}}, ParaChanges) of 
                true ->
                    analyze_input_output_changes_2(
                      Others, InterfaceChanges, Type,[{type_changed, E, E1}|Acc]);
                false ->
                    analyze_input_output_changes_2(
                      Others, InterfaceChanges, Type,[{renamed, E, E1}|Acc])
            end;
        {{d, E}, {i, E1}} when Type==out->
            case lists:member({{d, E}, {i, E1}}, ParaChanges) of 
                true ->
                    analyze_input_output_changes_2(
                      Others, InterfaceChanges, Type,[{api_output_field_type_changed, E, E1}|Acc]);
                false ->
                    analyze_input_output_changes_2(
                      Others, InterfaceChanges, Type,[{api_output_field_renamed, E, E1}|Acc])
            end
    end;
analyze_input_output_changes_2([{i,E}|Others], InterfaceChanges, in, Acc)->
    analyze_input_output_changes_2(Others, InterfaceChanges, in, [{added, E}|Acc]);
analyze_input_output_changes_2([{i,E}|Others], InterfaceChanges, out, Acc)->
    analyze_input_output_changes_2(Others, InterfaceChanges, out, [{added, E}|Acc]).
        

calc_api_dist({Name1, Input1, Output1, Method1}, {Name2, Input2, Output2, Method2}) ->
    NameDist=case Name1==Name2 of 
                 true -> 0;
                 _ -> 1
             end,
    MethodDist = case Method1==Method2 of 
                     true -> 0;
                     _ -> 1
                 end,
    {Input11, _} = lists:unzip(Input1),
    {Output11, _} = lists:unzip(Output1),
    {Input21, _} = lists:unzip(Input2),
    {Output21, _} = lists:unzip(Output2),
    InputDist =  length(Input11--Input21) + 
        length(Input21--Input11),
    OutputDist = length(Output11--Output21) + 
        length(Output21--Output11),
    {NameDist, MethodDist, InputDist, OutputDist};
calc_api_dist({Name1, Type1}, {Name2, Type2}) ->
    NameDist=case Name1==Name2 of 
                 true -> 0;
                 _ -> 1
             end,
    TypeDist = case Type1==Type2 of 
               true -> 0;
               _ -> 1
           end,
    {NameDist, TypeDist};
calc_api_dist(E1=#el{}, E2=#el{}) ->
    case  E1==E2 of   %% need to be refined!!!
        true ->
            {0, 0};
        false ->
            {1, 1}
    end.

  
    

analyze_type_changes(Changes) ->
    Res=analyze_type_changes_1(Changes, []),
    analyze_type_changes_2(Res).

analyze_type_changes_1([], Acc)->
    lists:reverse(Acc);
analyze_type_changes_1([{'*',E}|Others], Acc) ->
    analyze_type_changes_1(Others, [{'*',E}|Acc]);
analyze_type_changes_1([{i, E}|Others], Acc) ->
    case lists:member({d, E}, Others) of 
        true ->
            Others1=lists:keyreplace(E, 2, Others, {'*', E}),
            analyze_type_changes_1(Others1, Acc);
        false -> 
            analyze_type_changes_1(Others, [{i, E}|Acc])
    end;
analyze_type_changes_1([{d, E}|Others], Acc) ->
    case lists:member({i, E}, Others) of 
        true ->
            Others1=lists:keyreplace(E, 2, Others, {'*', E}),
            analyze_type_changes_1(Others1, Acc);
        false -> 
            analyze_type_changes_1(Others, [{d, E}|Acc])
    end;
analyze_type_changes_1([{s, E1, E2}|Others], Acc) ->
    analyze_type_changes_1([{d, E1}, {i, E2}|Others], Acc).

    
analyze_type_changes_2(Changes) ->
    Deletes = [{d, E}||{d, E}<-Changes],
    Inserts = [{i, E}||{i, E}<-Changes],
    DistMatrix=[{{d,E1}, {i, E2}, calc_type_dist(E1, E2)}
            ||{d, E1}<-Deletes, {i, E2}<-Inserts],
    ElemChanges = [{{d, E1},{i, E2}}||
                      {{d, E1},{i, E2}, 
                       {0, C, 0}}<-DistMatrix, C/=0],
    AttrChanges = [{{d, E1},{i, E2}}||
                      {{d, E1},{i, E2}, 
                       {0, 0, C}}<-DistMatrix, C/0],
    Renames = [{{d, E1}, {i, E2}}||
                  {{d, E1},{i, E2}, {1,0,0}}<-DistMatrix],
    FakeInserts = element(2, lists:unzip(ElemChanges)) ++
        element(2, lists:unzip(Renames)) ++ 
        element(2, lists:unzip(AttrChanges)),
    Changes1 = Changes -- FakeInserts,
    analyze_type_changes_2(Changes1, {ElemChanges++AttrChanges, Renames}, []).
 
analyze_type_changes_2([], _InterfaceChanges, Acc) ->
    lists:reverse(Acc);
analyze_type_changes_2([{'*', _E}|Others], InterfaceChanges, Acc) ->
   analyze_type_changes_2(Others, InterfaceChanges, Acc);
analyze_type_changes_2([{d, E}|Others], InterfaceChanges={FieldChanges, Renames}, Acc) ->
    case lists:keyfind({d, E}, 1, FieldChanges) of
        {{d, E}, {i, E1}} ->
            Res = analyze_input_output_change(E, E1),
            analyze_type_changes_2(Others, InterfaceChanges, [{field_changed, E, E1, Res}|Acc]);
        false ->
            case lists:keyfind({d,E}, 1, Renames) of 
                {{d, E}, {i, E1}} ->
                    analyze_type_changes_2(
                      Others, InterfaceChanges, 
                      [{type_renamed, E, E1}|Acc]);
                false ->
                    analyze_type_changes_2(Others, InterfaceChanges, [{type_deleted, E}|Acc])
            end
    end;
analyze_type_changes_2([{i,E}|Others], InterfaceChanges, Acc) ->
    analyze_type_changes_2(Others,  InterfaceChanges, [{type_added, E}|Acc]).



calc_type_dist({Name1, Elems1, Attrs1}, {Name2, Elems2, Attrs2}) ->
    NameDist=case Name1==Name2 of 
                 true -> 0;
                 _ -> 1
             end,
    ElemDist =  length(Elems1--Elems2) + 
        length(Elems2--Elems1),
    AttrDist = length(Attrs1--Attrs2) + 
        length(Attrs2--Attrs1),
    {NameDist, ElemDist, AttrDist}.

  
get_element_names(Elems, AllTypes) ->
    lists:append([get_element_name(E, AllTypes)||E<-Elems]).

get_element_name(#el{alts = Alternatives, mn=Min, mx=Max}, AllTypes) ->
    [get_element_name_1(Alt, {Min, Max}, AllTypes)||Alt<-Alternatives].
    
%%TODO: to be completed!!!
get_element_name_1(A=#alt{tag = Tag, rl = true, tp=_Type, mx=_Max}, 
                   {ElemMin, ElemMax}, AllTypes) ->
    {Str,_}=write_hrl:write_alt_type(A, {ElemMin, ElemMax}, AllTypes),
    TypeStr=lists:last(string:tokens(Str, [$:])),
    {Tag, TypeStr}.


get_attr_names(Attrs, AllTypes) ->
    [get_attr_name(A, AllTypes)||A<-Attrs].
 
get_attr_name(A=#att{nm = Name, tp=_Type}, AllTypes) ->
    {Str, _}=write_hrl:writeAttribute(A, AllTypes),
    TypeStr=lists:last(string:tokens(Str, [$:])),
    {Name,TypeStr}.

process_interface_and_binding(Interface, Binding, Model) ->
    APIInterface= process_interface(Interface),
    APIBinding = process_binding(Binding),
    [process_interface_and_binding_1(APIName, ParamType, 
                                       ResponseType, APIBinding, Model)
     ||{APIName, ParamType, ResponseType}<-APIInterface].

process_interface_and_binding_1(APIName, ParamType, ResponseType, APIBinding, Model) ->
    Method = case lists:keyfind(APIName, 1, APIBinding) of 
                 {APIName, Method1, _URI} ->
                     Method1;
                 _ -> 
                    %% io:format("API does not have a method:~p\n", [APIName]),
                     none
             end,
    ParaTypeName= case ParamType of none -> none; 
                      _ -> list_to_atom(rm_prefix(ParamType))
                  end,
    ResponseTypeName=list_to_atom(rm_prefix(ResponseType)),
    Params = get_param_field_names(ParaTypeName, Model),
    Response = get_param_field_names(ResponseTypeName, Model),
    {APIName,Params,Response, Method}.
  
process_binding(_Binding=#'BindingType'{choice=Choice}) ->
    [process_binding_option(C)||
        C<-Choice, element(1,C)=='BindingOperationType'].

process_binding_option(C) ->
    Attrs=C#'BindingOperationType'.anyAttribs,
    [Method]=[M||{{"method", _URL}, M}<-Attrs],
    {qname, _, OpName, _, _}=C#'BindingOperationType'.ref,
    case [L||{{"location", _URL}, L}<-Attrs] of 
        [Location] ->
            {OpName, Method, Location};
        [] ->
            {OpName, Method, ""}
    end.

get_param_field_names(none, _) ->
    [];
get_param_field_names(TypeName, _DataModel=#model{tps =Types}) ->
    DocType = lists:keyfind('_document', #type.nm,Types),
    DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),
    Type=case lists:keyfind(TypeName, #alt.tag, DocAlts) of 
             false -> TypeName;
             V -> V#alt.tp
         end,
    AllTypes=[T#type.nm||T<-Types, 
                         not is_list(T#type.anyAttr) 
                             orelse lists:keyfind(is_simple_type,1,T#type.anyAttr)==false],
    case lists:keyfind(Type, #type.nm, Types) of 
        #type{nm=Type, els=Elems, atts=Attrs} ->
            Names1 = get_element_names(Elems, AllTypes),
            Names2 = get_attr_names(Attrs, AllTypes),
            Names1++Names2;
        false ->
            []
    end.
  
rm_prefix(Type) ->
    lists:last(string:tokens(Type, [$:])).
    
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

levenshtein_dist(OldTypes, NewTypes) ->
    {Matrix, OldLen, NewLen} = calc_matrix(OldTypes, NewTypes),
    get_edit_ops(Matrix, OldTypes, NewTypes, OldLen, NewLen, []). 
    
calc_matrix(OldTypes, NewTypes) ->
    OldLen = length(OldTypes),
    NewLen = length(NewTypes),
    Is = lists:seq(0, OldLen),
    Js = lists:seq(0, NewLen),
    InitAcc = [{{I, 0}, I} || I <- Is] 
        ++ [{{0, J}, J} || J <- Js],
    Matrix = levenshtein_dist(OldTypes, NewTypes, OldLen,
                              NewLen, {1, 1}, InitAcc),
    {Matrix, OldLen, NewLen}.



levenshtein_dist(_OldParams, _NewTypes, _OldLen, NewLen, {_I, J}, Acc)
  when J>NewLen ->  Acc;
levenshtein_dist(OldParams, NewTypes, OldLen, NewLen, {I, J}, Acc)
  when I>OldLen ->
    levenshtein_dist(OldParams, NewTypes, OldLen, NewLen,  {1, J+1}, Acc);
levenshtein_dist(OldParams, NewTypes, OldLen, NewLen, {I, J}, Acc) ->
    Cost = case lists:nth(I, OldParams)==lists:nth(J, NewTypes) of
               true ->
                   0;
               false ->
                   1
           end,
    {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Acc),
    {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Acc),
    {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Acc),
    Min = lists:min([Del+1, Ins+1, Sub+Cost]),
    levenshtein_dist(OldParams, NewTypes, OldLen, NewLen, {I+1, J}, 
                                               [{{I,J},Min}|Acc]).

get_edit_ops(_Matrix, _OldParams, _NewTypes, I, J,Acc) 
  when I=<0 andalso J=<0 ->
    Acc;
get_edit_ops(Matrix, OldParams, NewTypes, I, J,Acc) 
  when I=<0 ->
    Jth = lists:nth(J, NewTypes),
    get_edit_ops(Matrix, OldParams, NewTypes, I, J-1, [{'i', Jth}|Acc]);
get_edit_ops(Matrix, OldParams, NewTypes, I, J, Acc) 
  when  J=<0 ->
    Ith = lists:nth(I, OldParams),
    get_edit_ops(Matrix, OldParams, NewTypes, I-1, J, [{'d', Ith}|Acc]);
get_edit_ops(Matrix, OldParams, NewTypes, I, J, Acc) ->
    Ith = lists:nth(I, OldParams),
    Jth = lists:nth(J, NewTypes),
    case Ith==Jth of
        true ->
            get_edit_ops(Matrix, OldParams, NewTypes, I-1, J-1, [{'*', Ith}|Acc]);
        false ->
            {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Matrix),
            {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Matrix),
            {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Matrix),
                   case lists:min([Del, Ins, Sub]) of
                       Ins ->
                           get_edit_ops(Matrix, OldParams, NewTypes, I, J-1,[{'i', Jth}|Acc]);
                       Del ->
                           get_edit_ops(Matrix, OldParams, NewTypes, I-1, J,[{'d', Ith}|Acc]);
                       Sub ->
                           get_edit_ops(Matrix, OldParams, NewTypes, I-1, J-1,[{'s', Ith, Jth}|Acc])
                   end
    end.

same(E1, E2) -> E1==E2.
