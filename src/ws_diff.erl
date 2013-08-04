%%% ====================================================================
%%%                    This is only a prototype!
%% ====================================================================
-module(ws_diff).

-export([ws_diff/2]).

%%-export([test/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").


%% test() ->
%%     ws_diff({"../tests/bookstore_sample/vodkatv_v0.xsd", 
%%              "../tests/bookstore_sample/vodkatv_v0.wsdl"},
%%             {"../tests/bookstore_sample/vodkatv.xsd", 
%%              "../tests/bookstore_sample/vodkatv_expanded.wsdl"}).


ws_diff({XsdFile1, WsdlFile1}, {XsdFile2, WsdlFile2}) ->
    {ok, _OldTypes, OldAPIs}=analyze_model(XsdFile1, WsdlFile1),
    {ok, _NewTypes, NewAPIs}=analyze_model(XsdFile2, WsdlFile2),
    APIChanges =levenshtein_dist(OldAPIs, NewAPIs),
    analyze_api_changes(APIChanges).
      

analyze_model(XsdFile, WsdlFile) ->
    {ok, Model} = erlsom:compile_xsd_file("../priv/wsdl20.xsd"),
    Model1 = erlsom:add_xsd_model(Model),
    Result=erlsom:parse_file(WsdlFile, Model1),
    case Result of
        {ok, Res} ->
            {ok, DataModel} = erlsom:compile_xsd_file(XsdFile),
            #model{tps=Types0} = DataModel,
            Types =[T||T<-Types0, T#type.nm/='_document'],
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
                      {{d, E1},{i, E2}, {0,0, _I,_O}}<-DistMatrix],
    Renames = [{{d, E1}, {i, E2}}||
                  {{d, E1},{i, E2}, {1,0, 0,0}}<-DistMatrix],
    
    FakeInserts = element(2, lists:unzip(ParaChanges)) ++
        element(2, lists:unzip(Renames)),
    Changes1 = Changes -- FakeInserts,
    analyze_api_changes_2(Changes1, ParaChanges++Renames, []).

analyze_api_changes_2([], _InterfaceChanges, Acc) ->
    lists:reverse(Acc);
analyze_api_changes_2([{'*', E}|Others], InterfaceChanges, Acc) ->
   analyze_api_changes_2(Others, InterfaceChanges, [{unchange, E}|Acc]);
analyze_api_changes_2([{d, E}|Others], InterfaceChanges, Acc)->
    case lists:keyfind({d,E}, 1, InterfaceChanges) of 
        false ->
            analyze_api_changes_2(Others, InterfaceChanges, [{delete, E}|Acc]);
        {{d, E}, {i, E1}} ->
            analyze_api_changes_2(Others, InterfaceChanges, [{substitute, E, E1}|Acc])
    end;
analyze_api_changes_2([{i,E}|Others], InterfaceChanges, Acc) ->
    analyze_api_changes_2(Others,  InterfaceChanges, [{insert, E}|Acc]).
        
                                  

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
    {MethodDist, NameDist, InputDist, OutputDist}.
  
    
get_element_names(Elems) ->
    lists:append([get_element_name(E)||E<-Elems]).

get_element_name(#el{alts = Alternatives}) ->
    [get_element_name_1(Alt)||Alt<-Alternatives].
    
%%TODO: to be completed!!!
get_element_name_1(#alt{tag = Tag, rl = true, tp=Type, mx=_Max}) ->
    {Tag, Type}.


get_attr_names(Attrs) ->
    [get_attr_name(A)||A<-Attrs].
 
get_attr_name(#att{nm = Name, tp=Type}) ->
   {Name, Type}.
 
process_interface_and_binding(Interface, Binding, Model) ->
    APIInterface= process_interface(Interface),
    APIBinding = process_binding(Binding),
    [process_interface_and_binding_1(APIName, ParamType, 
                                       ResponseType, APIBinding, Model)
     ||{APIName, ParamType, ResponseType}<-APIInterface].

process_interface_and_binding_1(APIName, ParamType, ResponseType, APIBinding, Model) ->
    {APIName, Method, _URI} = lists:keyfind(APIName, 1, APIBinding),
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
    #type{nm=Type, els=Elems, atts=Attrs}=lists:keyfind(Type, #type.nm, Types),
    Names1 = get_element_names(Elems),
    Names2 = get_attr_names(Attrs),
    Names1++Names2.
  

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

%% calc_levenshtein_dist(OldTypes, NewTypes) ->
%%     {Matrix, OldLen, NewLen} = calc_matrix(OldTypes, NewTypes),
%%     {value, {_, Val}} = lists:keysearch({OldLen, NewLen}, 1, Matrix),
%%     Val.
    
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
    Cost = case same(lists:nth(I, OldParams), lists:nth(J, NewTypes)) of
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
    case same(Ith, Jth) of
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
