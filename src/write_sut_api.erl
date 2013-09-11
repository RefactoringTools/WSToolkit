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
%%%                    This is only a prototype!
%% ====================================================================
-module(write_sut_api).

-export([write_sut_api/5]).

-export([test1/0,
         test2/0,
         test3/0]).

-include_lib("erlsom/include/erlsom_parse.hrl").
-include_lib("erlsom/include/erlsom.hrl").
-include("../include/wsdl20.hrl").

%%@private
test1() ->
    write_sut_api(
      none,
      "../tests/bookstore_sample/booklist_expanded.wsdl", 
      "../tests/bookstore_sample/booklist.xsd",
      "http://www.bookstore.com/books/",
      "booklist_sut.erl").

%%@private
test2() ->
    write_sut_api(
      none,
      "../tests/bookstore_sample/book-0321396855_expanded.wsdl", 
      "../tests/bookstore_sample/book.xsd",
      "http://www.bookstore.com/books/",
      "book_sut.erl").

%%@private
test3()->
    write_sut_api(
      none,
      "../tests/vodkatv_sample/vodkatv_v0.wsdl", 
      "../tests/vodkatv_sample/vodkatv_v0.xsd", 
      "http://localhost:8082/vodkatv/",
      "vodkatv_sut.erl").

%%@doc Generates the WS connector module. The WS connector module
%%     defines a collection of connector functions that are used by
%%     QuickCheck to invoke web service operations. There is a connector
%%     function defined for each web service operation.
%%     This function takes the the .hrl file containing type definitions,
%%     the WSDL specification, the XSD schema, and the base url for the 
%%     web service as input, and writes the connector module generated to 
%%     `OutFile'. 
%%     Note: Some utility functions are added to the module generated, but 
%%     in some cases not all the utility functions are used by the connector
%%     functions. This will be improved.
-type url()::string().
-spec write_sut_api(HrlFile::file:filename()|none,
                    WsdlFile::file:filename(),
                    XsdFile::file:filename(),
                    BaseURL::url(),
                    OutFile::file:filename())->
                           ok|{error, Error::term()}.
write_sut_api(HrlFile, WsdlFile, XsdFile, BaseURL, OutFile) ->
    {ok, Model} = erlsom:compile_xsd_file("../priv/wsdl20.xsd"),
    Model1 = erlsom:add_xsd_model(Model),
    Result=erlsom:parse_file(WsdlFile, Model1),
    case Result of
        {ok, Res} ->
            {ok, DataModel} = erlsom:compile_xsd_file(XsdFile), 
            Choice = Res#'DescriptionType'.choice, 
            write_sut_api_1(HrlFile, Choice, DataModel, XsdFile, BaseURL, OutFile);
        {error, Error} -> 
            throw({error, Error})
    end.

write_sut_api_1(HrlFile, Choice, DataModel, XsdFile, BaseURL, OutFile) ->
    Interface=lists:keyfind('InterfaceType', 1, Choice),
    Binding = lists:keyfind('BindingType', 1, Choice),
    APIInterface=process_interface(Interface),
    APIBinding=process_binding(Binding),
    write_sut_api_2(HrlFile, APIInterface, APIBinding, DataModel, XsdFile, BaseURL,OutFile).
    

write_sut_api_2(HrlFile, APIInterface, APIBindings, DataModel, XsdFile, BaseURL, OutFile) ->
    UtilFuns=util_funs(),
    Res=[gen_sut_funs_1(I, APIBindings, DataModel)
         ||I<-APIInterface],
    {SUTs, FAs}=lists:unzip(Res),
    Heading=create_heading(HrlFile, XsdFile, BaseURL, FAs, OutFile),
    Content=Heading++lists:flatten(SUTs)++UtilFuns,
    file:write_file(OutFile, list_to_binary(Content)).
   
                                                               
gen_sut_funs_1({APIName, Param, Response}, APIBindings, DataModel) ->
    {APIName, Method, URI} = lists:keyfind(APIName, 1, APIBindings),
    APIName1=camelCase_to_camel_case(APIName),
    ResponseRecordName=lists:last(string:tokens(Response, [$:])),
    case Method of 
        "POST" ->
            ParaRecordName = lists:last(string:tokens(Param, [$:])),
            FieldNames = get_param_field_names(list_to_atom(ParaRecordName), DataModel),
            Params = gen_param_string(FieldNames),
            Def=APIName1 ++ "(" ++ Params ++ ")->\n" ++
               "    PostData = generate_post_params(" ++ ParaRecordName
              ++ ", [" ++ Params ++ "]),\n"
                "    Url = ?BASE_URL++\"" ++ URI ++ "\",\n"
                "    http_request('" ++ Method ++ "', Url, PostData,\n"
                "                  fun(Data) -> \n"
                "                      process_response(" ++ ResponseRecordName ++ ", Data)\n"
                "                  end).\n\n",
            {Def, {list_to_atom(APIName1), length(FieldNames)}};
        "GET" ->
            case lists:member(Param, ["#none", '#none', 'none', "none"]) of 
                true ->
                    %% Spec = gen_type_spec(APIName1,  none, ResponseRecordName),
                    Def=APIName1++"()->\n" ++
                        "    Url = ?BASE_URL++\"" ++ URI ++"\",\n"
                        "    http_request('"++Method++"', Url,\n"
                        "                  fun(Data) -> \n"
                        "                       process_response("++ResponseRecordName++", Data)\n"
                        "                  end).\n\n",
                    {Def, {list_to_atom(APIName1),0}};
                _ ->
                    ParaRecordName = lists:last(string:tokens(Param, [$:])),
                    FieldNames = get_param_field_names(list_to_atom(ParaRecordName), DataModel),
                    Params = gen_param_string(FieldNames),
                    Def=APIName1 ++ "(" ++ Params ++ ")->\n" ++
                        "    GetParams = generate_get_params(" ++ ParaRecordName
                        ++ ", [" ++ Params ++ "]),\n"
                        "    Url = add_get_params(?BASE_URL++\"" ++ URI ++ "\",GetParams),\n"
                        "    http_request('" ++ Method ++ "', Url,\n"
                        "                  fun(Data) -> \n"
                        "                      process_response("++ResponseRecordName++", Data)\n"
                        "                  end).\n\n",
                    {Def, {list_to_atom(APIName1), length(FieldNames)}}
            end
    end.
    
 
create_heading(HrlFile, XsdFile, BaseURL, FAs, OutFile)->
    BaseName = filename:basename(OutFile, ".erl"),
    Exports = mk_exports(FAs),
    IncludeHrl = case HrlFile of
                     none -> "";
                     _ -> "-include(\""++HrlFile++"\").\n\n"
                 end,
    "-module("++BaseName++").\n\n" ++ IncludeHrl++
        "-include_lib(\"erlsom/include/erlsom_parse.hrl\").\n\n"
        "-define(BASE_URL, "++"\""++BaseURL++"\").\n"
        "-define(XSD_File, "++"\""++XsdFile++"\").\n\n"
        "-export(["++Exports++"]).\n\n".
        
mk_exports([]) ->
    "";
mk_exports([{F,A}]) ->
    format_fa({F,A});
mk_exports([H|T]) ->
   format_fa(H) ++ ",\n         "++mk_exports(T).

format_fa({F,A}) ->
    atom_to_list(F)++"/"++integer_to_list(A).

util_funs() ->
    "%%---------------------------------------------------------------\n"
    "%% Utilities (move to another module?)\n"
    "%%---------------------------------------------------------------\n"
    ++generate_post_params()++process_response()++http_request()++gen_params().

generate_post_params() ->
    "\n"
    "generate_post_params(ParamType, Values)->\n"
    "    Data=list_to_tuple([ParamType, []|Values]),\n"
    "    {ok, Model}=erlsom:compile_xsd_file(?XSD_File),\n"
    "    erlsom:write(Data, Model).\n\n".

process_response() ->
    "\n"
    "process_response(_Type, Data) -> \n"
    "    {ok, Model}=erlsom:compile_xsd_file(?XSD_File),\n"
    "    try erlsom:scan(Data, Model) of\n"
    "       {ok, Result, _} -> Result;\n"
    "       _    -> 'response_data_does_not_conform_to_model'\n"
    "    catch {_E1,_E2} -> 'respsone_data_does_not_conform_to_model'\n"
    "    end. \n\n".
                

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

gen_param_string([]) ->
    "";
gen_param_string([P]) ->
    to_upper(atom_to_list(P));
gen_param_string([H|T]) ->
    to_upper(atom_to_list(H))
        ++", "++gen_param_string(T).

to_upper([H|T]) -> [string:to_upper(H)|T].
    

   
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
    

http_request() ->
    "\n"
    "http_request(Method, Url, FunParse)->\n"
    "    http_request(Method, Url, \"\", FunParse).\n\n"
    "http_request(Method, Url, Body, FunParse)->\n"
    "    case do_http_request(Method, Url, Body) of\n"
    "   	{ok, {{_Protocol, 200, _Msg}, _Headers, Response}} ->\n"
    "		           {ok, FunParse(Response)};\n"
    "    	{ok, {{_Protocol, Code, Msg}, _Headers, _Response}} ->\n"
    "  	            {error, {Code, Msg}};\n"
    "    	{error, Reason} ->\n"
    "	            {error, Reason}\n"
    "    end.\n\n"
    "do_http_request('GET', Url, _Body)->\n"
    "    httpc:request(get, {Url, []}, [], []);\n\n"
    "do_http_request('POST', Url, Body)->\n"
    "    httpc:request(post, {Url, [], \"text/xml\", Body}, [], []).\n\n".

gen_params()->
    "\n"
    "generate_get_params(ParamType, Values) ->\n"
    "    {ok, DataModel}=erlsom:compile_xsd_file(?XSD_File),\n"
    "    FieldNames = get_param_field_names(ParamType, DataModel),\n"
    "    TagValuePairs=lists:zip(FieldNames, Values),\n"
    "    TagValuePairs1=lists:append([expand_list(P)||P<-TagValuePairs]),\n"
    "    generate_get_params(TagValuePairs1).\n\n"
    "expand_list({{Tag, Max}, Value})when Max=/=1 ->\n"
    "    [{atom_to_list(Tag), V}||V<-Value];\n"
    "expand_list({{Tag, _Max}, Value}) ->\n"
    "    [{atom_to_list(Tag), Value}].\n"
    "\n"
    "generate_get_params([]) ->\n"
    "    \"\";\n"
    "generate_get_params([{_Name, undefined}]) ->\n"
    "    \"\";\n"
    "generate_get_params([{_Name, \"\"}]) ->\n"
    "    \"\";\n"
    "generate_get_params([{Name, Value}]) ->\n"
    "    Name ++ \"=\" ++ encode_get_param(Value);\n"
    "generate_get_params([{_Name, undefined}|Params]) ->\n"
    "    generate_get_params(Params);\n"
    "generate_get_params([{_Name, \"\"}|Params]) ->\n"
    "    generate_get_params(Params);\n"
    "generate_get_params([Param | Params]) ->\n"
    "    case generate_get_params(Params) of \n"
    "        \"\" ->\n"
    "            generate_get_params([Param]);\n"
    "        Str ->\n"
    "           generate_get_params([Param]) ++  \"&\" ++ Str\n"
    "    end.\n"
    "\n"
    "encode_get_param(Param) when is_integer(Param) ->\n"
    "    encode_get_param(integer_to_list(Param));\n"
    "encode_get_param(Param) ->\n"
    "    http_uri:encode(Param).\n\n"
    "add_get_params(Url, \"\") ->\n"
    "    Url;\n"
    "add_get_params(Url, Params) ->\n"
    "    Url ++ \"?\" ++ Params.\n"
    "\n"
    "get_param_field_names(TypeName, _DataModel=#model{tps = Types}) ->\n"
    "    DocType = lists:keyfind('_document', #type.nm,Types),\n"
    "    DocAlts = lists:append([E#el.alts||E<-DocType#type.els]),\n"
    "    Type=case lists:keyfind(TypeName, #alt.tag, DocAlts) of\n" 
    "             false -> TypeName;\n"
    "             V -> V#alt.tp\n"
    "         end,\n"
    "    #type{nm=Type, els=Elems, atts=Attrs}\n"
    "        = lists:keyfind(Type, 2, Types),\n"
    "    Names1 = get_element_names(Elems),\n"
    "    Names2 = get_attr_names(Attrs),\n"
    "    Names1++Names2.\n"
    "\n"
    "get_element_names(Elems) ->\n"
    "    lists:append([get_element_name(E)||E<-Elems]).\n"
    "\n"
    "get_element_name(#el{alts = Alternatives}) ->\n"
    "    [get_element_name_1(Alt)||Alt<-Alternatives].\n"
    "\n"
    "%%TODO: to be completed!!!\n"
    "get_element_name_1(#alt{tag = Tag, rl = true, tp=_Type, mx=Max}) ->\n"
    "    {Tag, Max}.\n"
    "\n"
    "get_attr_names(Attrs) ->\n"
    "    [get_attr_name(A)||A<-Attrs].\n"
    "\n"
    "get_attr_name(#att{nm = Name}) ->\n"
    "   {Name,1}.\n".



