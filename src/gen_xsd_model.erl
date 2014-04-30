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
-module(gen_xsd_model).

-export([gen_xsd_model/1]).
       
-export([test/0, test1/0, test2/0, test3/0]).


-include("../include/erlsom_parse.hrl").
-include("../include/erlsom.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/wsdl20.hrl").

%%@private
test()->
    gen_xsd_model("../tests/weather/weather.xsd").
%%@private
test1() ->
    gen_xsd_model("../tests/bookstore_sample/book.xsd").
%%@private
test2()->
    gen_xsd_model("../tests/bookstore_sample/person.xsd").
%%@private
test3() ->
    gen_xsd_model("../tests/vodkatv_sample/vodkatv.xsd").
                                 
gen_xsd_model(XsdFile) ->
    case erlsom:compile_xsd_file(XsdFile, []) of
        {ok, Model} ->
            {ParseRes, _}=xmerl_scan:file(XsdFile),
            %% currently does not work with nested defintions.
            NewModel=add_simple_type_info(Model, ParseRes), 
            {ok, NewModel};
        {error, Error} -> 
            {error, Error}
    end.

 
add_simple_type_info(Model, ParseRes)->
    Content=ParseRes#xmlElement.content,
    SimpleTypes=[C||C<-Content,
                    is_record(C, xmlElement) andalso
                        lists:last(string:tokens(
                                     atom_to_list(C#xmlElement.name), [$:]))
                        =="simpleType"],
    update_simple_types_in_model(Model, SimpleTypes).
   
update_simple_types_in_model(Model, []) ->
    Model;
update_simple_types_in_model(Model, [Type|SimpleTypes]) ->
    NewModel=update_a_simple_type_in_model(Model, Type),
    update_simple_types_in_model(NewModel, SimpleTypes).

update_a_simple_type_in_model(Model, Type) ->
    Attrs= Type#xmlElement.attributes,
    Content=Type#xmlElement.content,
    [NameAttr]=[A||A<-Attrs, A#xmlAttribute.name==name],
    Name = NameAttr#xmlAttribute.value,
    Elems=[C||C<-Content, is_record(C, xmlElement)],
    Restrictions = [E||E<-Elems,
                    is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                    =="restriction"],
    NewModel=update_simple_type_info_in_model(Model, Name),
    case Restrictions of 
        [R] -> 
            update_simple_type_restriction(NewModel, Name, R);
        [] ->
            NewModel
    end.
    
update_simple_type_restriction(Model, Name, R) ->
    Attrs= R#xmlElement.attributes,
    Content=R#xmlElement.content,
    [Base]=[A||A<-Attrs, A#xmlAttribute.name==base],
    BaseType= lists:last(string:tokens(
                           Base#xmlAttribute.value,[$:])),
    Elems=[C||C<-Content, is_record(C, xmlElement)],
    Enums =  [E||E<-Elems,
                 is_record(E, xmlElement) andalso
                     lists:last(string:tokens(
                                  atom_to_list(E#xmlElement.name), [$:]))
                     =="enumeration"],
    Pattern=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                    =="pattern"],
    MinIncl=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                     =="minInclusive"],
    MaxIncl=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                    =="maxInclusive"],
    MinExcl=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                     =="minExclusive"],
    MaxExcl=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                    =="maxExclusive"],
    Length=[E||E<-Elems,
                is_record(E, xmlElement) andalso
                    lists:last(string:tokens(
                                 atom_to_list(E#xmlElement.name), [$:]))
                    =="length"],
    MinLength=[E||E<-Elems,
               is_record(E, xmlElement) andalso
                   lists:last(string:tokens(
                                atom_to_list(E#xmlElement.name), [$:]))
                   =="minLength"],
    MaxLength=[E||E<-Elems,
               is_record(E, xmlElement) andalso
                   lists:last(string:tokens(
                                atom_to_list(E#xmlElement.name), [$:]))
                       =="maxLength"],
    Model0=update_base_type_info_in_model(Model, Name, BaseType),
    Model1=update_simple_type_enums(Model0,Name, Enums),
    Model2=update_simple_type_pattern(Model1, Name, Pattern),
    Model3=update_simple_type_min_incl(Model2, Name, MinIncl),
    Model4=update_simple_type_max_incl(Model3, Name, MaxIncl),
    Model5=update_simple_type_min_excl(Model4, Name, MinExcl),
    Model6=update_simple_type_max_excl(Model5, Name, MaxExcl),
    Model7=update_simple_type_length(Model6, Name, Length),
    Model8=update_simple_type_min_length(Model7, Name, MinLength),
    update_simple_type_max_length(Model8, Name, MaxLength).
    

update_simple_type_pattern(Model, _Name,  []) ->Model;
update_simple_type_pattern(Model,Name, [Pattern])->
    Attrs= Pattern#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {pattern, ValueAttr#xmlAttribute.value}).
 

update_simple_type_min_incl(Model, _Name, []) ->Model;
update_simple_type_min_incl(Model,Name, [MinIncl])->
    Attrs= MinIncl#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {min_inclusive, list_to_integer(ValueAttr#xmlAttribute.value)}).
 

update_simple_type_max_incl(Model, _Name, []) ->Model;
update_simple_type_max_incl(Model,Name, [MaxIncl])->
    Attrs= MaxIncl#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name, 
                              {max_inclusive, list_to_integer(ValueAttr#xmlAttribute.value)}).

update_simple_type_min_excl(Model, _Name, []) ->Model;
update_simple_type_min_excl(Model,Name, [MinExcl])->
    Attrs= MinExcl#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {min_exclusive, list_to_integer(ValueAttr#xmlAttribute.value)}).
 

update_simple_type_max_excl(Model, _Name, []) ->Model;
update_simple_type_max_excl(Model,Name, [MaxExcl])->
    Attrs= MaxExcl#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {max_exclusive, list_to_integer(ValueAttr#xmlAttribute.value)}).

update_simple_type_length(Model, _Name, []) ->Model;
update_simple_type_length(Model,Name, [Length])->
    Attrs= Length#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {length, list_to_integer(ValueAttr#xmlAttribute.value)}).

update_simple_type_min_length(Model, _Name, []) ->Model;
update_simple_type_min_length(Model,Name, [MinLength])->
    Attrs= MinLength#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name,  
                              {min_length, list_to_integer(ValueAttr#xmlAttribute.value)}).

update_simple_type_max_length(Model, _Name, []) ->Model;
update_simple_type_max_length(Model,Name, [MaxLength])->
    Attrs= MaxLength#xmlElement.attributes,              
    [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
    update_type_info_in_model(Model, Name, 
                              {max_length, list_to_integer(ValueAttr#xmlAttribute.value)}).


update_simple_type_enums(Model, _Name, []) ->Model;
update_simple_type_enums(Model,Name, Enums)->
    EnumVals = lists:foldl(fun(E, Acc)->
                                 Attrs= E#xmlElement.attributes,              
                                 [ValueAttr]=[A||A<-Attrs, A#xmlAttribute.name==value],
                                 %% case BaseType of 
                                 %%     string ->
                                   [ValueAttr#xmlAttribute.value|Acc]
                                  %%    _ ->  %% To Fix: what are the possibilities?
                                 %%         [list_to_integer(ValueAttr#xmlAttribute.value)|Acc]
                                 %% end
                              end, [],Enums),
    update_type_info_in_model(Model, Name, {enumerations, lists:reverse(EnumVals)}).
    
update_simple_type_info_in_model(Model, Name0)->
    Name = list_to_atom(Name0),
    Types=Model#model.tps,
    NewTypes=[case T#type.nm==Name of 
                  true -> T#type{anyAttr=[{is_simple_type, true}]};
                  false -> T
              end||T<-Types],
    Model#model{tps=NewTypes}.
         
update_base_type_info_in_model(Model, Name0, BaseType) ->
    Name = list_to_atom(Name0),
    Types=Model#model.tps,
    case [T||T<-Types, T#type.nm==Name] of 
        [] -> Model;
        [CurType] ->
            #type{nm = Name, els = Elements, atts = _Attributes}=CurType,
            [#el{alts =[Alt]}]=Elements,
            NewAlt=Alt#alt{tp=list_to_atom(BaseType)},
            NewElems=[#el{alts=[NewAlt]}],
            NewType=CurType#type{els=NewElems},
            NewTypes = lists:keyreplace(Name, 2, Types, NewType),
            Model#model{tps=NewTypes}
    end.

update_type_info_in_model(Model, Name0, Info) ->
    Name = list_to_atom(Name0),
    Types=Model#model.tps,
    case [T||T<-Types, T#type.nm==Name] of 
        [] -> Model;
        [CurType] ->
            #type{nm = Name, els = Elements, atts = _Attributes}=CurType,
            [#el{alts =[Alt]}]=Elements,
            NewInfo =case Alt#alt.anyInfo of 
                         undefined -> [Info];
                         A when is_list(A) ->
                             [Info|A]
                     end,
            NewAlt=Alt#alt{anyInfo=NewInfo},
            NewElems=[#el{alts=[NewAlt]}],
            NewType=CurType#type{els=NewElems},
            NewTypes = lists:keyreplace(Name, 2, Types, NewType),
            Model#model{tps=NewTypes}
    end.

