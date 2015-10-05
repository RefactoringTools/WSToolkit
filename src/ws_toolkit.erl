%% Copyright (c) 2014, Huiqing Li, Simon Thompson
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
%%%                    This is still a prototype!
%% ====================================================================
-module(ws_toolkit).

-export([write_eqc_statem/6,
         write_eqc_statem/5,
         write_sut_api/5,
         write_sut_api/4,
         write_hrl_file/2,
         gen_diff/2,
         gen_refac_script/3]).


%%@doc Generate the initial `eqc_statem' test module. This function 
%%     takes the WSDL specification of the web service, the XSD schema,
%%     the .hrl file containing the type definitions, or `none' if no
%%     .hrl file is needed, and the name of the connector module as input,
%%     and writes the `eqc_statem' module generated to `OutFile'.
%%     This function assumes that the WSDL file follows wsdl2.0 standard.
-spec write_eqc_statem(WsdlFile::file:filename()|none,
                       XsdFile::file:filename(),
                       HrlFile::file:filename()|none,
                       SUT::file:filename(),
                       Style::{grouping, tuple}|
                              {grouping, non_tuple}|
                              {non_grouping, tuple}|
                              {non_grouping, non_tuple},
                       OutFile::file:filename()) ->
                              ok | {error, Error::term()}.
write_eqc_statem(WsdlFile, XsdFile,HrlFile, SUT, Style, OutFile) ->
    write_eqc_statem:write_eqc_statem(WsdlFile, XsdFile,HrlFile, SUT, Style, OutFile).

%%@doc Generate the initial `eqc_statem' test module without using a `.hrl' file.
-spec write_eqc_statem(WsdlFile::file:filename()|none,
                       XsdFile::file:filename(),
                       SUT::file:filename(),
                       Style::{grouping, tuple}|
                              {grouping, non_tuple}|
                              {non_grouping, tuple}|
                              {non_grouping, non_tuple},
                       OutFile::file:filename()) ->
                              ok | {error, Error::term()}.
write_eqc_statem(WsdlFile, XsdFile, SUT, Style, OutFile) ->
    write_eqc_statem:write_eqc_statem(WsdlFile, XsdFile,none,SUT, Style, OutFile).

%%@doc Generate the WS connector module. The WS connector module
%%     defines a collection of connector functions that are used by
%%     QuickCheck to invoke web service operations. There is a connector
%%     function defined for each web service operation.
%%     This function takes the the .hrl file containing type definitions,
%%     the WSDL specification, the XSD schema, and the base url for the 
%%     web service as input, and writes the connector module generated to 
%%     `OutFile'. 
%%     Note: Some utility functions are added to the module generated, but 
%%     in some cases not all the utility functions are used by the connector
%%     functions. This should be improved.
-type url()::string().
-spec write_sut_api(HrlFile::file:filename()|none,
                    WsdlFile::file:filename(),
                    XsdFile::file:filename(),
                    BaseURL::url(),
                    OutFile::file:filename())->
                           ok|{error, Error::term()}.
write_sut_api(HrlFile, WsdlFile, XsdFile, BaseURL, OutFile) ->
    write_sut_api:write_sut_api(HrlFile, WsdlFile, XsdFile, BaseURL, OutFile).


%%@doc Generate the WS connector module without using a `.hrl' file.
-spec write_sut_api(WsdlFile::file:filename(),
                    XsdFile::file:filename(),
                    BaseURL::url(),
                    OutFile::file:filename())->
                           ok|{error, Error::term()}.
write_sut_api(WsdlFile, XsdFile, BaseURL, OutFile) ->
    write_sut_api:write_sut_api(none, WsdlFile, XsdFile, BaseURL, OutFile).

%%@doc Generate type definitions. This function takes an .xsd file as input, 
%%     generates the Erlang representation of types, and write the results to 
%%     `OutFile'. The `erlsom' library is used to parse .xsd files, however
%%     `erlsom' has certain limitations, for instance in Erlsom, all restrictions
%%     on simple types are ignored, and those types are treated as 'string'. As
%%     a result, some of the types generated might not be as accurate as needed.
-spec write_hrl_file(XsdFile::file:filename(), OutFile::file:filename()) 
                    -> ok | {error, term()}.
write_hrl_file(XsdFile, OutFile) -> 
    write_hrl:write_hrl_file(XsdFile, OutFile).


%%@doc This funtions tries to infer the API changes between two versions
%%     of the web service specification. It takes the WSDL and XSD specification
%%     of both versions, and reports what has been changed from the first 
%%     version to the second version.So far, the changes this tool is able to 
%%     report include: the adding/removing of WS APIs, renaming of APIs, 
%%     adding/removing of API parameters, renaming of API parameters, as 
%%     well as parameter type changes.
-spec gen_diff({OldWsdl::file:filename(), Oldxsd::file:filename()},
              {NewWsdl::file:filename(), NewXsd::file:filename()}) ->
                     {ok, [term()], [term()]}.
gen_diff({OldWsdl, OldXsd}, {NewWsdl, NewXsd}) ->
    ws_diff:ws_diff({OldWsdl, OldXsd}, {NewWsdl, NewXsd}).

%%@doc Infer the API changes between two versions of the web service 
%%     specification, and generate a refactoring script that can be 
%%     applied to the existing `eqc_statem' test model. The current 
%%     implementation ignores type changes.
-spec gen_refac_script({OldWsdl::file:filename(), Oldxsd::file:filename()},
                       {NewWsdl::file:filename(), NewXsd::file:filename()},
                       OutFile::file:filename()) ->
                              ok|{error, term()}.
gen_refac_script({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}, OutFile) ->
    gen_refac_script:gen_refac_script({WsdlFile1,XsdFile1}, {WsdlFile2,XsdFile2}, OutFile).
                                                                              
