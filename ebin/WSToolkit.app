%%
%% %CopyrightBegin%
%% 
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

{application,WSToolKit,
	[{description, 	"A toolset for automating the testing of web services"},
	 {vsn,		"0.1"},
 	 {modules,	[gen_lib        
                         write_eqc_statem
                         som_compile.hrl  
                         gen_refac_script  
                         write_hrl
                         som_lib      
                         gen_xsd_model     
                         write_sut_api
                         reg_exp           
                         ws_diff
                         erlsom_pass2    
                         write_data_gen 
                         ws_lib]}
       	 {applications,	[kernel,stdlib]},
	 {env,		[]}
	]}.



