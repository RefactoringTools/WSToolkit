-module(demo).

-compile(export_all).

gen_hrl()->
    write_hrl:write_hrl_file(
      "../tests/vodkatv/vodkatv.xsd", 
      "vodkatv.hrl").

gen_eqc_statem()->
    write_eqc_statem:write_eqc_statem(
      "../tests/vodkatv/vodkatv.wsdl", 
      "../tests/vodkatv/vodkatv.xsd",
      none, 
      "vodkatv_sut",
      tuple,
      "vodkatv_eqc.erl").

gen_sut_api() ->
    write_sut_api:write_sut_api(
      none,
      "../tests/vodkatv/vodkatv.wsdl",
      "../tests/vodkatv/vodkatv.xsd", 
      "http://localhost:8082/vodkatv/",
      "vodkatv_sut.erl").


gen_diff() ->
    ws_diff:gen_diff("../tests/vodkatv-wsdl-evolution/2014-02-10", 
             "../tests/vodkatv-wsdl-evolution/2014-02-17").


gen_refac_script() ->
    gen_refac_script:gen_refac_script(
      {"../tests/vodkatv-wsdl-evolution/2014-02-10/vodkatv.wsdl", 
       "../tests/vodkatv-wsdl-evolution/2014-02-10/vodkatv.xsd"}, 
      {"../tests/vodkatv-wsdl-evolution/2014-02-17/vodkatv.wsdl",
       "../tests/vodkatv-wsdl-evolution/2014-02-17/vodkatv.xsd"}, 
      "refac_script.erl").
