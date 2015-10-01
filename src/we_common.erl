-module(we_common).
-compile(export_all).

tuple_to_record(Fields, Tuple) when is_tuple(Tuple)->
	tuple_to_record(Fields, [Tuple], []);
tuple_to_record(Fields ,TupleList) ->
	tuple_to_record(Fields, TupleList, []).

tuple_to_record(_, [], Acc) ->
  Acc;
tuple_to_record(Fields, [undefined|TupleList], Acc) ->
  tuple_to_record(Fields, TupleList, Acc);
tuple_to_record(Fields, [Tuple|TupleList], Acc) ->
  Record = lists:zip(Fields, tl(tuple_to_list(Tuple))),
  tuple_to_record(Fields, TupleList, [Record|Acc]).

list_to_atom(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

role_pname(RoleID) ->
    MyRoleName = "we_role_"++ integer_to_list(RoleID),
    MyRoleName2 = we_common:list_to_atom(MyRoleName),
    MyRoleName2.

string_to_tuple(A) when is_list(A) ->
  case hd(lists:reverse(A)) of
    $. ->
      B = A;
    _ ->
      B = A++"."
  end,
  {ok, Scan1, _} = erl_scan:string(B),
  {ok,P}=erl_parse:parse_exprs(Scan1),
  {value, Term, _} = erl_eval:exprs(P, []),
  Term;
string_to_tuple(A) when is_binary(A) ->
  string_to_tuple(binary_to_list(A)).

