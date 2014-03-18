%%
%% jsonpath - json data retrieval and updates via
%%            javascript-like notation
%%
%% Copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc Fast javascript-like "path" notation for querying and updating JSON
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
-module(jsonpath).
-export([search/2, replace/3, remove/2, add/3]).
-export([parse_path/1]).

-include("jsonpath.hrl").

search(Path, Data) when is_binary(Data) ->
	search(Path, jiffy:decode(Data));
search(Path, Data) ->
	search_data(parse_path(Path), Data).

replace(Path, Replace, Data) when is_binary(Data) ->
	replace(Path, Replace, jiffy:decode(Data));
replace(Path, Replace, Data) ->
	replace_data(parse_path(Path), Replace, Data).

replace_data([SearchHead|SearchTail], Replace, Structure) ->
	case Structure of
		{TupleList} ->
			%?DEBUG("tuple list: ~p", [TupleList]),
			{ replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) };
		List ->
			%?DEBUG("looks like a list: ~p", [List]),
			replace_list([SearchHead|SearchTail], Replace, List)
	end.

% Remove hack
remove(Path, Structure) ->
    replace(Path, '#remove', Structure).

add(Path, Add, Structure) ->
    replace(Path, {'#add', Add}, Structure).

replace_list([SearchHead|SearchTail], Replace, List) ->
    Index = try
                list_to_integer(binary_to_list(SearchHead)) + 1
            catch
                _:_ -> erlang:error(not_an_object)
            end,
    case (Index > length(List)) of
        true ->
            erlang:error(out_of_bounds);
        false ->
            replace_list([Index|SearchTail], Replace, List, 1, [])
    end.
replace_list([_SearchHead|_SearchTail], _Replace, [], _Count, Accum) ->
	%?DEBUG("at the end of this list with accum: ~p", [Accum]),
	lists:reverse(Accum);
replace_list([SearchHead|SearchTail], Replace, [Head|Tail], Count, Accum) ->
	%?DEBUG("list: ~p", [Head|Tail]),
	NewAccum = case SearchHead of 
		Count ->
			%?DEBUG("Found index ~p", [Count]),
			case SearchTail of
				[] ->
                                        case Replace of
                                            '#remove' -> Accum;
                                            {'#add', _} ->
                                                erlang:error(jsonpath_add_in_list);
                                            _ -> [Replace|Accum]
                                        end;
				_SearchTail ->
					%?DEBUG("Not last, so no replacement, but replaceing into: ~p", [Head]),
					[replace_data(SearchTail, Replace, Head)|Accum]
			end;
		_SearchHead ->
			%?DEBUG("Not index ~p", [Count]),
			[Head|Accum]
	end,
	replace_list([SearchHead|SearchTail], Replace, Tail, Count+1, NewAccum).



replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) ->
	replace_tuple_list([SearchHead|SearchTail], Replace, TupleList, []).
replace_tuple_list([SearchHead], {'#add', Add}, [], Accum) ->
    lists:reverse([{SearchHead,Add}|Accum]);
replace_tuple_list([_SearchHead|_SearchTail], _Replace, [], Accum) ->
	%?DEBUG("at the end of this tuple list with accum: ~p", [Accum]),
	lists:reverse(Accum);
replace_tuple_list([SearchHead|SearchTail], Replace, [Head|Tail], Accum) ->
	%?DEBUG("tuple: ~p", [Head]),
	NewAccum = case Head of
		{SearchHead, Value} ->
			%?DEBUG("Found match for ~p: ~p", [SearchHead, {SearchHead, Value}]),
			case SearchTail of
				[] ->
                                        case Replace of
                                            '#remove' -> Accum;
                                            {'#add', _} ->
                                                erlang:error(jsonpath_already_in_object);
                                            _ -> [{SearchHead, Replace}|Accum]
                                        end;
				_SearchTail ->
					%?DEBUG("Not last, so no replacement, but replaceing into : ~p",[Head]),
					[{SearchHead, replace_data(SearchTail, Replace, Value) }|Accum]
			end;
		_Other ->
			%?DEBUG("No match for ~p: ~p", [SearchHead, Other]),
			[Head|Accum]
	end,
	%?DEBUG("continue processing tail: ~p", [Tail]),
	replace_tuple_list([SearchHead|SearchTail], Replace, Tail, NewAccum).

search_data([], Data) ->
	Data;
search_data([Head|Tail], Data) ->
	%?DEBUG("Searching for ~p in ~p", [Head,Data]),
	case Head of
		<<>> ->
			search_data(Tail, Data);
		_Other ->
			case Data of 
				{_Tuple} ->
					%?DEBUG("found tuple: ~p", [Tuple]),
					search_tuple([Head|Tail], Data);
				_List ->
					%?DEBUG("found list: ~p", [List]),
					search_list([Head|Tail], Data)
			end
	end.

search_list([Head|Tail], List) ->
	%?DEBUG("list search for ~p in ~p",[Head, List]),
	try 
		Index = list_to_integer(binary_to_list(Head)) + 1,
		case (Index > length(List)) of
			true ->
				undefined;
			false ->
				search_data(Tail, lists:nth(Index, List))
		end
	catch
		_:_ -> 
			%?DEBUG("that wasn't an integer",[]),
			undefined
	end.

search_tuple([Head|Tail], Tuple) ->
	{TuplePayload} = Tuple,
	case lists:keyfind(Head, 1, TuplePayload) of
		false ->
			%?DEBUG("did not find tuple value for ~p. done.", [Head]),
			undefined;
		{Head,Value} ->
			%?DEBUG("found tuple value for ~p: ~p", [Head,Value]),
			search_data(Tail, Value)
	end.

parse_path(Path) ->
	Split = binary:split(Path, [<<".">>,<<"[">>,<<"]">>], [global]),
	lists:filter(fun(X) -> X =/= <<>> end, Split).

