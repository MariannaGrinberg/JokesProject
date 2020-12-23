-module(jokesServer).

-export([start/0, add/3, randomJoke/4, getTop5/4, jokeRate/6, receiveJokes/0, newJoke/3]). 


sleep(T) ->
   receive 
   after T -> 
      true 
   end. 

%% Sends to client request for enter a new joke. 
add(Socket, Host, Port) ->
	gen_udp:send(Socket, Host, Port, "Enter joke").

%% Get random joke and sends it to the client.
randomJoke(Socket, Host, Port, Jokes) ->
	RandomIndex = rand:uniform(length(Jokes)),
	RandJoke = lists:nth(RandomIndex, Jokes),
	JokeStr = element(1, RandJoke),
	IndexStr = integer_to_list(RandomIndex),
	Packet = "random joke@"++JokeStr++"@"++IndexStr,
	gen_udp:send(Socket, Host, Port, Packet).

%% Get top 5 jokes and sends it to the client.
getTop5(Socket, Host, Port, Jokes) ->	
	NewList = createList(Jokes, []),
	SortedList = lists:reverse(lists:keysort(2,NewList)),
	Top5List = lists:sublist(SortedList, 1, 5),
	LstToReturn = lists:flatten(io_lib:format("~p",[Top5List])),
	SentPacket = string:concat("top 5@", LstToReturn),
	gen_udp:send(Socket, Host, Port, SentPacket).

%% Create New List that containes the jokes and the avarage rate for each joke.
createList([H|T], NewList) ->
	Len = length(element(2, H)),
	if
		Len > 0 ->
			Avg = lists:sum(element(2, H)) / Len;
		true ->
			Avg = 0
	end,


	Joke = [{element(1, H), Avg}],
	List = lists:append(NewList, Joke),
	createList(T, List);

createList([], NewList) ->
	NewList.

%% add a new joke to the the list and sends the updated list to the main process (Master)
newJoke(Jokes, Joke, Master) ->
	NewJoke = [{Joke, []}],
	NewJokes = lists:append(Jokes, NewJoke),
	Master ! {id, NewJokes}.

%% Get joke rate from the client, add it to the joke rate list & send to the client the average rate of this joke, 
%% And after then sending the updated list to the main process (Master).
jokeRate(Socket, Host, Port, Jokes, Msg, Master) ->
	RateStr = lists:nth(2, Msg),
	RateInt = list_to_integer(string:sub_string(RateStr, 1, 1)),
	Index = lists:nth(3, Msg),
 	I = list_to_integer(Index),
	Old = lists:nth(I, Jokes),
	OldRateList = element(2, Old),
	
	NewRateList = lists:append(OldRateList, [RateInt]),
	NewJoke = [{element(1, Old), NewRateList}],
	Len = length(Jokes),
		
	case I of
		1 -> 
			Rest = lists:sublist(Jokes, 2, length(Jokes)),
			NewJokes = NewJoke++Rest;
		
		Len ->
			Rest = lists:sublist(Jokes, length(Jokes) - 1),
			NewJokes = Rest ++ NewJoke;

		_ ->
			Start = lists:sublist(Jokes, I - 1),
			End = lists:sublist(Jokes, I + 1, length(Jokes)),
			NewJokes = Start++NewJoke++End
	end,
	
	Master ! {id, NewJokes},
	
	Avg = (lists:sum(NewRateList)) / length(NewRateList),
	AvgStr = float_to_list(Avg, [{decimals, 2}]),
	SentPacket = string:concat("joke rate@", AvgStr),
	gen_udp:send(Socket, Host, Port, list_to_binary(SentPacket)).

%% Receive a Pid message that contains id of the sending process and NewJokes list, returns the NewJokes list.
receiveJokes() ->
	receive
		{id, NewJokes} -> NewJokes
	end.
  
%% Open Socket based the port & initialize array of jokes
server(Port) ->
   {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]), 
   io:format("server opened socket:~p~n",[Socket]), 
   Jokes = [
{"Err...lang", [5,5,5,5]}, 
{"Why do programmers always mix up Halloween and Christmas? Because Oct 31 equals Dec 25", [3,5]}, 
{"How many programmers does it take to change a light bulb?\n None, It's a hardware problem", [3,5,1,2]}, 
{"There are only 10 kinds of people in this world: those who know binary and those who don't.",[4,3,2,5]},
{"Knock, knock.\nWho's there?\nvery long pause...\nJava.",[5,5,4,2,5]},
{"Q. How did the programmer die in the shower?\nA. He read the shampoo bottle instructions: Lather. Rinse. Repeat.",[4,5,3,4,4]},
{"Programmer (noun.) - A machine that turnes coffee into code.", [4,4,5,2,1]},
{"Q: What do you call a programmer from finland?\nA: Nerdic",[2,2,3,4,5]},
{"Q: Why did the programmer quit his job?\nA: Because he didn't get arrays. (a raise)",[5,5,4,3,1]},
{"Q: 0 is false and 1 is true, right?\nA: 1",[5,5,2,4,1,3]},
{"Q: Why do Java programmers hace to wear glasses?\nA: Because they don't C#. (see sharp)",[5,3,4,2,1,5]}],
   loop(Socket, Jokes). 

%% The Main process (Master), listening and handling UDP messages from the Client. 
loop(Socket, Jokes) ->
	Self = self(),
	
	inet:setopts(Socket, [{active, once}]), 
	receive
		
		%% if client enter "add joke" or "top 5" or "random joke" the spawn function will exe. 
		%% for "add joke" request, add function will send the client request for enter a new joke.
		{udp, Socket, Host, Port, <<"add joke">>} ->
			io:format("server received add joke request ~n"), 
			spawn(fun() -> add(Socket, Host, Port) end),
			io:format("**add joke** request handeled~n"), 
			sleep(1000),	  
			loop(Socket, Jokes);
		
		%% for "top 5" request, getTop5 function will send the client the top 5 reated jokes.
		{udp, Socket, Host, Port, <<"top 5">>} ->
			io:format("server received top 5 request~n"), 
			spawn(fun() -> getTop5(Socket, Host, Port, Jokes) end),
			io:format("**get top 5** request handeled~n"), 
			sleep(1000),	  
			loop(Socket, Jokes);
	
		%% for "random joke" request, randomJoke function will send the client random joke and the client can rate the joke and see the avg rate. 
		{udp, Socket, Host, Port, <<"random joke">>} ->
			io:format("server received random joke request~n"),
			spawn(fun() -> randomJoke(Socket, Host, Port, Jokes) end),
			io:format("**random joke** request handeled~n"), 
			sleep(1000),
			loop(Socket, Jokes);

		%% In case of the client send another input like new joke, joke rate or an error. 
		{udp, Socket, Host, Port, Other} ->
			Packet = binary_to_list(Other),
			Msg = string:split(Packet, "@", all),
			MsgType = lists:nth(1, Msg),
			
			case MsgType of
				"new joke" ->
					Joke = lists:nth(2, Msg),
					spawn(fun() -> newJoke(Jokes, Joke, Self) end),
					NewJokes = receiveJokes(),
					io:format("new joke added successfully~n"),
					io:format("~p", [NewJokes]),
					loop(Socket, NewJokes);
				
				"joke rate" ->
					io:format("server received joke rate request~n"),
					spawn(fun() -> jokeRate(Socket, Host, Port, Jokes, Msg, Self) end),
					io:format("**Avg** request handeled~n"),
					NewJokes = receiveJokes(),
					loop(Socket, NewJokes);
				
				%% Error handeling
				_ ->
					io:format("server received Unknown ~p ~n",[Other]),
					gen_udp:send(Socket, Host, Port, ["unknown"]),
					loop(Socket, Jokes)
			end
   end. 



%% Start server function
start() -> 
   spawn(fun() -> server(4000) end).
   


