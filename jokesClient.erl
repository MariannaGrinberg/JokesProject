-module(jokesClient).

-export([start/1]). 

%% receive message from server 
receive_message(Socket) ->
	receive 
		{udp, Socket, _, _, <<"unknown">>} ->
			io:format("Error: unknown message, try again~n"),
			done;
		
		{udp, Socket, _, _, <<"done">>} ->
			io:format("Done from server~n"),
			done;
		
		{udp, Socket, _, _, <<"~n">>} ->
			io:format("~n"),
			receive_message(Socket);
		
		%% Get a new joke from end user.
		{udp, Socket, _, _, <<"Enter joke">>} ->
			String = io:get_line("Enter a joke: "),
			gen_udp:send(Socket, "localhost", 4000, list_to_binary(["new joke@", String])),
			io:format("The joke sent succesfully\n");
		
		%% get a packet from server (header and content).
		{udp, Socket, _, _, Other} ->
			Packet = binary_to_list(Other),
			Msg = string:split(Packet, "@", all),
			MsgType = lists:nth(1, Msg),
			
			case MsgType of
				 %% Show to the end user a random joke that send from the server. Then ask the end user to rate the joke.
				"random joke" ->
					Joke = lists:nth(2, Msg),
					io:format("~s", [Joke++"\n"]),
					Rate = rateInput(),
					Index = lists:nth(3, Msg),
					SentPacket = "joke rate@"++Rate++"@"++Index,
					gen_udp:send(Socket, "localhost", 4000, list_to_binary([SentPacket])),
					receive_message(Socket);
					
				%% Show to the end user the avg rate that receive from server.
				"joke rate" ->
					Rate = lists:nth(2, Msg),
					io:fwrite("Average Rate: "++ Rate++"~n");
				
				%% Print to the end user the top 5 jokes which ranked highestly.
				"top 5" ->
					Top5Str = lists:nth(2, Msg),
					
					%% function fo parsering from a string back to list
					Parse = fun(S) -> {ok, Ts, _} = erl_scan:string(S), {ok, Result} = erl_parse:parse_term(Ts ++ [{dot,1} || element(1, lists:last(Ts)) =/= dot]), Result end,
					Top5List = Parse(Top5Str),
					Print = fun(Joke) -> 
									JokeStr = element(1,Joke),
									JokeAvg = float_to_list(element(2,Joke), [{decimals, 2}]),
									io:format(JokeStr++"\n"),
									io:format("Average Score: " ++ JokeAvg++"\n"),
									io:format("\n")end,
					io:format("Top 5 Joke by Average: \n"),
					lists:foreach(Print, Top5List)
			end
	end.

%% Input validation.
rateInput(Rate) ->
	RateNum = string:sub_string(Rate, 1, length(Rate) - 1),
	
	try
		RateInt = list_to_integer(RateNum),
		if 
			(RateInt < 1) or (RateInt > 5) ->
				io:format("Please enter a whole number between 1 and 5\n"),
			 	rateInput();
			
			true -> Rate
		end
	catch error:badarg ->
			  io:format("Please enter a whole number between 1 and 5\n"),
			  rateInput()
	end.
		

%% Get rate between 1 and 5 for random joke.
rateInput() ->
	Rate = io:get_line("Rate the joke between 1-5: "),
	rateInput(Rate).
	
%% Sends to server the client request.
client(Request) -> 
	{ok, Socket} = gen_udp:open(0, [binary]), 
	io:format("client opened socket=~p~n",[Socket]), 
	ok = gen_udp:send(Socket, "localhost", 4000, [Request]), 
	Answer = receive_message(Socket), 
	gen_udp:close(Socket), 
	Answer.
	

%% Start client
start(Request) -> 
	
	client(Request).



