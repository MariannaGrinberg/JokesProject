# JokesProject

The project simulates a client-server system for jokes. Through the system you can add a joke, watch a random joke from the dataset and rate it and watch the 5 jokes with the highest rating.

## Instructions:

To activate the system, open two cmd windows - one for the server and one for the client, and start erlang.
In one window the server should be run by the jokesServer: start() function that activates the server.
In the second window, run the jokesClient("Request") function, sending one of the following options instead of Request:

add joke - Add a new joke to the joke dataset.
random joke - Presenting a random joke from the dataset, allowing the customer to rate the joke and return the average rating of the joke.
top 5 - Show 5 jokes with the highest average rating.

## Key modules:

### Server side

server (Port) - a function that initializes the server and the basic dataset of jokes built as a list that contains tuples - the first organ in the tuple is the joke itself and the second organ is a list that contains the ratings received for that joke, and activates the loop function (Socket, Jokes) Begins to listen to requests received from the customer.

loop (Socket, Jokes) - The function of listening and handling inquiries received from the customer. The function receives the updat the list of jokes each time to include changes such as an added joke or an additional rating for the joke. The function handles cases of defined messages, or cases of facts containing interchangeable information (content of a new joke or rating of a joke) breaks down the fact into the type of message and the relevant content. The function also handles the case where the received message is invalid. The different cases in which the server handles are each activated in a separate process to create parallel work. In these separate processes, the auxiliary functions for performing the request processing are activated:

add (Socket, Host, Port) - sends the customer a message that will activate a new joke reception at the customer
newJoke (Jokes, Joke, Master) - Adding a new joke to the list of jokes and sending a self-message (to the server process itself) to return the updated list from the process.
randomJoke (Socket, Host, Port, Jokes) - A function that randomly selects a joke from the list and sends it as a package to the customer.

jokeRate (Socket, Host, Port, Jokes, Msg, Master) - A function that receives from the customer his rating for the random joke he received, adds the new rating to the rating list of that joke and sends the customer the new average rating of the joke. The function sends the updated list of jokes updated with the new rating as a self-notification for updating.

getTop5 (Socket, Host, Port, Jokes) - A function that sorts the list of jokes according to the average rating of the joke. The function creates a new list using a recursive function - createList and compiles a list of tuples consisting of the joke itself and the average rating of the joke. The function sorts the list according to the average rating and turns the order so that the list is sorted from large to small. The function creates a list that contains the first five places and converts it to a string and sending the message back to the client.

### Customer side

client (Request) - A function that opens a new Socket for the client side, sends the request to the server, waits for a response and activates the receive_message function to handle the received response, closes the Socket and returns the response.

receive_message (Socket) - A function that handles responses that are received from the server and reach the client. The function handles defined messages, but also in cases where the messages received are part of a packet that contains a number of information details, and it is necessary to break down the packet and use the various pieces of information. These facts will consist of the type of message and the information itself, after dismantling and searching for the appropriate case for the type of message, the message will be handled accordingly. All messages that reach the customer come as a string and must be converted to the relevant type during message handling. Also, for the introduction of the rating by the customer, a validity check is performed that the input received is only an integer between 1 and 5.
