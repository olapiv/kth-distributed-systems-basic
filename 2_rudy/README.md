# About: Rudy the Webserver

The topic covered by this assignment is the setup and benchmarking of a simple webserver in Erlang, based on the open-source library gen tcp. By benchmarking it’s behavior with different settings, certain conclusions can be made on how the underlying processes of the programming language Erlang work. More specifically, it helps us understand the effect concurrent programming (in Erlang equivalent to spawning multiple processes) has on distributed systems.

In this assignment, the parameters with which the webserver, which is further called ”Rudy”, was tested with, were the total amount of requests, the amount of parallel clients and the number of parallel handlers that were sharing Rudy’s listener.

The results of the experiments suggest that firstly, the number of requests handled per second drop when the amount of requests per user increase and secondly, that the number of handlers only increases the number of total handled requests per second, if there are multiple parallel clients sending these requests.