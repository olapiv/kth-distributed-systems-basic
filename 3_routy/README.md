# About: Routy the Router

For this report, a routing system following the link-state protocol was built. This is used in the Open Shortest Path First (OSPF) protocol, which is based on the Dijkstra algorithm invented by Edsger W. Dijkstra.

With the link-state protocol, multiple routers can be set up and con- nected with one another, whereby a single added connection is one-way. By sending each other link-state messages, which describe the immediate con- nections around the respective sending router, each router can calculate a universal map of connections. These link-state messages are not only sent, but broadcasted - in this sense meaning forwarded, so each router that is part of the network and can receive information, receives the message. Mean- while, by keeping a history of messages received from every other router, messages running in cycles are prevented.

The assignement describes these functionalities in more detail and will indulge on the difficulties faced with the current implementation and the link-state protocol in general.
