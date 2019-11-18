# About: Chordy the Distributed Hash Table

When dealing with peer-to-peer systems, it may be difficult to pinpoint which node has stored a certain set of data. With this in mind, the Chord protocol was developed, which aims to evenly distribute data amongst it’s nodes. By placing node-ids and keys of data-values onto the same axis, the keys (along with the values) can be passed to the nodes, which succeed them on this axis. By bending the axis into a circle, keys can also obtain larger values than the nodes, as their successor is then the first node on the axis.

In this assignment, a simplified version of the Chord protocol is implemented using Erlang. It enables adding and deleting nodes as well as data to the circle. Furthermore, experiments are run to analyze its scalability.