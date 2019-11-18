%% node1:

c(test).
c(node1).

test:start(node1).
test:start(node1, <0.91.0>).
<0.91.0> ! probe.

%% node2:

c(test).
c(node2).
c(storage).
c(key).
c(chordy).

test:start(node2).
test:start(node2, <0.101.0>).

<0.101.0> ! probe.


Keys = test:keys(4000).
test:add(Keys, <0.101.0>).
test:check(Keys, <0.101.0>).



test:add(20, "Blablabla", <0.101.0>).
test:lookup(20, <0.101.0>).

<0.101.0> ! printStatus. 

%% Chordy:

chordy:start(5, 5, 1000).

%% node3:

c(test).
c(node3).
c(storage).
c(key).
c(chordy).

test:start(node3).
test:start(node3, <0.81.0>).

<0.101.0> ! probe.
<0.101.0> ! stop.
<0.101.0> ! probe.
