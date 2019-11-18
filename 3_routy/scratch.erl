%% Small commands:

% Check if processes are running:
whereis(r1).
whereis(r2).
whereis(r3).

%% 

% ---------
erl -name sweden@192.168.0.45 -setcookie routy -connect_all false

c(routy).
c(map).
c(dijkstra).
c(hist).
c(intf).

routy:start(r1, stockholm).
routy:start(r2, lund).
routy:start(r3, malmo).

r1 ! {add, lund, whereis(r2)}.
r2 ! {add, malmo, whereis(r3)}.
r3 ! {add, stockholm, whereis(r1)}.

% Other connections:
GermanyNode = 'germany@192.168.0.45'.
r3 ! {add, berlin, {r1, GermanyNode}}.

r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.

%%%
% Wait for outside broadcasts..
%%%

r1 ! update.
r2 ! update.
r3 ! update.

r3 ! {send, lund, 'hello'}.
r1 ! {send, munich, 'hello'}.

% ---------
erl -name germany@130.229.188.162 -setcookie routy -connect_all false

c(routy).
c(map).
c(dijkstra).
c(hist).
c(intf).

routy:start(r1, berlin).
routy:start(r2, frankfurt).
routy:start(r3, munich).

r1 ! {add, frankfurt, whereis(r2)}.
r2 ! {add, berlin, whereis(r1)}.
r2 ! {add, munich, whereis(r3)}.
r3 ! {add, frankfurt, whereis(r2)}.

% Other connections
SwedenNode = 'sweden@192.168.0.45'.
r1 ! {add, malmo, {r3, SwedenNode}}.

r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.

%%%
% Wait for outside broadcasts..
%%%

r1 ! update.
r2 ! update.
r3 ! update.


% ---------
% Testing with Mindaugas (UK)

erl -name germany@130.229.188.162 -setcookie routy -connect_all false

c(routy).
c(map).
c(dijkstra).
c(hist).
c(intf).

routy:start(r1, berlin).
routy:start(r2, frankfurt).
routy:start(r3, munich).

r1 ! {add, frankfurt, whereis(r2)}.
r2 ! {add, berlin, whereis(r1)}.
r2 ! {add, munich, whereis(r3)}.
r3 ! {add, frankfurt, whereis(r2)}.

% Other connections:
UKNode = 'uk@130.229.174.13'.
r2 ! {add, london, {r1, UKNode}}.


r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.

%%%
% Wait for outside broadcasts..
%%%

r1 ! update.
r2 ! update.
r3 ! update.


r1 ! {send, munich, 'hello'}.
r2 ! {send, london, 'hello'}.