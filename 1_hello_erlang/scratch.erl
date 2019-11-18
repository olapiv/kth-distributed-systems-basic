

erl -name foo@130.229.174.83 -setcookie secret
erl -name bar@130.229.174.83 -setcookie secret

## -----
erl -name foo@130.229.174.83
erl -sname foo -setcookie secret
erl -sname foo


Load and start the suspending hello process that we defined 
before on the foo-node and register it under the name wait.

c(wait).
P = spawn(wait, hello, []).
register(wait, P).

## -----
erl -name bar@130.229.174.83
erl -sname bar -setcookie secret
erl -sname bar

{wait, ’foo’} ! "a message from bar".


{wait, 'foo@130.229.174.83'} ! "a message from bar".

{wait, foo@130.229.174.83} ! "a message from bar".

P = spawn(wait, hello, []).

Now register the process identifier under the name “foo”.

register(foo, P).

Load and start the suspending hello process that we defined before on the foo-node and register it under the name wait.


P = spawn(hello, world, []).

cd /Users/vincent/not_in_cloud/Codes/KTH/Distrubuted\ Systems/

## -----

Try to define a process on one machine ping, that sends a 
message to a registered process on another machine with its 
own process identifier in the message. The process should the 
wait for a reply. The receiver on the other machine should 
look at the message and use the process identifier to send a 
reply.



