
f().
c(gms1).
c(gms2).
c(gms3).
c(test).
c(gui).
c(worker).

W1 = test:more(5, gms3, 1000).
W2 = test:more(3, gms3, 1000). 


W1 = test:more(5, gms1, 1000).

W1 = test:first(1, gms3, 1000).
test:add(2, gms3, W1, 1000).
test:add(3, gms3, W1, 1000).
test:add(4, gms3, W1, 1000).
test:add(6, gms3, W1, 1000).
test:add(7, gms3, W1, 1000).
test:add(8, gms3, W1, 1000).
test:add(9, gms3, W1, 1000).


test:add(9, gms3, <0.238.0>, 1000).


gms2:start(a). 
% a: Leader pid
gms2:start(b, a).
gms2:start(c, a).
gms2:start(d, a).
gms2:start(e, b).
a ! stop.
% b ! stop.

gms2:start(f, a).

c(gui).
gui:start("hi", <0.88.0>).