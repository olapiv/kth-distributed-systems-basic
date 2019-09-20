Suggestions for Tutorial improvements:

- Update method in vect.erl?

----------

T(e1) < T(e2) --> e1 happened before e2.

However, if
(T(e1) < T(e2)) --> false,
this does not mean
(T(e1) >= T(e2))

Example:
    T(e1) = [{paul,1},{john,1},{ringo,4}]
    T(e2) = [{paul,2}]

In the norm we are working in, T(e1) and T(e2) are simply not comparable.

What are cases where this gives us an advantage over the normal Lamport clock?
