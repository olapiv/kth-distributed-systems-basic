


run rudy [start ["8001"]]

-s rudy [start ["8001"]]

erl -s rudy start[8001]

% One Terminal 1
f().
c(http).
c(rudy).
c(bench).
Port = 8001.
Host = rudy:start(Port).
BenchtimeTwoT = bench:bench("130.229.142.38", Port).


Opt = [list, {active, false}, {reuseaddr, true}].
{ok, Server} = gen_tcp:connect(Host, Port, Opt).

% Terminal 1
f().
c(http).
c(rudy).
rudy:start(8001).

% Terminal 2




PE = spawn(rudy, start, [8001]).
PE = spawn(rudy, start, [8001]).



% GNU:
# Set linestyle 1 to blue (#0060ad)
set style line 1 \
    linecolor rgb 'red' \
    linetype 1 linewidth 2 \
    pointtype 7 pointsize 1.5

set style line 2 \
    linecolor rgb 'blue' \
    linetype 1 linewidth 2 \
    pointtype 7 pointsize 1.5

set style line 3 \
    linecolor rgb 'green' \
    linetype 1 linewidth 2 \
    pointtype 7 pointsize 1.5

set style line 4 \
    linecolor rgb 'purple' \
    linetype 1 linewidth 2 \
    pointtype 7 pointsize 1.5

set xlabel "Total consecutive requests"
set ylabel "Requests handled per second"

plot 'rudy_basic_no_delay.data' with linespoints linestyle 1,\
    'rudy_basic.data' with linespoints linestyle 2

plot 'rudy_basic.data' with linespoints linestyle 1,\
    'rudy_pool.data' with linespoints linestyle 2


plot 'rudy_1Handler_1Client_Long.data' with linespoints linestyle 1

plot 'rudy_1Handler_1Client.data' with linespoints linestyle 1,\
    'rudy_2Handlers_1Client.data' with linespoints linestyle 2,\
    'rudy_4Handlers_1Client.data' with linespoints linestyle 3,\
    'rudy_6Handlers_1Client.data' with linespoints linestyle 4

plot 'rudy_1Handler_4Clients.data' with linespoints linestyle 1,\
    'rudy_2Handlers_4Clients.data' with linespoints linestyle 2,\
    'rudy_4Handlers_4Clients.data' with linespoints linestyle 3,\
    'rudy_6Handlers_4Clients.data' with linespoints linestyle 4