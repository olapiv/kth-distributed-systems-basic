-module(setup).
-compile(export_all).

start() ->
  io:format("Starting the demo, initilize the cities ~n"),
  routy:start(stockholm, stockholm),
  routy:start(lund, lund),
  routy:start(uppsala, uppsala),
  routy:start(gotenberg, gotenberg),
  routy:start(malmo, malmo),
  routy:start(visby, visby),
  routy:start(linkoping, linkoping),

  stockholm ! {add, lund, {lund, node()}},
  lund ! {add, stockholm, {stockholm, node()}},
  lund ! {add, uppsala, {uppsala, node()}},
  uppsala ! {add, lund, {lund, node()}},
  lund ! {add, gotenberg, {gotenberg, node()}},
  gotenberg ! {add, lund, {lund, node()}},
  uppsala ! {add, malmo, {malmo, node()}},
  malmo ! {add, uppsala, {uppsala, node()}},
  gotenberg ! {add, visby, {visby, node()}},
  visby ! {add, gotenberg, {gotenberg, node()}},
  malmo ! {add, visby, {visby, node()}},
  visby ! {add, malmo, {malmo, node()}},
  visby ! {add, linkoping,{linkoping,node()}},
  linkoping ! {add, visby,{visby,node()}},
  stockholm ! broadcast,
  timer:sleep(1000),
  lund ! broadcast,
  timer:sleep(1000),
  uppsala ! broadcast,
  timer:sleep(1000),
  gotenberg ! broadcast,
  timer:sleep(1000),
  malmo ! broadcast,
  timer:sleep(1000),
  visby ! broadcast,
  timer:sleep(1000),
  linkoping ! broadcast,
  timer:sleep(1000),
  stockholm ! update,
  timer:sleep(1000),
  lund ! update,
  timer:sleep(1000),
  uppsala ! update,
  timer:sleep(1000),
  gotenberg ! update,
  timer:sleep(1000),
  malmo ! update,
  timer:sleep(1000),
  visby ! update,
  timer:sleep(1000),
  linkoping ! update,
  timer:sleep(1000).

stop() ->
  gotenberg ! stop.
status() ->
  routy:status(stockholm).