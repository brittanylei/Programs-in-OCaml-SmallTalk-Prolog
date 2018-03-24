% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
% Brittany Lei  bclei@ucsc.edu  asg4

% Prolog version of not
not(X) :- X, !, fail.
not(_).

conv_degmin_to_rad(degmin(Deg, Min), Rad) :-
  Degs is Deg + Min / 60,
  Rad is Degs * pi / 180.

% arguments are in radians
haversine(Lat1, Lat2, Long1, Long2, D) :-
  DLat is Lat2 - Lat1,
  DLong is Long2 - Long1,
  A is sin(DLat / 2) ** 2 + cos(Lat1) * cos(Lat2)
    * sin(DLong / 2) ** 2,
  C is 2 * atan2(sqrt(A), sqrt(1 - A)),
  R is 3961,  % miles
  D is R * C.

distance(Dep, Arr, Distance) :-
  airport(Dep, _, Lat1, Long1),
  airport(Arr, _, Lat2, Long2),
  conv_degmin_to_rad(Lat1, RLat1),
  conv_degmin_to_rad(Lat2, RLat2),
  conv_degmin_to_rad(Long1, RLong1),
  conv_degmin_to_rad(Long2, RLong2),
  haversine(RLat1, RLat2, RLong1, RLong2, D),
  Distance is D.

% planes fly at constant speeds of 500mi/h
travel_time(Dist, Time) :-
  Time is Dist / 500.

time_to_dec(time(Hour, Min), DecTime) :-
  DecTime is Hour + Min / 60.

time_under_10(Hour) :-
  Hour < 10,
  write('0'),
  write(Hour).

time_under_10(Hour) :-
  Hour >= 10,
  write(Hour).

dec_to_time(Time) :-
  Hour is floor(Time),
  time_under_10(Hour),
  write(':'),
  HunMin is ceiling(Time * 60),
  Min is mod(HunMin, 60),
  time_under_10(Min).

to_upper(Lower, Upper) :-
  atom_chars(Lower, Lowerlist),
  maplist(lower_upper, Lowerlist, Upperlist),
  atom_chars(Upper, Upperlist).


writepath([]) :-
  nl.

% write for leg to Destination.
writepath([[DepAirport, DDep, DArr], ArrAirport]) :-
  write('depart '),
  to_upper(DepAirport, UpDepAirport),
  write(UpDepAirport),
  airport(DepAirport, DCity, _, _),
  format(' ~w', [DCity]),
  dec_to_time(DDep), nl,
  write('arrive '),
  to_upper(ArrAirport, UpArrAirport),
  write(UpArrAirport),
  airport(ArrAirport, ACity, _, _),
  format(' ~w', [ACity]),
  dec_to_time(DArr),
  true.

% write for Transfers.
writepath([[DepAirport, DDep, DArr],[ArrAirport, ADep, AArr]
          |Tail]) :-
  write('depart '),
  to_upper(DepAirport, UpDepAirport),
  write(UpDepAirport),
  airport(DepAirport, DCity, _, _),
  format(' ~w', [DCity]),
  dec_to_time(DDep), nl,
  write('arrive '),
  to_upper(ArrAirport, UpArrAirport),
  write(UpArrAirport),
  airport(ArrAirport, ACity, _, _),
  format(' ~w', [ACity]),
  dec_to_time(DArr), nl,
  writepath([[ArrAirport, ADep, AArr]|Tail]).


% flight goes to end
flight_end(Trans, Arr, Tried, Path, DecArrTime) :-
  % check if arrival is in the same day
  DecArrTime < 24.0,
  Trans = Arr,
  flightpath(Arr, Arr, [Arr|Tried], Path, _).
% path with Transfers
flight_end(Trans, Arr, Tried, Path, DecArrTime) :-
  % check if arrival is in the same day
  DecArrTime < 24.0,
  not(Trans = Arr),
  flight(Trans, _, NextTDep),
  time_to_dec(NextTDep, NextTDepTime),
  % check if 30min transfer time is possible
  NextTDepTime - DecArrTime >= 0.5,
  flightpath(Trans, Arr, [Trans|Tried], Path, NextTDep).


flightpath(Dep, Arr, Outlist) :-
  flightpath(Dep, Arr, [Dep], Outlist, _).

flightpath(Dep, Dep, _, [Dep], _).

flightpath(Dep, Arr, Tried,
          [[Dep, DecDepTime, DecArrTime]|Path], DepTime) :-
  flight(Dep, Trans, DepTime),
  not(member(Trans, Tried)),
  time_to_dec(DepTime, DecDepTime),
  distance(Dep, Trans, Distance),
  travel_time(Distance, TravelTime),
  DecArrTime is DecDepTime + TravelTime,
flight_end(Trans, Arr, Tried, Path, DecArrTime).


fly(Dep, Arr) :-
  airport(Dep, _, _, _),
  airport(Arr, _, _, _),
  flightpath(Dep, Arr, Path),
  nl, writepath(Path),
  true.

% error message for zero-fly query
fly(Dep, Arr) :-
  Dep = Arr,
  write('Error: Departure and Destination airports are the same'),
  !, fail.

% error message for nonexistent airports
fly(A, _) :-
  not(airport(A, _, _, _)),
  write('Error: airport(s) does not exist'),
  !, fail.
fly(_, B) :-
  not(airport(B, _, _, _)),
  write('Error: airport(s) does not exist'),
  !, fail.
