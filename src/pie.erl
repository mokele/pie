-module(pie).

-include("pie.hrl").

-export([
    gpio_init/2,
    gpio_read/1,
    gpio_write/2,
    gpio_release/1
  ]).

-on_load(on_load/0).

on_load() ->
  case code:which(?MODULE) of
    Filename when is_list(Filename) ->
      erlang:load_nif(filename:join([filename:dirname(Filename),"../priv/pie_drv"]), []);
    Err ->
      Err
  end.

gpio_init(Pin, Mode) ->
  gpio_init_nif(Pin, mode(Mode)).
gpio_init_nif(_Pin, _Mode) ->
  error(not_loaded).

gpio_read(_Pin) ->
  error(not_loaded).
gpio_write(Pin, L) when is_list(L) ->
  gpio_write_list(Pin, L);
gpio_write(Pin, Value) ->
  gpio_write_nif(Pin, value(Value)).
gpio_write_nif(_Pin, _Value) ->
  error(not_loaded).
gpio_write_list(_Pin, _L) ->
  error(not_loaded).
gpio_release(_Pin) ->
  error(not_loaded).

mode(input) -> 1;
mode(output) -> 0;
mode(1) -> 1;
mode(0) -> 0.

value(high) -> 1;
value(low) -> 0;
value(1) -> 1;
value(0) -> 1.
