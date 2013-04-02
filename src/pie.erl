-module(pie).

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

gpio_init(_Pin, _Mode) ->
  error(not_loaded).
gpio_read(_Pin) ->
  error(not_loaded).
gpio_write(_Pin, _Value) ->
  error(not_loaded).
gpio_release(_Pin) ->
  error(not_loaded).
