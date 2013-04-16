// An Erlang NIF which wraps the WiringPi library (by Gordon Henderson).
// Copyright (C) 2012  Klas Johansson
//
// This file is part of pie.
//
// pie is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// pie is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with pie.  If not, see <http://www.gnu.org/licenses/>.

#include <stdio.h>
#include "erl_nif.h"
#include <pihwm.h>
#include <pi_gpio.h>

#define MAX_ATOM_LEN 32

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM
gpio_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, mode;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &mode))
    {
        return enif_make_badarg(env);
    }
    if(gpio_init(pin, mode) == 1) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "failed"));
    }
}

static ERL_NIF_TERM
gpio_release_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    if(gpio_release(pin) == 1) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "failed"));
    }
}

static ERL_NIF_TERM
gpio_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    int read = gpio_read(pin);
    if(read == -1) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "failed"));
    } else {
        return enif_make_int(env, read);
    }
}

static ERL_NIF_TERM
gpio_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    if(gpio_write(pin, value) == 1) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "failed"));
    }
}

static ERL_NIF_TERM
gpio_write_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin;
    unsigned int len;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_list_length(env, argv[1], &len))
    {
        return enif_make_badarg(env);
    }
    if(len == 0) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM value_term;
    ERL_NIF_TERM wait_term;
    int value, wait;
    ERL_NIF_TERM current = argv[1];
    ERL_NIF_TERM tail;
    while(enif_get_list_cell(env, current, &value_term, &tail)) {
        current = tail;
        if(enif_get_list_cell(env, current, &wait_term, &tail)) {
            enif_get_int(env, value_term, &value);
            enif_get_int(env, wait_term, &wait);
            if(!gpio_write(pin, value)) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "failed"));
            }
            delayMicroseconds(wait);
        } else {
 
        }
        current = tail;
    }
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
  // the basics: pins and stuff
  {"gpio_init_nif", 2, gpio_init_nif},
  {"gpio_read", 1, gpio_read_nif},
  {"gpio_write_nif", 2, gpio_write_nif},
  {"gpio_write_list", 2, gpio_write_list_nif},
  {"gpio_release", 1, gpio_release_nif}
};

ERL_NIF_INIT(pie, nif_funcs, load, NULL, NULL, NULL)
