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

#include "erl_nif.h"
#include <pihwm.h>
#include <pi_gpio.h>

#define MAX_ATOM_LEN 32

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM error_failed_tuple;

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    error_failed_tuple = enif_make_tuple2(env, atom_error, enif_make_atom(env, "failed"));
    return 0;
}

static ERL_NIF_TERM
gpio_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin;
    char mode[MAX_ATOM_LEN];
    unsigned mode_len;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_atom_length(env, argv[1], &mode_len, ERL_NIF_LATIN1) ||
        !enif_get_atom(env, argv[1], (char *)&mode, mode_len + 1, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    unsigned int dir = mode[0] == 'i' ? INPUT : OUTPUT;
    return gpio_init(pin, dir) == 1 ? atom_ok : error_failed_tuple;
}

static ERL_NIF_TERM
gpio_release_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    return gpio_release(pin) == 1 ? atom_ok : error_failed_tuple;
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
        return error_failed_tuple;
    } else {
        return enif_make_int(env, read);
    }
}

static ERL_NIF_TERM
gpio_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[0], &value))
    {
        return enif_make_badarg(env);
    }
    return gpio_write(pin, value) == 1 ? atom_ok : error_failed_tuple;
}

static ErlNifFunc nif_funcs[] =
{
  // the basics: pins and stuff
  {"gpio_init", 2, gpio_init_nif},
  {"gpio_read", 1, gpio_read_nif},
  {"gpio_write", 2, gpio_write_nif},
  {"gpio_release", 1, gpio_release_nif}
};

ERL_NIF_INIT(pie, nif_funcs, load, NULL, NULL, NULL)
