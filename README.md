Requires https://github.com/omerk/pihwm

Requires root privileges to access gpio interface

```
1> pie:gpio_init(4, input).
ok
2> pie:gpio_read(4).
0
3> pie:gpio_release(4).
ok
4> pie:gpio_init(17, output).
ok
5> pie:gpio_write(17, high).
ok
5> pie:gpio_write(17, 1). % same as high atom
ok
```

It's also useful to write a series of highs and lows with microsecond
delays between them. You can do this too with gpio_write/2

```
6> pie:gpio_write(17, [1, 400, 0, 133, 1, 400, 0, 5133]).
ok
```

This equates to:
 * high
 * delay 400 microseconds
 * low
 * delay 133 microseconds
 * high
 * delay 400 microseconds
 * low
 * delay 5133 microseconds
