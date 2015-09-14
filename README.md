# jinterface_stdlib

Erlang stdlib implementation on Java, based on JInterface

# Contact Us

For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/jinterface_stdlib/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

# About

[Erlang](http://erlang.org) comes with a very nice java library ([jinterface](http://www.erlang.org/doc/apps/jinterface/)) that let's you _do Erlang stuff_ inside a Java program.
But Erlang programmers are used to write applications using [OTP](http://www.erlang.org/doc/).
If you check otp's code you may come accross [this comment](https://github.com/erlang/otp/blob/maint/lib/stdlib/src/gen.erl#L228-L233) that seems to indicate that there is OTP support for Java nodes. Turns out… there is no such thing.
This library goal is to fill that vacuum.

# Structure

This project provides [abstract] classes that work as your usual OTP behaviours and modules, according to the following table:

| Class | OTP Module |
| ----- | ---------- |
| `OtpGenServer` | `gen_server` |
| `OtpSysProcess` | `gen` |

# Usage

To use it just include ``priv/jinterface_stdlib.jar`` in your classpath as you do with the erlang provided jinterface.jar

## Documentation

To generate the [documentation](http://inaka.github.io/jinterface_stdlib/), run

```sh
$ make doc
```

## Usage Example

You can see this library being used in [lucene_server](http://github.com/inaka/lucene_server)
