# HAPPY-NEOVIM

## Summary:

A small wrapper around a captured NeoVIM process, providing a _real NeoVIM_ experience inside Common Lisp applications.

The provided nvim class takes care of RPC with NeoVIM.  Keystrokes may be sent to NeoVIM, while generic functions are invoked to service NeoVIM's screen upates.  A toy demo demonstrates an in-repl control of a NeoVIM instance. 

Resource utilization and dependencies are minimal: the project requires no multithreading, and provides a byte-at-a-time state-machine messagepack decoder to minimize consing and improve reliability (the state machine can be fed a byte at a time without getting stuck waiting for more data).

Status: proof of concept / early development.

## Requirements

* A functioning NeoVIM installation;
* external-program (to start NeoVIM portably)

Optionally,

* cl-interpol - to simplify sending escape and complicated key sequences for now.

## Hands-on

A working 'vimtoy' implementation provides a simple demo of the wrapper.

```
(ql:quickload :happy-neovim)
(in-package :hn)
HN> (connect (make-instance 'nvim)  64 10) 
HN> (vin)

#("                                                                "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "[No Name]                                     0,0-1          All"
  "                                                                ") 
3524
HN> (! "iHello World!")
HN> (vin)

#("Hello World!                                                    "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "~                                                               "
  "[No Name] [+]                                 1,13           All"
  "-- INSERT --                                                    ") 
384                 
HN> (interpol:enable-interpol-syntax)
HN> (! #?"\e")
```

Note: some screen noise omitted for brevity.


## Messagepack State Machine


The state machine parser is a set of layered classes that create a parser for a protocol described with a lisp-like description language.  The plan is a sexp where each element is either an expected %TYPE/value, or a list of alternative %TYPES/values.  In addition, keyword side effects capture values into parameters, call functions, and provide looping contstructs.


```
(parse-plan
   '(:LOOP
     (%ARR 
      ;; RESPONSE: 1 <id> <error> <data>
      (4 %U 1 %U * :VAR 1  %ANY * :VAR 2 %ANY * :VAR 3  :FUN (RESPONSE  1 2 3))
      ;; REDRAW    2 "redraw" <var data array>
      (3 %U 2  :STRINGS ("redraw" %ARR * :GOTO *plan-redraw*))))))
```

## Hacking it

Messagepack bytes are assembled into atomic values and headers.  Lambda stored in OBJ-HANDLER slot is called when a %TYPE/VALUE pair is assembled.  For atomic data it is just type and value; for headers the value represents the count of items to follow.



Each file provide detailed comments about its functionality.


### READER.LISP - Layer 1

Defines RPC types, accepts bytes (see #'decode) and outputs typed atomic values and aggregate headers via 'obj-handler' function-slot.

### SUPPORT.LISP - Layer 2

Expands reader functionality by storing additional information for looping and loop index.

### RPC.LISP - Layer 3

Implements a parser that locksteps with a plan (a formal grammar description of RPC data).

### PLAN.LISP 

Implements a compile-time translator that assists users in preparing a plan.

### CAPTIVE-NEOVIM.LISP

Implements a toy neovim process with input and output captured for the Lisp user.


