# Description
The lisp-struct package is similar to the [struct module](https://docs.python.org/3/library/struct.html) in python.
It allows the packing and unpacking of binary data (e.g. saved structures or data traffic) with a very simple interface.
An *embedded language* is used to represent the data structure in the form of *format strings*.
In python, the format strings are *interpreted* at runtime.
Contrary to this, lisp-struct will *compile* the format strings using *macro expansion*.

# Examples
The following code unpacks three integer values from a data array:

    (let ((binary-data #(253 210 4 21 205 91 7)))
      (lisp-struct:unpack "<bHL" binary-data))
    
This results in the list `(-3 1234 123456789)`. The format string `"<bHL"` means
* `<`: Use little-endian format
* `b`: The first item is an signed 8 bit integer
* `H`: The second item is an unsigned 16 bit integer
* `L`: The third item is an unsigned 32 bit integer

The reverse can also be done:

    (lisp-struct:pack "<bHL" '(-3 1234 123456789))

This will result in the array `#(253 210 4 21 205 91 7)`.

# Usage
After installation of the package and configuration of ASDF, the package can be loaded like this:

    (require :asdf)
    (asdf:load-system :lisp-struct)

The two examples above already show most of the functionality of the package.

## Format Strings
Many of the features in the python module are implemented in the lisp package.

### Data Format
There are two options for the data format:

| Char | Byte Order    | Aligmnent |
| :--- | :------------ | :-------- |
| `<`  | little-endian | none      |
| `>`  | big-endian    | none      |

Contrary to the python module, a format *has* to be selected.

### Data types
The following data types are supported at this time:

| Char | Type    | Signedness | Size   |
| :--- | :------ | :--------- | :----- |
| `b`  | Integer | signed     | 8 bit  |
| `B`  | Integer | unsigned   | 8 bit  |
| `h`  | Integer | signed     | 16 bit |
| `H`  | Integer | unsigned   | 16 bit |
| `l`  | Integer | signed     | 32 bit |
| `L`  | Integer | unsigned   | 32 bit |
| `q`  | Integer | signed     | 64 bit |
| `Q`  | Integer | unsigned   | 64 bit |

More types may be supported in the future.

### Repetitions
Format string characters may include a number for repetition. Example: `4B` is equivalent to `BBBB`.
