# lisp-struct

## Description
The lisp-struct package is similar to the [struct module](https://docs.python.org/3/library/struct.html) in python.
It allows the packing and unpacking of binary data (e.g. saved structures or data traffic) with a very simple interface.
An *embedded language* is used to represent the data structure in the form of *format strings*.
In python, the format strings are *interpreted* at runtime.
Contrary to this, lisp-struct will *compile* the format strings using *macro expansion*.

## Examples
The following code unpacks three integer values, one character and a boolean from a data array:

    (let ((binary-data #(253 1 210 4 65 21 205 91 7)))
      (lisp-struct:unpack "<b?HcL" binary-data))

This results in the list `(-3 T 1234 #\A 123456789)`. The format string `"<b?HcL"` means
* `<`: Use little-endian format
* `b`: The first item is an signed 8 bit integer
* `?`: The second item is a boolean value
* `H`: The third item is an unsigned 16 bit integer
* `c`: The fourth item is a character
* `L`: The fifth item is an unsigned 32 bit integer

The reverse can also be done:

    (lisp-struct:pack "<b?HcL" '(-3 t 1234 #\A 123456789))

This will result in the array `#(253 1 210 4 65 21 205 91 7)`.

## Usage
After installation of the package and configuration of ASDF, the package can be loaded like this:

    (require :asdf)
    (asdf:load-system :lisp-struct)

The two examples above already show most of the functionality of the package.

### Format Strings
Many of the features in the python module are implemented in the lisp package.

#### Data Format
There are two options for the data format:

| Char | Byte Order    | Aligmnent |
| :--- | :------------ | :-------- |
| `<`  | little-endian | none      |
| `>`  | big-endian    | none      |

Contrary to the python module, a format *has* to be selected.

#### Data types
The following data types are supported at this time:

| Char | Type      | Signedness | Size   |
| :--- | :-------- | :--------- | :----- |
| `x`  | Padding   |            | 8 bit  |
| `c`  | Character |            | 8 bit  |
| `s`  | String    |            | 8 bit  |
| `?`  | Boolean   |            | 8 bit  |
| `b`  | Integer   | signed     | 8 bit  |
| `B`  | Integer   | unsigned   | 8 bit  |
| `h`  | Integer   | signed     | 16 bit |
| `H`  | Integer   | unsigned   | 16 bit |
| `l`  | Integer   | signed     | 32 bit |
| `L`  | Integer   | unsigned   | 32 bit |
| `q`  | Integer   | signed     | 64 bit |
| `Q`  | Integer   | unsigned   | 64 bit |

More types may be supported in the future.

#### Repetitions
Format string characters may include a number for repetition. Example: `4B` is equivalent to `BBBB`.
As in the python module, the number for strings indicate the length of the string instead of repetitions.

## Licence
lisp-struct is distributed with the GNU General Public License, version 2:

Copyright (C) 2022 Marco Rossini

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

You can also find the full licence [online](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).

## Questions/Bugs/Help/Requests/Feedback etc.

If you have questions regarding lisp-struct, found any bugs, would like to offer help, have a feature request, give feedback etc., feel free to contact me through GitHub.
