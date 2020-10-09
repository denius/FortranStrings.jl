# FortranStrings

[![Build Status](https://travis-ci.com/denius/FortranStrings.jl.svg?branch=master)](https://travis-ci.com/denius/FortranStrings.jl)
[![Coverage](https://codecov.io/gh/denius/FortranStrings.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/denius/FortranStrings.jl)

`FortranStrings.jl` is a datatype for Fortran strings emulation. These strings have two main points:
* `FortranString` is mutable (but not resizable)
* flexibility in assignment: can truncate or fill the right side with spaces if the length is not coincide.

The original Fortran strings are composed of the 1-byte elements. To create such strings:
```julia
s = FortranString{UInt8}("ABC")
```
or, the same with the shorthand macro `@F8_str`:
```julia
s = F8"ABC"
```
It is also possible to create strings with `Char` elements, or any other:
`str = FortranString{Char}("ABC")` (or `str = F"ABC"`), `S = FortranString{UInt64}("ABC")`.

Assignment must be done element-wise with `.=`. Other element-wise operations (`.`) are supported also.
```julia
julia> s = F8"ABC"
F8"ABC"

julia> s .= "abc"
F8"abc"

julia> s .= "DE"
F8"DE "

julia> s .= "f" * "abc"
F8"fab"

julia> str = F"ABC"
F"ABC"

julia> str .+= 1
F"BCD"

julia> str[2:3] .= s[1:3]
2-element view(::FortranString{Char}, 2:3) with eltype Char:
 'f': ASCII/Unicode U+0066 (category Ll: Letter, lowercase)
 'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> str
F"Bfa"

julia> @btime $s .= uppercase.($s)
  11.272 ns (0 allocations: 0 bytes)
F8"FAB"
```
Internally, the `FortranString{T}` is just a wrapper for `Vector{T}` with a bunch of broadcasting routines.

## Quotes escaping
In Fortran strings, the quotes with which the strings were created must be escaped by doubling, for example Fortran's: `'Can''t be with only single quote inside'` or `"Should be ""doubled"""`. To simplify conversion from Fortran the escape flag `d` has been introduced:
```julia
julia> str = F"Should be \"\"doubled\"\""d
F"Should be \"doubled\""
```
Otherwise, without the flag, the single quotes can be doubled:
```julia
julia> s = F"Can''t be with only single quote inside"
F"Can't be with only single quote inside"
```
Although escaping is not necessary, you should remember that double `''` will give only single `'`, and the same for `\"\"` with `d` flag, when `FortranString` is created from `String`. This does not apply when a string is created from an vector:
```julia
julia> FortranString{Char}([0x27, 0x27, 0x61, 0x62, 0x63, 0x27, 0x27])
F"''abc''"
```
