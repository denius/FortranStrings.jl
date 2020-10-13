module FortranStrings

export FortranString, REPEAT, LEN, LEN_TRIM, TRIM, LNBLNK, LSAME, INDEX, SCAN, @F_str, @F8_str

import Base.Broadcast: AbstractArrayStyle, DefaultArrayStyle, Broadcasted


abstract type AbstractFortranString{CharType} <: AbstractVector{CharType} end

"""
    FortranString{CharType}

Datatype for emulating FORTRAN `CHARACTER*N` strings.

This type is a wrapper for a `Vector` with elements of type `CharType`.
Some functions and broadcasting added for limited strings emulation.

Assignment should be done via broadcasting:
```jldoctest
julia> s = F8"abc"; s .= "AB" * F"CD"; s
F8"ABC"

julia> s = F"abc"; s[2:end] .= "ABC"; s
F"aAB"
```
Element-wise assignment is also supported, but it is not FORTRAN alike.
Element comparison
```jldoctest
julia> s = F8"abc"; s[1:1] == 'a'
true

julia> s = F8"abc"; (s[1:1] == 'a', s[2:2] == "b")
(true, true)
```

See also `@F_str` for quoting.
"""
struct FortranString{CharType} <: AbstractFortranString{CharType}
    data :: Vector{CharType}
end

const ForStr = FortranString

(::Type{ForStr{T}})(ch::AbstractChar) where {T} = ForStr{T}([T(ch)])
(::Type{ForStr{T}})(::UndefInitializer, n) where {T} = ForStr{T}(Array{T}(undef, n))
function (::Type{ForStr{T}})(s::AbstractString, flags::AbstractString = "") where {T}
    if length(flags) == 0
        return ForStr{T}(map(T, collect(s)))
    elseif lowercase(flags) == "q"
        return ForStr{T}(map(T, collect(replace(s, "''" => "'"))))
    elseif lowercase(flags) == "qq" || lowercase(flags) == "d"
        return ForStr{T}(map(T, collect(replace(s, "\"\"" => "\""))))
    else
        throw(ArgumentError("unknown FortranString flag: $flags"))
    end
end

Base.convert(A::Type{AbstractFortranString}, s::AbstractString) = A(s)
Base.convert(::Type{String}, fs::AbstractFortranString) = join(map(Char, fs))

Base.promote_rule(::Type{Union{AbstractChar, AbstractString}}, B::Type{AbstractFortranString}) = B
Base.promote_rule(A::Type{AbstractFortranString}, ::Type{Union{AbstractChar, AbstractString}}) = A

Base.string(fs::ForStr) = convert(String, fs)

Base.:*(fs1::ForStr{T1}, fs2::ForStr{T2}) where {T1} where {T2}=
    ForStr{T1}(vcat(fs1.data, map(T1, fs2.data)))
Base.:*(s::Union{AbstractChar, AbstractString}, fs::ForStr{T}) where {T} =
    ForStr{T}(vcat(ForStr{T}(s).data, fs.data))
Base.:*(fs::ForStr{T}, s::Union{AbstractChar, AbstractString}) where {T} =
    ForStr{T}(vcat(fs.data, ForStr{T}(s).data))

for o in (:(==), :<, :(<=), :>, :(>=), :(!=))
    @eval begin
        Base.$o(A::Union{AbstractChar, AbstractString}, B::ForStr) = ($o)(string(A), string(B))
        Base.$o(A::ForStr, B::Union{AbstractChar, AbstractString}) = ($o)(string(A), string(B))
        Base.$o(A::AbstractVector, B::ForStr) = ($o)(string(ForStr{Char}(A)), string(B))
        Base.$o(A::ForStr, B::AbstractVector) = ($o)(string(A), string(ForStr{Char}(B)))
        Base.$o(A::ForStr, B::ForStr) = ($o)(string(A), string(B))
    end
end

@inline Base.length(fs::ForStr) = length(fs.data)
@inline Base.ncodeunits(fs::ForStr) = length(fs)
@inline Base.codeunit(fs::ForStr{T}) where {T} = T<:Char ? UInt8 : unsigned(T)
@inline Base.sizeof(fs::ForStr) = sizeof(fs.data)
@inline Base.isvalid(fs::ForStr, i::Integer) = 1 <= i <= length(fs)

Base.IteratorSize(::Type{ForStr{T}}) where {T} = Base.HasLength()
Base.IteratorEltype(::Type{ForStr{T}}) where {T} = Base.HasEltype()

@inline Base.iterate(fs::ForStr) = iterate(fs.data)
@inline Base.iterate(fs::ForStr, state::Integer) = iterate(fs.data, state)

@inline Base.setindex!(fs::ForStr{T}, c, i::Integer) where {T} = setindex!(fs.data, T(c), i)
@inline Base.getindex(fs::ForStr{T}, i::Integer) where {T} = fs.data[i]
@inline Base.getindex(fs::ForStr{T}, r::AbstractRange{K}) where {T, K} = ForStr{T}(fs.data[r])

@inline Base.ndims(::ForStr) = 1
@inline Base.ndims(::Type{ForStr}) = 1
@inline Base.size(fs::ForStr) = size(fs.data)
@inline Base.axes(fs::ForStr) = axes(fs.data)
@inline Base.eltype(fs::ForStr{T}) where {T} = T
Base.similar(fs::ForStr{T}) where {T} = ForStr{T}(undef, length(fs))

function Base.show(io::IO, ::MIME"text/plain", fs::FortranString)
    show(io, fs)
    return
end
function Base.show(io::IO, fs::FortranString{T}) where {T}
    if T === Char
        print(io, 'F')
    elseif T === UInt8
        print(io, 'F', '8')
    else
        print(io, 'F', T)
    end
    show(io, string(fs))
    return
end
Base.print(io::IO, fs::FortranString) = print(io, string(fs))
Base.textwidth(fs::FortranString) = textwidth(string(fs))

# Broadcasting

struct FortranStringStyle <: AbstractArrayStyle{1} end

const ForStrStyle = FortranStringStyle

ForStrStyle(::Val{0}) = ForStrStyle()
ForStrStyle(::Val{1}) = ForStrStyle()
ForStrStyle(::Val{N}) where N = DefaultArrayStyle{N}()
Base.Broadcast.BroadcastStyle(::Type{<:ForStr}) = ForStrStyle()

function Base.Broadcast.instantiate(bc::Broadcasted{ForStrStyle})
    if bc.axes isa Nothing
        bcaxes = Broadcast.combine_axes(bc.args...)
    else
        bcaxes = bc.axes
        # FortranString is flexible in assignment in any direction thus any sizes are allowed
        #check_broadcast_axes(axes, bc.args...)
    end
    return Broadcasted{ForStrStyle}(bc.f, bc.args, bcaxes)
end
Base.similar(bc::Broadcasted{ForStrStyle}, ::Type{ElType}) where {ElType} = similar(bc, ElType, axes(bc))
Base.similar(::Broadcasted{ForStrStyle}, ::Type{ElType}, dims) where {ElType} = similar(ForStr{ElType}, dims)
Base.similar(::Broadcasted{ForStrStyle}, ::Type{Bool}, dims) = similar(BitArray, dims)

@inline unref(x) = isa(x, Base.RefValue{String}) ? x[] : x

const AllCharTypes = Union{AbstractChar, AbstractString, Ref{<:AbstractString}, AbstractVector{<:AbstractChar}}
const CmpOps = Union{typeof(<), typeof(<=), typeof(==), typeof(>=), typeof(>), typeof(!=)}

Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted,Broadcasted}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, bc.args)), copy(bc.args[1]), copy(bc.args[2]))

Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted,<:AllCharTypes}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (bc.args[1], Char[]))), copy(bc.args[1]), unref(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted,<:Any}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, bc.args)), copy(bc.args[1]), bc.args[2])

Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:AllCharTypes,Broadcasted}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], bc.args[2]))), unref(bc.args[1]), copy(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any,Broadcasted}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, bc.args)), bc.args[1], copy(bc.args[2]))

Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:AllCharTypes,<:AllCharTypes}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], Char[]))), unref(bc.args[1]), unref(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any,<:AllCharTypes}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (bc.args[1], Char[]))), bc.args[1], unref(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:AllCharTypes,<:Any}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], bc.args[2]))), unref(bc.args[1]), bc.args[2])
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any,<:Any}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, bc.args)), bc.args[1], bc.args[2])

Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:CmpOps, <:Tuple{<:AllCharTypes,<:AllCharTypes}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], Char[]))), unref(bc.args[1]), unref(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:CmpOps, <:Tuple{<:Any,<:AllCharTypes}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], Char[]))), bc.args[1], unref(bc.args[2]))
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:CmpOps, <:Tuple{<:AllCharTypes,<:Any}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, (Char[], Char[]))), unref(bc.args[1]), bc.args[2])
Base.copy(bc::Broadcasted{ForStrStyle, <:Any, <:CmpOps, <:Tuple{<:Any,<:Any}}) =
     fortranstringbroadcast!(bc.f, similar(bc, Broadcast.combine_eltypes(bc.f, bc.args)), bc.args[1], bc.args[2])


Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted,Broadcasted}}) =
    # TODO: an unwanted copy is making here
    fortranstringbroadcast!(bc.f, dest, copyto!(dest, bc.args[1]), copy(bc.args[2]))

Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted,<:Any}}) =
    fortranstringbroadcast!(bc.f, dest, copyto!(dest, bc.args[1]), bc.args[2])

Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any,Broadcasted}}) =
    fortranstringbroadcast!(bc.f, dest, bc.args[1], copyto!(dest, bc.args[2]))

Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any,<:Any}}) =
    fortranstringbroadcast!(bc.f, dest, bc.args[1], bc.args[2])

Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{Broadcasted}}) =
    fortranstringcopyto!(bc.f, dest, copyto!(dest, first(bc.args)))

Base.copyto!(dest::AbstractVector, bc::Broadcasted{ForStrStyle, <:Any, <:Any, <:Tuple{<:Any}}) =
    fortranstringcopyto!(bc.f, dest, bc.args[1])

# special case for `String` because it has `DefaultArrayStyle{0}`
# see also broadcast.jl:852 -- `copy(bc::Broadcasted{<:AbstractArrayStyle{0}}) = bc[CartesianIndex()]`
Base.copyto!(dest::Union{SubArray{T,N,P},ForStr{T}}, bc::Broadcasted{DefaultArrayStyle{0}}) where {T,N,P<:ForStr} =
    fortranstringcopyto!(bc.f, dest, bc.args[1])


for (Tc,oc) in [(Any, :UInt64), (CmpOps, :Char)]
    for (T1,o1) in [(Any, :identity), (Ref{<:AbstractString}, :getindex)]
        for (T2,o2) in [(Any, :identity), (Ref{<:AbstractString}, :getindex)]
            """
            Apply `op` element-wise on `src1` and `src2` converting to `Char`
            in of comparison operations, and store the result in `dest`.
            """
            @eval function fortranstringbroadcast!(op::$Tc, dest, src1::$T1, src2::$T2)
                i = firstindex(dest) - 1
                l1 = length($o1(src1))
                l2 = length($o2(src2))
                if l1 == l2 == 1
                    v1 = first($o1(src1))
                    v2 = first($o2(src2))
                    dest[i+=1] = op($oc(v1), $oc(v2))
                elseif l1 == 1
                    v1 = first($o1(src1))
                    for v2 in $o2(src2)
                        dest[i+=1] = op($oc(v1), $oc(v2))
                    end
                elseif l2 == 1
                    v2 = first($o2(src2))
                    for v1 in $o1(src1)
                        dest[i+=1] = op($oc(v1), $oc(v2))
                    end
                else
                    for (v1,v2) = zip($o1(src1), $o2(src2))
                        dest[i+=1] = op($oc(v1), $oc(v2))
                    end
                end
                return dest
            end
        end
    end
end

for (T1,o1) in [(Any, :identity), (Ref{<:AbstractString}, :getindex)]
    """
    Apply `op` element-wise on `src` converted to `Char` and set the result into `dest`.
    The result vector will be stretched or shrinked to satisfy size of `dest`.
    """
    @eval function fortranstringcopyto!(op, dest, src::$T1)
        (lendest = length(dest)) == 0 && return dest
        T = eltype(dest)
        i = firstindex(dest)
        lastind = lastindex(dest)
        for v in $o1(src)
            i <= lastind || break
            dest[i] = T(op(Char(v)))
            i += 1
        end
        while i <= lastind
            dest[i] = T(' ')
            i += 1
        end
        return dest
    end
end

# Some `String` and FORTRAN functions

Base.strip(fs::AbstractFortranString) = lstrip(rstrip(fs))

Base.lstrip(fs::AbstractFortranString) =
    typeof(fs)(fs.data[something(findfirst(c->!isspace(Char(c)), fs),end+1):end])

Base.rstrip(fs::AbstractFortranString) =
    typeof(fs)(fs.data[1:something(findlast(c->!isspace(Char(c)), fs),0)])

function Base.repeat(fs::AbstractFortranString, n::Integer)
    l = length(fs)
    r = typeof(fs)(undef, n*l)
    for i = 0:n-1
        @view(r[1 + i*l:l + i*l]) .= fs
    end
    return r
end

Base.uppercase(fs::ForStr) = typeof(fs)(map(uppercase, string(fs)))
Base.lowercase(fs::ForStr) = typeof(fs)(map(lowercase, string(fs)))
Base.uppercase(fs::AbstractFortranString) = typeof(fs)(map(uppercase, Char.(fs.data)))
Base.lowercase(fs::AbstractFortranString) = typeof(fs)(map(lowercase, Char.(fs.data)))

Base.occursin(r, fs::AbstractFortranString) = occursin(r, string(fs))
Base.occursin(r::AbstractFortranString, fs::AbstractFortranString) = occursin(string(r), string(fs))
Base.occursin(r::AbstractFortranString, fs) = occursin(string(r), fs)
Base.occursin(r::Regex, fs::AbstractFortranString; offset::Integer=0) = occursin(r, string(fs), offset=offset)

Base.match(r, fs::AbstractFortranString) = match(r, string(fs))

Base.split(fs::AbstractFortranString, splitter::AbstractFortranString; limit::Integer=0, keepempty::Bool=true) =
    map(typeof(fs), split(string(fs), string(splitter), limit=limit, keepempty=keepempty))
Base.split(fs::AbstractFortranString, splitter; limit::Integer=0, keepempty::Bool=true) =
    map(typeof(fs), split(string(fs), splitter, limit=limit, keepempty=keepempty))
Base.split(fs, splitter::AbstractFortranString; limit::Integer=0, keepempty::Bool=true) =
    map(typeof(fs), split(fs, string(splitter), limit=limit, keepempty=keepempty))


"""
    REPEAT(STRING, N)

Repeat `STRING` `N` times.
"""
REPEAT(s::Union{AbstractChar,AbstractString,AbstractFortranString}, n::Integer) = repeat(s, n)

"""
    LEN(STRING)

FORTRAN compatible name alias for `length()`.
"""
LEN(s::Union{AbstractChar,AbstractString,AbstractFortranString}) = length(s)

"""
    LEN_TRIM(STRING)

Return the length of the `STRING` with trailing spaces ignored.
"""
LEN_TRIM(s::Union{AbstractChar,AbstractString,AbstractFortranString}) =
    something( findlast(c->!isspace(Char(c)), string(s)), 0 )

"""
    LNBLNK(STRING)

Alias for LEN_TRIM.
"""
LNBLNK(s::Union{AbstractString,AbstractFortranString}) = LEN_TRIM(s)

"""
    TRIM(STRING)

Return a truncated copy of the `STRING` where all trailing
spaces are removed.
"""
TRIM(s::Union{AbstractString,AbstractFortranString}) = rstrip(s)

"""
    LSAME(s1, s2)

Standard BLAS function to compare first character of two strings,
returns `bool`.
"""
LSAME(s1, s2) = uppercase(Char(first(s1))) == uppercase(Char(first(s2)))

"""
    INDEX(STRING, SUBSTRING, BACK=false)

Return the position of the start of the first occurrence of string `SUBSTRING`
as a substring in `STRING`, counting from one.

If `SUBSTRING` is not present in `STRING`, zero is returned.
If the `BACK` argument is present and true, the return value is the start of
the last occurrence rather than the first.
"""
INDEX(s::Union{AbstractString,AbstractFortranString}, substr, back::Bool) =
    first(something( back ? findlast(string(substr), string(s)) : findfirst(string(substr), string(s)), 0 ))
@inline INDEX(s::Union{AbstractString,AbstractFortranString}, substr; back::Bool=false) =
    INDEX(s, substr, back)

"""
    SCAN(STRING, CHARSET, BACK=false)

Return the position of the start of the first occurrence of string `CHARSET`
as a `CHARSET` in `STRING`, counting from one.

If `CHARSET` is not present in `STRING`, zero is returned. If the `BACK` argument
is present and true, the return value is the start of the last occurrence rather than
the first.
"""
function SCAN(s::Union{AbstractString,AbstractFortranString}, charset, back::Bool)
    if back
        i = 0
        for c in charset
            i = max( something(findlast(==(Char(c)), string(s)), i), i)
        end
        return i
    else
        i = length(s) + 1
        for c in charset
            i = min( something(findfirst(==(Char(c)), string(s)), i), i)
        end
        return i > length(s) ? 0 : i
    end
end
@inline SCAN(s::Union{AbstractString,AbstractFortranString}, charset; back::Bool=false) =
    SCAN(s, charset, back)

"""
    F"some string"

String macro for `FortranString{Char}("some string")`.

The `'d'` and `"qq"` flag can be used to emulate behaviour fortran strings in double quotes,
where the double quotes inside such strings must be doubled:

    F8"Use \\"\\"doubled\\"\\""qq

The `'q'` flag indicates the emulation of Fortran strings in single quotes:

    F8"Can''t be with only single quote inside"q

"""
macro F_str(s, flags...) ; FortranString{Char}(s, flags...) ; end

"""
    F8"some string"

String macro for compatible with FORTRAN strings `FortranString{UInt8}`.

The `'d'` and `"qq"` flag can be used to emulate behaviour fortran strings in double quotes,
where the double quotes inside such strings must be doubled:

    F8"Use \\"\\"doubled\\"\\""qq

The `'q'` flag indicates the emulation of Fortran strings in single quotes:

    F8"Can''t be with only single quote inside"q

"""
macro F8_str(s, flags...) ; FortranString{UInt8}(s, flags...) ; end


end # of module FortranStrings

