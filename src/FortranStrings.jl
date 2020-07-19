module FortranStrings

export FortranString, REPEAT, LEN, LEN_TRIM, TRIM, LNBLNK, LSAME, @F_str, @F8_str

import Base.Broadcast:
AbstractArrayStyle, DefaultArrayStyle, Broadcasted

"""
    FortranString{CharType}
Datatype for FORTRAN `CHARACTER*N` strings emulation.
The type parameter `CharType` specifies the type of elements in the string.
"""
struct FortranString{CharType} <: AbstractVector{CharType}
    data :: Array{CharType, 1}
end

const ForStr = FortranString

(::Type{FortranString{T}})(str::AbstractString) where {T} = FortranString{T}(map(T, collect(str)))
(::Type{FortranString{T}})(ch::AbstractChar) where {T} = FortranString{T}([T(ch)])
(::Type{FortranString{T}})(::UndefInitializer, n) where {T} = FortranString{T}(Array{T}(undef, n))

Base.convert(::Type{FortranString{T}}, str::AbstractString) where {T} = FortranString{T}(str)
Base.convert(::Type{String}, fstr::FortranString{T}) where {T} = join(map(Char, fstr.data))

Base.promote_rule(::Type{Union{AbstractChar, AbstractString}}, ::Type{FortranString{T}}) where {T} = FortranString{T}
Base.promote_rule(::Type{FortranString{T}}, ::Type{Union{AbstractChar, AbstractString}}) where {T} = FortranString{T}

Base.string(fstr::FortranString{T}) where {T} = convert(String, fstr)

Base.:*(fstr1::FortranString{T1}, fstr2::FortranString{T2}) where {T1} where {T2}=
    FortranString{T1}(vcat(fstr1.data, map(T1, fstr2.data)))
Base.:*(str::Union{AbstractChar, AbstractString}, fstr::FortranString{T}) where {T} =
    FortranString{T}(vcat(FortranString{T}(str).data, fstr.data))
Base.:*(fstr::FortranString{T}, str::Union{AbstractChar, AbstractString}) where {T} =
    FortranString{T}(vcat(fstr.data, FortranString{T}(str).data))

#"""
#    `str1 // str2`
#Concatenate strings in FORTRAN, alias for `*`
#"""
#Base.://(fstr1::FortranString{T1}, fstr2::FortranString{T2}) where {T1} where {T2} = fstr1 * fstr2
#Base.://(str::Union{AbstractChar, AbstractString}, fstr::FortranString{T}) where {T} = str * fstr
#Base.://(fstr::FortranString{T}, str::Union{AbstractChar, AbstractString}) where {T} = fstr * str
## is it type piracy?
#Base.://(str1::Union{AbstractChar, AbstractString}, str2::Union{AbstractChar, AbstractString}) = str1 * str2

@inline Base.length(fstr::FortranString{T}) where {T} = length(fstr.data)
@inline Base.ncodeunits(fstr::FortranString{T}) where {T} = length(fstr)
@inline Base.isvalid(fstr::FortranString{T}, i::Integer) where {T} = 1 <= i <= length(fstr)

@inline Base.iterate(fstr::FortranString{T}) where {T} = iterate(fstr.data)
@inline Base.iterate(fstr::FortranString{T}, state::Integer) where {T} = iterate(fstr.data, state)

@inline Base.setindex!(fstr::FortranString{T}, c, i::Integer) where {T} = setindex!(fstr.data, T(c), i)
@inline Base.getindex(fstr::FortranString{T}, i::Integer) where {T} = fstr.data[i]
@inline Base.getindex(fstr::FortranString{T}, r::AbstractRange{K}) where {T, K} = FortranString{T}(fstr.data[r])

@inline Base.ndims(::FortranString{T}) where {T} = 1
@inline Base.ndims(::Type{FortranString{T}}) where {T} = 1
@inline Base.size(fstr::FortranString{T}) where {T} = size(fstr.data)
@inline Base.axes(fstr::FortranString{T}) where {T} = axes(fstr.data)
@inline Base.eltype(fstr::FortranString{T}) where {T} = T
Base.similar(fstr::FortranString{T}) where {T} = FortranString{T}(undef, length(fstr))

struct FortranStringStyle <: AbstractArrayStyle{1} end
Base.Broadcast.BroadcastStyle(::Type{FortranString{T}}) where {T} = FortranStringStyle()
#Base.Broadcast.BroadcastStyle(::FortranStringStyle, ::DefaultArrayStyle{1}) = FortranStringStyle()
Base.Broadcast.BroadcastStyle(::DefaultArrayStyle{1}, ::FortranStringStyle) = FortranStringStyle()
Base.Broadcast.BroadcastStyle(::FortranStringStyle, ::FortranStringStyle) = FortranStringStyle()

function Base.Broadcast.instantiate(bc::Broadcasted{FortranStringStyle})
    if bc.axes isa Nothing
        axes = Broadcast.combine_axes(bc.args...)
    else
        axes = bc.axes
        # FortranString is flexible in any direction thus any sizes are allowed
        #check_broadcast_axes(axes, bc.args...)
    end
    return Broadcasted{FortranStringStyle}(bc.f, bc.args, axes)
end
Base.similar(::Broadcasted{FortranStringStyle}, ::Type{ElType}, dims) where {ElType} =
    similar(FortranString{ElType}, dims)
Base.similar(::Broadcasted{FortranStringStyle}, ::Type{Bool}, dims) =
    similar(BitArray, dims)


#function Base.copyto!(dest::FortranString{T}, bc::Broadcasted{FortranStringStyle, <:Any, typeof(identity)}) where {T}
#function Base.copyto!(dest::Union{AbstractVector{T},FortranString{T}}, bc::Broadcasted{FortranStringStyle}) where {T}
function Base.copyto!(dest::AbstractVector{T}, bc::Broadcasted{FortranStringStyle}) where {T}
    @debug dest, bc
    (lendest = length(dest)) == 0 && return dest
    (src,) = bc.args
    lensrc = length(src)
    for (i,v) = enumerate(src)
        dest[i] = T(v)
        i == lendest && break
    end
    for i = lensrc+1:lendest
        dest[i] = T(' ')
    end
    return dest
end

function Base.show(io::IO, ::MIME"text/plain", fstr::FortranString{T}) where {T}
    show(io, fstr)
    return
end
function Base.show(io::IO, fstr::FortranString{T}) where {T}
    if T === Char
        print(io, 'F')
    elseif T === UInt8
        print(io, 'F', '8')
    else
        print(io, 'F', T)
    end
    show(io, string(fstr))
    return
end
Base.print(io::IO, fstr::FortranString) = print(io, string(fstr))
Base.textwidth(fstr::FortranString) = textwidth(string(fstr))

Base.strip(fstr::FortranString{T}) where {T} = lstrip(rstrip(fstr))

Base.lstrip(fstr::FortranString{T}) where {T} =
    FortranString{T}(fstr.data[findfirst(c->!isspace(Char(c))&&!iszero(c), fstr):end])

Base.rstrip(fstr::FortranString{T}) where {T} =
    FortranString{T}(fstr.data[1:findlast(c->!isspace(Char(c))&&!iszero(c), fstr)])

function Base.repeat(fstr::FortranString{T}, n::Integer) where {T}
    l = length(fstr)
    r = FortranString{T}(undef, n*l)
    for i = 0:n-1
        r[1+i*l:l+i*l] .= fstr
    end
    return r
end
REPEAT(str::Union{AbstractChar,AbstractString,FortranString}, n::Integer) = repeat(str, n)

"""
    LEN(s::FortranString{T})

FORTRAN compatible name alias for `length()`.
"""
LEN(fstr::FortranString{T}) where {T} = length(fstr)
LEN(str::AbstractString) = length(str)

"""
    LEN_TRIM(s::FortranString{T})

Returns the length of the `FortranString{T}` `s` with trailing spaces ignored.
"""
LEN_TRIM(fstr::FortranString{T}) where {T} = findlast(c->!isspace(Char(c))&&!iszero(c), fstr)
LEN_TRIM(str::AbstractString) = findlast(c->!isspace(Char(c))&&!iszero(c), str)
LNBLNK(fstr::FortranString{T}) where {T} = LEN_TRIM(fstr)
LNBLNK(fstr::AbstractString) = LEN_TRIM(str)

"""
    TRIM(s::FortranString{T})

Returns a truncated copy of the `FortranString{T}` `s` where all trailing
spaces are removed.
"""
TRIM(fstr::FortranString{T}) where {T} = rstrip(fstr)
TRIM(str::AbstractString) = rstrip(str)

"""
    LSAME(s1, s2)

Standard BLAS function to compare first character of the strings.
Returns `bool`.
"""
LSAME(s1, s2) = uppercase(Char(first(s1))) == uppercase(Char(first(s2)))

"""
    F"some Char string"

String macro for FORTRAN strings based on `Char`
"""
macro F_str(str) ; FortranString{Char}(str) ; end

"""
    F8"some UInt8 string"

String macro for FORTRAN compatible strings based on `UInt8`
"""
macro F8_str(str) ; FortranString{UInt8}(str) ; end


end # of module FortranStrings
