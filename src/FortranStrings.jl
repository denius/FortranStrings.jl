module FortranStrings

export FortranString, @F_str, @F8_str

"""
    FortranString{CharType}
Datatype for FORTRAN CHARACTER*N strings emulation.
The type parameter `CharType` specifies the type of elements in strings.
"""
mutable struct FortranString{CharType} <: AbstractString
    data :: Array{CharType, 1}
end

(::Type{FortranString{T}})(str::AbstractString) where {T} = FortranString{T}(map(T, collect(str)))

Base.convert(::Type{FortranString{T}}, str::AbstractString) where {T} = FortranString{T}(str)
Base.convert(::Type{String}, fstr::FortranString{T}) where {T} = join(map(Char, fstr.data))

# is it need?
Base.promote_rule(::Type{Union{AbstractChar, AbstractString}}, ::Type{FortranString{T}}) where {T} = FortranString{T}
Base.promote_rule(::Type{FortranString{T}}, ::Type{Union{AbstractChar, AbstractString}}) where {T} = FortranString{T}

Base.string(fstr::FortranString{T}) where {T} = convert(String, fstr)

Base.:*(fstr1::FortranString{T1}, fstr2::FortranString{T2}) where {T1} where {T2}=
    FortranString{T1}(vcat(fstr1.data, map(T1, fstr2.data)))
Base.:*(str::Union{AbstractChar, AbstractString}, fstr::FortranString{T}) where {T} =
    FortranString{T}(vcat(FortranString{T}(str).data, fstr.data))
Base.:*(fstr::FortranString{T}, str::Union{AbstractChar, AbstractString}) where {T} =
    FortranString{T}(vcat(fstr.data, FortranString{T}(str).data))

Base.length(fstr::FortranString{T}) where {T} = length(fstr.data)
Base.ncodeunits(fstr::FortranString{T}) where {T} = length(fstr)
Base.isvalid(fstr::FortranString{T}, i::Integer) where {T} = 1 <= i <= length(fstr)

Base.iterate(fstr::FortranString{T}) where {T} = length(fstr) > 0 ? (fstr.data[1], 1) : nothing
Base.iterate(fstr::FortranString{T}, i::Integer) where {T} =
    i < length(fstr) ? (fstr.data[i+1], i+1) : nothing

Base.setindex!(fstr::FortranString{T}, c, i::Integer) where {T} = setindex!(fstr.data, T(c), i)
Base.getindex(fstr::FortranString{T}, i::Integer) where {T} = fstr.data[i]

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
Base.print(io::IO, s::FortranString) = print(io, string(s))
Base.textwidth(s::FortranString) = textwidth(string(s))

"""
String macro for FORTRAN strings based on `Char`
"""
macro F_str(str) ; FortranString{Char}(str) ; end
"""
String macro for FORTRAN compatible strings based on `UInt8`
"""
macro F8_str(str) ; FortranString{UInt8}(str) ; end


end # of module FortranStrings
