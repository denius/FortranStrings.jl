using FortranStrings, Test

@testset "FortranString{T} creating         " begin

    @testset "FortranString{UInt8} " begin

        s = FortranStrings.FortranString{UInt8}("abc")

        @test length(s) == 3
        @test s         == F8"abc"
        @test s         == F"abc"
        @test s         == "abc"
        @test F"abc"    == s
        @test F8"abc"   == s
        @test "abc"     == s
        @test s         == FortranStrings.FortranString{UInt8}(UInt8[0x61, 0x62, 0x63])
        @test string(s) == "abc"
        @test String(s) == "abc"
        @test s[1:1]    == 'a'
        @test s[2:2]    == 'b'
        @test s[3:3]    == 'c'
        @test s.data    == UInt8[0x61, 0x62, 0x63]

        io = IOBuffer()
        show(io, s)
        @test String(take!(io)) == "F8\"abc\""

        show(io, typeof(s))
        @test String(take!(io)) == "FortranString{UInt8}"
    end

    @testset "FortranString{Char}  " begin

        s = FortranStrings.FortranString{Char}("abc")

        @test length(s) == 3
        @test s         == F8"abc"
        @test s         == F"abc"
        @test s         == "abc"
        @test F"abc"    == s
        @test F8"abc"   == s
        @test "abc"     == s
        @test s         == FortranStrings.FortranString{Char}(['a', 'b', 'c'])
        @test string(s) == "abc"
        @test String(s) == "abc"
        @test s[1:1]    == 'a'
        @test s[2:2]    == 'b'
        @test s[3:3]    == 'c'
        @test s.data    == ['a', 'b', 'c']

        io = IOBuffer()
        show(io, s)
        @test String(take!(io)) == "F\"abc\""

        show(io, typeof(s))
        @test String(take!(io)) == "FortranString{Char}"
    end

    @testset "FortranString{UInt64}" begin

        s = FortranStrings.FortranString{UInt64}("abc")

        @test length(s) == 3
        @test s         == F8"abc"
        @test s         == F"abc"
        @test s         == "abc"
        @test F"abc"    == s
        @test F8"abc"   == s
        @test "abc"     == s
        @test s         == FortranStrings.FortranString{UInt64}(UInt64[0x0061, 0x0062, 0x0063])
        @test string(s) == "abc"
        @test_throws MethodError String(s) == "abc" # String(<:AbstractVector{UInt64}) not exist
        @test s[1:1]    == 'a'
        @test s[2:2]    == 'b'
        @test s[3:3]    == 'c'
        @test s.data    == UInt64[0x0061, 0x0062, 0x0063]

        io = IOBuffer()
        show(io, s)
        @test String(take!(io)) == "FUInt64\"abc\""

        show(io, typeof(s))
        @test String(take!(io)) == "FortranString{UInt64}"
    end

end

@testset "FortranString{T} broadcasting     " begin

    for T in (Char, UInt8, UInt64)
        @eval S = FortranString{$T}

        @eval begin
            #
            @test (incr(x)=x+0x01 ; r = incr.($S("abc"))               ; (r, typeof(r))) == ($S("bcd"), $S)
            @test (incr(x)=x+0x01 ; r = incr.(incr.($S("abc")))        ; (r, typeof(r))) == ($S("cde"), $S)
            @test (incr(x)=x+0x01 ; r = incr.(incr.(incr.($S("abc")))) ; (r, typeof(r))) == ($S("def"), $S)
            @test_throws DimensionMismatch $S("abc") .== $S("ab")
            @test_throws DimensionMismatch $S("abc") .== $S("abcd")
            @test (r = $S("abc").==$S("abc")          ;  r) == BitVector([1,1,1])
            @test (r = $S("abc").==$S("abd")          ;  r) == BitVector([1,1,0])
            @test (r = $S("abc").==$S("abd").==$S("cbd").==$S("cba");  r) == BitVector([0,1,0])
            @test (r = $S("abc").==$S("abd").==$S("cbd").=="cba".==$S("cba");  r) == BitVector([0,1,0])
            $T != Char && @test (r = $S("abc").+$S("ABC").-$S("abc"); (r, typeof(r))) == ($S("ABC"), $S)
            @test (r = $S("abc").+$S("ABC").-$S("abc");  r) == $S("ABC")
            @test (r = $S("abc").+   "ABC" .-$S("abc");  r) == $S("ABC")
            @test (r = $S("abc").+   "A"   .-$S("abc");  r) == $S("AAA")
            @test (r = $S("abc").+   'A'   .-$S("abc");  r) == $S("AAA")
            @test (r = $S("abc").+   0x41  .-$S("abc");  r) == $S("AAA")
            @test (r = $S("abc").+   65    .-$S("abc");  r) == $S("AAA")

            if $T != Char
                s1 = $S(collect(1+1:6+1))
                s2 = $S(collect(1+2:6+2))
                s3 = $S(collect(1+3:6+3))
                s4 = $S(collect(1+4:6+4))
                s5 = $S(collect(1+5:6+5))
                s6 = $S(collect(1+6:6+6))
                s7 = $S(collect(1+7:6+7))
                s8 = $S(collect(1+8:6+8))
                @test (s1.+s2)                         == (s1+s2)
                @test (s1.+s2.+s3)                     == (s1+s2+s3)
                @test (s1.+s2.+s3.+s4)                 == (s1+s2+s3+s4)
                @test (s1.+s2.+s3.+s4.+s5)             == (s1+s2+s3+s4+s5)
                @test (s1.+s2.+s3.+s4.+s5.+s6)         == (s1+s2+s3+s4+s5+s6)
                @test (s1.+s2.+s3.+s4.+s5.+s6.+s7)     == (s1+s2+s3+s4+s5+s6+s7)
                @test (s1.+s2.+s3.+s4.+s5.+s6.+s7.+s8) == (s1+s2+s3+s4+s5+s6+s7+s8)
            end
        end

        for STR in (String, FortranString{Char}, FortranString{UInt8}, FortranString{UInt64})
            @eval begin
                @test (s = $S("abc"); s      .= $STR("")               ; (s, typeof(s))) == ($S("   "), $S)
                @test (s = $S("abc"); s[1:3] .= $STR("")               ; (s, typeof(s))) == ($S("   "), $S)
                @test (s = $S("abc"); s[1:2] .= $STR("")               ; (s, typeof(s))) == ($S("  c"), $S)
                @test (s = $S("abc"); s[2:2] .= $STR("")               ; (s, typeof(s))) == ($S("a c"), $S)
                @test (s = $S("abc"); s      .= $STR("1")              ; (s, typeof(s))) == ($S("1  "), $S)
                @test (s = $S("abc"); s[2:3] .= $STR("1")              ; (s, typeof(s))) == ($S("a1 "), $S)
                @test (s = $S("abc"); s[1:3] .= $STR("1")              ; (s, typeof(s))) == ($S("1  "), $S)
                @test (s = $S("abc"); s      .= $STR("123")            ; (s, typeof(s))) == ($S("123"), $S)
                @test (s = $S("abc"); s      .= $STR("123456789")      ; (s, typeof(s))) == ($S("123"), $S)
                @test (s = $S("abc"); s[1:3] .= $STR("123")            ; (s, typeof(s))) == ($S("123"), $S)
                @test (s = $S("abc"); s[1:2] .= $STR("123")            ; (s, typeof(s))) == ($S("12c"), $S)
                @test (s = $S("abc"); s[2:2] .= $STR("123")            ; (s, typeof(s))) == ($S("a1c"), $S)
                @test (s = $S("abc"); s[2:3] .= $STR("123")            ; (s, typeof(s))) == ($S("a12"), $S)
                @test (s = $S("abc"); s      .= $STR("1")[1:1]         ; (s, typeof(s))) == ($S("1  "), $S)
                @test (s = $S("abc"); s[2:3] .= $STR("1")              ; (s, typeof(s))) == ($S("a1 "), $S)
                @test (s = $S("abc"); s[1:3] .= $STR("1")[1:1]         ; (s, typeof(s))) == ($S("1  "), $S)
                @test (s = $S("abc"); s      .= $STR("123")[1:1]       ; (s, typeof(s))) == ($S("1  "), $S)
                @test (s = $S("abc"); s      .= $STR("123456789")[5:7] ; (s, typeof(s))) == ($S("567"), $S)
                @test (s = $S("abc"); s[2:3] .= $STR("123456789")[5:7] ; (s, typeof(s))) == ($S("a56"), $S)
                @test (s = $S("abc"); s[1:3] .= $STR("123")[2:3]       ; (s, typeof(s))) == ($S("23 "), $S)
                @test (s = $S("abc"); s[1:2] .= $STR("123")[2:3]       ; (s, typeof(s))) == ($S("23c"), $S)
                @test (s = $S("abc"); s[2:2] .= $STR("123")[2:3]       ; (s, typeof(s))) == ($S("a2c"), $S)
                @test (s = $S("abc"); s[2:3] .= $STR("123")[2:3]       ; (s, typeof(s))) == ($S("a23"), $S)
            end
        end
    end

end

@testset "Some functions on FortranString{T}" begin

    @testset "concatenating    " begin

        s8 = F8"abc"
        s  = F"123"

        @test "abc" * s8 * "abc"             == F8"abcabcabc"
        @test '1' * s8 * '3'                 == F8"1abc3"
        @test typeof("abc" * s8 * "abc")     == FortranString{UInt8}
        @test typeof("abc" * s * "abc")      == FortranString{Char}
        @test typeof("abc" * s * s8 * "abc") == FortranString{Char}
        @test typeof("abc" * s8 * s * "abc") == FortranString{UInt8}

    end


    @testset "assigning        " begin

        s = F8"abc"

        @test (s[2] =  '2'; s) == F8"a2c"
        @test (s[3] = 0x20; s) == F8"a2 "
        @test (s[3] = 0   ; s) == FortranStrings.FortranString{UInt8}(UInt8[0x61, 0x32, 0x00])

    end


    @testset "vector functions " begin

        s = F8"abc"

        for T in (Char, UInt8, UInt64)
            @eval S = FortranString{$T}
            @eval begin
                s = $S("abc")
                @test map(Char, s) == ['a', 'b', 'c']
                @test collect(s)   == $T['a', 'b', 'c']
            end
        end
    end


    @testset "Fortran functions" begin

        for S in (String, FortranString{Char}, FortranString{UInt8}, FortranString{UInt64})
            @eval begin
                s = $S("     ")
                @test LEN(s)               == 5
                @test LEN_TRIM(s)          == 0
                @test LNBLNK(s)            == 0
                @test TRIM(s)              == $S("")
                @test isa(TRIM(s), $S==String ? AbstractString : $S)
                @test strip(s)             == $S("")
                @test isa(strip(s), $S==String ? AbstractString : $S)

                s = $S(" abc ")
                @test LEN(s)               == 5
                @test LEN_TRIM(s)          == 4
                @test LNBLNK(s)            == 4
                @test TRIM(s)              == $S(" abc")
                @test isa(TRIM(s), $S==String ? AbstractString : $S)
                @test strip(s)             == $S("abc")
                @test isa(strip(s), $S==String ? AbstractString : $S)

                @test REPEAT(s, 2)         == $S(" abc  abc ")
                @test isa(REPEAT(s, 2), $S)

                @test (r = uppercase($S("abc")); (r, typeof(r))) == ($S("ABC"), $S)
                @test (r = lowercase($S("ABC")); (r, typeof(r))) == ($S("abc"), $S)

                s = $S(" abc  abc ")
                $S==String || @test split(s, $S("c"))    == [$S(" ab"), $S("  ab"), $S(" ")]
                $S==String || @test split(s, r"c")       == [$S(" ab"), $S("  ab"), $S(" ")]
                @test occursin($S("c"), s) == true
                @test occursin($S("d"), s) == false
                @test (m = match(r"(c)", s); length(m.captures)) == 1
                @test (m = match(r"(d)", s); m===nothing)        == true

                @test INDEX($S("12341234"), $("23")           ) == 2
                @test INDEX($S("12341234"), $("23"), true     ) == 6
                @test INDEX($S("12341234"), $("23"), back=true) == 6
                @test INDEX($S("12341234"), $("00")           ) == 0
                @test INDEX($S("12341234"), $("00"), true     ) == 0

                @test SCAN($S("12341234"), $("23")            ) == 2
                @test SCAN($S("12341234"), $("23"), true      ) == 7
                @test SCAN($S("12341234"), $("23"), back=true ) == 7
                @test SCAN($S("12341234"), $("ab")            ) == 0
                @test SCAN($S("12341234"), $("ab"), true      ) == 0

            end
        end

        # check trim spaces but not '\0'
        s = F8"abc"
        @test (s[3] = 0x20; s) == F8"ab "
        @test LEN(s)           == 3
        @test LEN_TRIM(s)      == 2
        @test (s[3] = 0   ; s) == FortranStrings.FortranString{UInt8}(UInt8[0x61, 0x62, 0x00])
        @test LEN(s)           == 3
        @test LEN_TRIM(s)      == 3

        @test LSAME(F8"TRI", F8"tre") == true
        @test LSAME(F8"TRI", F"tre")  == true
        @test LSAME(F"TRI", F8"tre")  == true
        @test LSAME("TRI", F8"tre")   == true
        @test LSAME(F8"TRI", "tre")   == true
        @test LSAME(F8"TRI", 't')     == true
        @test LSAME(F8"TRI", F"upp")  == false
        @test LSAME(F"TRI", F8"upp")  == false
        @test LSAME(F8"TRI", 'u')     == false

    end

end

