defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "integer literals" do
    test "in decimal base" do
      assert_same "0"
      assert_same "100"
      assert_same "007"
      assert_same "10000"
      assert_format "100000", "100_000"
      assert_format "1000000", "1_000_000"
    end

    test "in binary base" do
      assert_same "0b0"
      assert_same "0b1"
      assert_same "0b101"
      assert_same "0b01"
      assert_format "0b111_111", "0b111111"
    end

    test "in octal base" do
      assert_same "0o77"
      assert_same "0o0"
      assert_same "0o01"
      assert_format "0o777_777", "0o777777"
    end

    test "in hex base" do
      assert_same "0x1"
      assert_format "0xabcdef", "0xABCDEF"
      assert_same "0x01"
      assert_format "0xFFF_FFF", "0xFFFFFF"
    end

    test "as chars" do
      assert_same "?a"
      assert_same "?1"
      assert_same "?è"
      assert_same "??"
      assert_same "?\\\\"
      assert_same "?\\s"
      assert_same "?🎾"
    end
  end

  describe "float literals" do
    test "with normal notation" do
      assert_same "0.0"
      assert_same "1.0"
      assert_same "123.456"
      assert_same "0.0000001"
      assert_same "001.100"
      assert_format "0_10000_0.000_000", "0_100_000.000000"
    end

    test "with scientific notation" do
      assert_same "1.0e1"
      assert_same "1.0e-1"
      assert_same "1.0e01"
      assert_same "1.0e-01"
      assert_same "001.100e-010"
      assert_format "0_1_00_0_000.100e-010", "01_000_000.100e-010"

      assert_format "1.0E01", "1.0e01"
      assert_format "1.0E-01", "1.0e-01"
    end
  end

  describe "aliases" do
    test "with atom-only parts" do
      assert_same "Elixir"
      assert_same "Elixir.Foo"
      assert_same "Foo.Bar.Baz"
    end

    test "removes spaces between aliases" do
      assert_format "Foo . Bar . Baz", "Foo.Bar.Baz"
    end

    test "starting with expression" do
      assert_same "__MODULE__.Foo.Bar"
      assert_same "'Foo'.Bar.Baz" # Syntatically valid, semantically invalid
    end

    test "wraps the head in parens if it has an operator" do
      assert_format "+(Foo . Bar . Baz)", "+Foo.Bar.Baz"
      assert_format "(+Foo) . Bar . Baz", "(+Foo).Bar.Baz"
    end
  end

  describe "atom literals" do
    test "true, false, nil" do
      assert_same "nil"
      assert_same "true"
      assert_same "false"
    end

    test "without escapes" do
      assert_same ~S[:foo]
    end

    test "with escapes" do
      assert_same ~S[:"f\a\b\ro"]
      assert_format ~S[:'f\a\b\ro'], ~S[:"f\a\b\ro"]
      assert_format ~S[:'single \' quote'], ~S[:"single ' quote"]
      assert_format ~S[:"double \" quote"], ~S[:"double \" quote"]
    end

    test "with unicode" do
      assert_same ~S[:ólá]
    end

    test "does not reformat aliases" do
      assert_same ~S[:"Elixir.String"]
    end

    test "removes quotes when they are not necessary" do
      assert_format ~S[:"foo"], ~S[:foo]
      assert_format ~S[:"++"], ~S[:++]
    end

    test "uses double quotes even when single quotes are used" do
      assert_format ~S[:'foo bar'], ~S[:"foo bar"]
    end

    test "with interpolation" do
      assert_same ~S[:"one #{2} three"]
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      :"one #{"two"} three"
      """

      good = ~S"""
      :"one #{
        "two"
      } three"
      """

      assert_format bad, good, @short_length
    end
  end

  describe "string literals" do
    test "without escapes" do
      assert_same ~S["foo"]
    end

    test "with escapes" do
      assert_same ~S["f\a\b\ro"]
      assert_same ~S["double \" quote"]
    end

    test "keeps literal new lines" do
      assert_same """
      "fo
      o"
      """
    end

    test "with interpolation" do
      assert_same ~S["one #{2} three"]
      # Interpolation uses block context
      assert_format ~S["one #{@two(three)}"], ~S["one #{@two three}"]
    end

    test "with escapes and interpolation" do
      assert_same ~S["one\n\"#{2}\"\nthree"]
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      "one #{"two"} three"
      """

      good = ~S"""
      "one #{
        "two"
      } three"
      """

      assert_format bad, good, @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
      "one
      #{"two"}
      three"
      """, @short_length
    end
  end

  describe "charlist literals" do
    test "without escapes" do
      assert_same ~S['']
      assert_same ~S[' ']
      assert_same ~S['foo']
    end

    test "with escapes" do
      assert_same ~S['f\a\b\ro']
      assert_same ~S['single \' quote']
    end

    test "keeps literal new lines" do
      assert_same """
      'fo
      o'
      """
    end

    test "with interpolation" do
      assert_same ~S['one #{2} three']
    end

    test "with escape and interpolation" do
      assert_same ~S['one\n\'#{2}\'\nthree']
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      'one #{"two"} three'
      """

      good = ~S"""
      'one #{
        "two"
      } three'
      """

      assert_format bad, good, @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
      'one
      #{"two"}
      three'
      """, @short_length
    end
  end

  describe "string heredocs" do
    test "without escapes" do
      assert_same to_string(~S'''
      """
      hello
      """
      ''')
    end

    test "with escapes" do
      assert_same to_string(~S'''
      """
      f\a\b\ro
      """
      ''')

      assert_same to_string(~S'''
      """
      multiple "\"" quotes
      """
      ''')
    end

    test "with interpolation" do
      assert_same to_string(~S'''
      """
      one
      #{2}
      three
      """
      ''')

      assert_same to_string(~S'''
      """
      one
      "
      #{2}
      "
      three
      """
      ''')
    end

    test "with interpolation on line limit" do
      bad = to_string(~S'''
      """
      one #{"two two"} three
      """
      ''')

      good = to_string(~S'''
      """
      one #{
        "two two"
      } three
      """
      ''')

      assert_format bad, good, @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same to_string(~S'''
      """
      one
      #{"two two"}
      three
      """
      '''), @short_length
    end
  end

  describe "charlist heredocs" do
    test "without escapes" do
      assert_same ~S"""
      '''
      hello
      '''
      """
    end

    test "with escapes" do
      assert_same ~S"""
      '''
      f\a\b\ro
      '''
      """

      assert_same ~S"""
      '''
      multiple "\"" quotes
      '''
      """
    end

    test "with interpolation" do
      assert_same ~S"""
      '''
      one
      #{2}
      three
      '''
      """

      assert_same ~S"""
      '''
      one
      "
      #{2}
      "
      three
      '''
      """
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      '''
      one #{"two two"} three
      '''
      """

      good = ~S"""
      '''
      one #{
        "two two"
      } three
      '''
      """

      assert_format bad, good, @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
      '''
      one
      #{"two two"}
      three
      '''
      """, @short_length
    end
  end

  describe "sigils" do
    test "without interpolation" do
      assert_same ~S[~s(foo)]
      assert_same ~S[~s{foo bar}]
      assert_same ~S[~r/Bar Baz/]
      assert_same ~S[~w<>]
      assert_same ~S[~W()]
    end

    test "with escapes" do
      assert_same ~S[~s(foo \) bar)]
      assert_same ~S[~s(f\a\b\ro)]
    end

    test "with interpolation" do
      assert_same ~S[~s(one #{2} three)]
    end

    test "with modifiers" do
      assert_same ~S[~w(one two three)a]
      assert_same ~S[~z(one two three)foo]
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      ~s(one #{"two"} three)
      """
      good = ~S"""
      ~s(one #{
        "two"
      } three)
      """
      assert_format bad, good, @short_length
    end

    test "with heredoc syntax" do
      assert_same ~S"""
      ~s'''
      one\a
      #{:two}\r
      three\0
      '''
      """

      assert_same ~S'''
      ~s"""
      one\a
      #{:two}\r
      three\0
      """
      '''
    end

    test "with heredoc syntax and interpolation on line limit" do
      bad = ~S"""
      ~s'''
      one #{"two two"} three
      '''
      """

      good = ~S"""
      ~s'''
      one #{
        "two two"
      } three
      '''
      """

      assert_format bad, good, @short_length
    end
  end

  describe "bitstrings" do
    test "without arguments" do
      assert_format "<< >>", "<<>>"
      assert_format "<<\n>>", "<<>>"
    end

    test "with arguments" do
      assert_format "<<1,2,3>>", "<<1, 2, 3>>"
    end

    test "add parens on first and last in case of ambiguity" do
      assert_format "<< <<>>, <<>>, <<>> >>", "<<(<<>>), <<>>, (<<>>)>>"
      assert_format "<< <<>>::1, <<>>::2, <<>>::3 >>", "<<(<<>>)::1, <<>>::2, <<>>::3>>"
    end

    test "with modifiers" do
      assert_format "<< 1 :: 1 >>", "<<1::1>>"
      assert_format "<< 1 :: 2 + 3 >>", "<<1::(2 + 3)>>"
      assert_format "<< 1 :: 2 - integer >>", "<<1::2-integer>>"
      assert_format "<< 1 :: 2 - unit(3) >>", "<<1::2-unit(3)>>"
      assert_format "<< 1 :: 2 - unit(3) - 4 * 5 >>", "<<1::2-unit(3)-(4 * 5)>>"
    end

    test "is flex on line limits" do
      assert_format "<<1, 2, 3, 4>>", "<<1, 2, 3,\n  4>>", @short_length
      assert_format "<<1::1, 2::bits, 3::4-binary>>", "<<1::1, 2::bits,\n  3::4-binary>>", @medium_length
    end
  end

  describe "lists" do
    test "empty" do
      assert_format "[ ]", "[]"
      assert_format "[\n]", "[]"
    end

    test "with elements" do
      assert_format "[ 1 , 2,3, 4 ]", "[1, 2, 3, 4]"
    end

    test "are flex on line limit" do
      bad = """
      [11, 22, 33, 44]
      """
      good = """
      [11, 22,
       33, 44]
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "[1,]", "[1]"
      assert_format "[1, 2, 3,]", "[1, 2, 3]"
    end
  end

  describe "unary operators" do
    test "formats symbol operators without spaces" do
      assert_format "+ 1", "+1"
      assert_format "- 1", "-1"
      assert_format "! 1", "!1"
      assert_format "^ 1", "^1"
      assert_format "~~~ 1", "~~~1"
    end

    test "formats word operators with spaces" do
      assert_same "not 1"
      assert_same "not true"
    end

    test "wraps operand if it is a unary or binary operator" do
      assert_format "!+1", "!(+1)"
      assert_format "+ +1", "+(+1)"
      assert_format "not +1", "not(+1)"
      assert_format "not !1", "not(!1)"
      assert_format "!not 1", "!(not 1)"
    end

    test "does not wrap operand if it is a nestable operator" do
      assert_format "! ! var", "!!var"
      assert_same "not not var"
    end

    test "nests operand according to operator length" do
      bad = "not foo(bar, baz, bat)"
      good = """
      not foo(bar,
              baz,
              bat)
      """
      assert_format bad, good, @short_length

      bad = "~~~ foo(bar, baz, bat)"
      good = """
      ~~~foo(bar,
             baz,
             bat)
      """
      assert_format bad, good, @short_length
    end
  end

  describe "local calls" do
    test "without arguments" do
      assert_format "foo( )", "foo()"
    end

    test "without arguments doesn't split on line limit" do
      assert_same "very_long_function_name()", @short_length
    end

    test "removes outer parens except for unquote_splicing/1" do
      assert_format "(foo())", "foo()"
      assert_same "(unquote_splicing(123))"
    end

    test "with arguments" do
      assert_format "foo( :one ,:two,\n   :three)", "foo(:one, :two, :three)"
    end

    test "with arguments splits on line limit" do
      bad = """
      fun(x, y, z)
      """
      good = """
      fun(x, y,
          z)
      """
      assert_format bad, good, @short_length
    end
  end

  describe "binary operators without space" do
    test "formats without spaces" do
      assert_format "1 .. 2", "1..2"
    end

    test "never breaks" do
      assert_same "123_456_789..987_654_321", @short_length
    end
  end

  describe "binary operators without newline" do
    test "formats without spaces" do
      assert_same "1 in 2"
      assert_format "1\\\\2", "1 \\\\ 2"
    end

    test "never breaks" do
      assert_same "123_456_789 in 987_654_321", @short_length
    end
  end

  describe "binary operators with preceding new line" do
    test "formats with spaces" do
      assert_format "1|>2", "1 |> 2"
    end

    test "breaks into new line with left precedence" do
      bad = "123_456_789 |> 987_654_321"
      good = """
      123_456_789
      |> 987_654_321
      """
      assert_format bad, good, @short_length

      bad = "123 |> foo(bar)"
      good = """
      123
      |> foo(bar)
      """
      assert_format bad, good, @short_length

      bad = "123 |> foo(bar, baz)"
      good = """
      123
      |> foo(bar,
             baz)
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, 123 |> bar(baz))"
      good = """
      foo(bar,
          123
          |> bar(baz))
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) |> 123"
      good = """
      foo(bar,
          baz)
      |> 123
      """
      assert_format bad, good, @short_length
    end

    test "breaks into new line with right precedence" do
      bad = "123_456_789 | 987_654_321"
      good = """
      123_456_789
      | 987_654_321
      """
      assert_format bad, good, @short_length

      bad = "123 | foo(bar)"
      good = """
      123
      | foo(bar)
      """
      assert_format bad, good, @short_length

      bad = "123 | foo(bar, baz)"
      good = """
      123
      | foo(bar,
            baz)
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, 123 | bar(baz))"
      good = """
      foo(bar,
          123
          | bar(baz))
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) | 123"
      good = """
      foo(bar,
          baz)
      | 123
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) | 123 | 456"
      good = """
      foo(bar,
          baz)
      | 123
      | 456
      """
      assert_format bad, good, @short_length

      bad = "123 | foo(bar, baz) | 456"
      good = """
      123
      | foo(bar,
            baz)
      | 456
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the same entry and left precedence" do
      assert_same "foo |> bar |> baz"

      bad = "foo |> bar |> baz"
      good = """
      foo
      |> bar
      |> baz
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the same entry and right precedence" do
      assert_same "foo when bar when baz"

      bad = "foo when bar when baz"
      good = """
      foo
      when bar
      when baz
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the different entry and same precedence" do
      assert_same "foo <|> bar ~> baz"

      bad = "foo <|> bar ~> baz"
      good = """
      foo
      <|> bar
      ~> baz
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the different entry and different precedence" do
      assert_same "foo when bar | baz"

      bad = "foo when bar | baz"
      good = """
      foo
      when bar
           | baz
      """
      assert_format bad, good, @short_length
    end
  end

  describe "binary operators with following new line" do
    test "formats with spaces" do
      assert_format "1++2", "1 ++ 2"
    end

    test "breaks into new line" do
      bad = "123_456_789 ++ 987_654_321"
      good = """
      123_456_789 ++
        987_654_321
      """
      assert_format bad, good, @short_length

      bad = "123 ++ foo(bar)"
      good = """
      123 ++
        foo(bar)
      """
      assert_format bad, good, @short_length

      bad = "123 ++ foo(bar, baz)"
      good = """
      123 ++
        foo(bar,
            baz)
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, 123 ++ bar(baz))"
      good = """
      foo(bar,
          123 ++
            bar(baz))
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) ++ 123"
      good = """
      foo(bar,
          baz) ++
        123
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the same entry" do
      assert_same "foo ++ bar ++ baz"

      bad = "foo ++ bar ++ baz"
      good = """
      foo ++
        bar ++
        baz
      """
      assert_format bad, good, @short_length
    end

    test "with multiple entries on optional parens" do
      assert_format "(a + b) == (c + d)", "a + b == c + d"
      assert_format "a + (b == c) + d", "a + (b == c) + d"

      bad = "(a + b) == (c + d)"
      good = """
      a + b ==
        c + d
      """
      assert_format bad, good, @short_length

      bad = "a * (b + c) * d"
      good = """
      a *
        (b +
           c) *
        d
      """
      assert_format bad, good, @short_length

      bad = "one * (two + three) * four"
      good = """
      one *
        (two + three) *
        four
      """
      assert_format bad, good, @medium_length

      bad = "one * (two + three + four) * five"
      good = """
      one *
        (two + three +
           four) * five
      """
      assert_format bad, good, @medium_length
    end

    test "with multiple entries and required parens" do
      assert_same "(a |> b) ++ (c |> d)"
      assert_format "a + b |> c + d", "(a + b) |> (c + d)"
      assert_format "a ++ b |> c ++ d", "(a ++ b) |> (c ++ d)"
      assert_format "a |> b ++ c |> d", "a |> (b ++ c) |> d"
    end

    test "mixed before and after lines" do
      bad = "a when b and c when d"
      good = """
      a
      when b and
             c
      when d
      """
      assert_format bad, good, @medium_length
    end
  end

  describe "module attributes" do
    test "when reading" do
      assert_format "@ my_attribute", "@my_attribute"
    end

    test "when setting" do
      assert_format "@ my_attribute(:some_value)", "@my_attribute :some_value"
    end

    test "doesn't split when reading on line limit" do
      assert_same "@my_long_attribute", @short_length
    end

    test "doesn't split when setting on line limit" do
      assert_same "@my_long_attribute :some_value", @short_length
    end

    test "is parenthesized when setting inside a call" do
      assert_same "my_fun(@foo(bar), baz)"
    end

    test "fall back to @ as an operator when needed" do
      assert_format "@+1", "@(+1)"
      assert_same "@(1 + 1)"
      assert_same "@:foo"
    end
  end
end
