defmodule CodeFormatter.OperatorsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "unary" do
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
      assert_format "!not 1", "!(not 1)"
      assert_format "not !1", "not(!1)"
      assert_format "not (!1)", "not(!1)"
    end

    test "does not wrap operand if it is a nestable operator" do
      assert_format "! ! var", "!!var"
      assert_same "not not var"
    end

    test "does not nests operand" do
      bad = "not foo(bar, baz, bat)"
      good = """
      not foo(
        bar,
        baz,
        bat
      )
      """
      assert_format bad, good, @short_length

      bad = "~~~ foo(bar, baz, bat)"
      good = """
      ~~~foo(
        bar,
        baz,
        bat
      )
      """
      assert_format bad, good, @short_length
    end

    test "inside do-end block" do
      assert_same """
      if +value do
        true
      end
      """
    end
  end

  describe "binary without space" do
    test "formats without spaces" do
      assert_format "1 .. 2", "1..2"
    end

    test "never breaks" do
      assert_same "123_456_789..987_654_321", @short_length
    end
  end

  describe "binary without newline" do
    test "formats without spaces" do
      assert_same "1 in 2"
      assert_format "1\\\\2", "1 \\\\ 2"
    end

    test "never breaks" do
      assert_same "123_456_789 in 987_654_321", @short_length
    end

    test "not in" do
      assert_format "not(foo in bar)", "foo not in bar"
      assert_same "(not foo) in bar"
      assert_same "(!foo) in bar"
    end
  end

  describe "binary operators with preceding new line" do
    test "formats with spaces" do
      assert_format "1|>2", "1 |> 2"
    end

    test "breaks into new line" do
      bad = "123_456_789 |> 987_654_321"
      good = """
      123_456_789
      |> 987_654_321
      """
      assert_format bad, good, @short_length

      bad = "123 |> foo(bar, baz)"
      good = """
      123
      |> foo(
           bar,
           baz
         )
      """
      assert_format bad, good, @short_length

      bad = "123 |> foo(bar) |> bar(bat)"
      good = """
      123
      |> foo(
           bar
         )
      |> bar(
           bat
         )
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, 123 |> bar(baz))"
      good = """
      foo(
        bar,
        123
        |> bar(
             baz
           )
      )
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) |> 123"
      good = """
      foo(
        bar,
        baz
      )
      |> 123
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) |> 123 |> 456"
      good = """
      foo(
        bar,
        baz
      )
      |> 123
      |> 456
      """
      assert_format bad, good, @short_length

      bad = "123 |> foo(bar, baz) |> 456"
      good = """
      123
      |> foo(
           bar,
           baz
         )
      |> 456
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
  end

  describe "binary with following new line" do
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
        foo(
          bar,
          baz
        )
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, 123 ++ bar(baz))"
      good = """
      foo(
        bar,
        123 ++
          bar(
            baz
          )
      )
      """
      assert_format bad, good, @short_length

      bad = "foo(bar, baz) ++ 123"
      good = """
      foo(
        bar,
        baz
      ) ++ 123
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the same entry and left associative" do
      assert_same "foo == bar == baz"

      bad = "a == b == c"
      good = """
      a == b ==
        c
      """
      assert_format bad, good, @short_length

      bad = "(a == (b == c))"
      good = """
      a ==
        (b == c)
      """
      assert_format bad, good, @short_length

      bad = "foo == bar == baz"
      good = """
      foo == bar ==
        baz
      """
      assert_format bad, good, @short_length

      bad = "(foo == (bar == baz))"
      good = """
      foo ==
        (bar ==
           baz)
      """
      assert_format bad, good, @short_length
    end

    test "with multiple of the same entry and right associative" do
      assert_same "foo ++ bar ++ baz"

      bad = "a ++ b ++ c"
      good = """
      a ++
        b ++ c
      """
      assert_format bad, good, @short_length

      bad = "((a ++ b) ++ c)"
      good = """
      (a ++ b) ++
        c
      """
      assert_format bad, good, @short_length

      bad = "foo ++ bar ++ baz"
      good = """
      foo ++
        bar ++
        baz
      """
      assert_format bad, good, @short_length

      bad = "((foo ++ bar) ++ baz)"
      good = """
      (foo ++
         bar) ++
        baz
      """
      assert_format bad, good, @short_length
    end

    test "with precedence" do
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
        (b + c) *
        d
      """
      assert_format bad, good, @short_length

      bad = "(one + two) == (three + four)"
      good = """
      one + two ==
        three + four
      """
      assert_format bad, good, @medium_length

      bad = "one * (two + three) * four"
      good = """
      one * (two + three) *
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

      bad = "var = one * (two + three + four) * five"
      good = """
      var =
        one *
          (two + three +
             four) * five
      """
      assert_format bad, good, @medium_length
    end

    test "with required parens" do
      assert_same "(a |> b) ++ (c |> d)"
      assert_format "a + b |> c + d", "(a + b) |> (c + d)"
      assert_format "a ++ b |> c ++ d", "(a ++ b) |> (c ++ d)"
      assert_format "a |> b ++ c |> d", "a |> (b ++ c) |> d"
    end

    test "mixed before and after lines" do
      bad = "var :: a | b and c | d"
      good = """
      var ::
        a
        | b and
            c
        | d
      """
      assert_format bad, good, @short_length

      bad = "var :: a | b and c + d + e + f | g"
      good = """
      var ::
        a
        | b and
            c + d + e + f
        | g
      """
      assert_format bad, good, @medium_length
    end
  end

  # Theoretically it fits under operators but the goal of
  # this section is to test common idioms.
  describe "match" do
    test "with calls" do
      bad = "var = fun(one, two, three)"
      good = """
      var =
        fun(
          one,
          two,
          three
        )
      """
      assert_format bad, good, @short_length

      bad = "fun(one, two, three) = var"
      good = """
      fun(
        one,
        two,
        three
      ) = var
      """
      assert_format bad, good, @short_length

      bad = "fun(foo, bar) = fun(baz, bat)"
      good = """
      fun(
        foo,
        bar
      ) =
        fun(
          baz,
          bat
        )
      """
      assert_format bad, good, @short_length

      bad = "fun(foo, bar) = fun(baz, bat)"
      good = """
      fun(foo, bar) =
        fun(baz, bat)
      """
      assert_format bad, good, @medium_length
    end

    test "with containers" do
      bad = "var = {one, two, three}"
      good = """
      var = {
        one,
        two,
        three
      }
      """
      assert_format bad, good, @short_length

      bad = "{one, two, three} = var"
      good = """
      {
        one,
        two,
        three
      } = var
      """
      assert_format bad, good, @short_length

      bad = "{one, two, three} = foo(bar, baz)"
      good = """
      {one, two, three} =
        foo(bar, baz)
      """
      assert_format bad, good, @medium_length
    end

    test "with heredoc" do
      assert_same ~S"""
      var = '''
      one
      '''
      """, @short_length

      assert_same ~S"""
      var = '''
      #{one}
    '''
      """, @short_length
    end

    test "with anonymous functions" do
      bad = "var = fn arg1 -> body1; arg2 -> body2 end"

      good = """
      var = fn
        arg1 ->
          body1
        arg2 ->
          body2
      end
      """
      assert_format bad, good, @short_length

      good = """
      var = fn
        arg1 -> body1
        arg2 -> body2
      end
      """
      assert_format bad, good, @medium_length
    end

    test "with do-end blocks" do
      assert_same """
      var =
        case true do
          foo -> bar
          baz -> bat
        end
      """
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

    test "with do-end block" do
      bad = "@attr for x <- y, do: z"
      good = """
      @attr (for x <- y do
               z
             end)
      """
      assert_format bad, good
    end

    test "is parenthesized when setting inside a call" do
      assert_same "my_fun(@foo(bar), baz)"
    end

    test "fall back to @ as an operator when needed" do
      assert_same "@(1 + 1)"
      assert_same "@:foo"
      assert_same "+@foo"
      assert_same "@@foo"
      assert_same "@(+foo)"
      assert_same "!(@(1 + 1))"
      assert_same "(@Foo).Baz"
      assert_same "@bar(1, 2)"

      assert_format "@+1", "@(+1)"
      assert_format "@Foo.Baz", "(@Foo).Baz"
      assert_format "@(Foo.Bar).Baz", "(@(Foo.Bar)).Baz"
    end
  end

  describe "capture" do
    test "with integers" do
      assert_same "&1"
      assert_format "&(&1)", "& &1"
      assert_format "&(&1.foo)", "& &1.foo()"
    end

    test "with operators inside" do
      assert_format "&(+1)", "&+1"
      assert_format "&(a ++ b)", "& a ++ b"
      assert_format "&(&1 && &2)", "& &1 && &2"
    end

    test "with operators outside" do
      assert_same "(& &1) == (& &2)"
    end

    test "with call expressions" do
      assert_format "& local(&1, &2)", "&local(&1, &2)"
    end

    test "with blocks" do
      bad = "&(1; 2)"
      good = """
      &(1
        2)
      """
      assert_format bad, good
    end

    test "precedence when combined with calls" do
      assert_same "(&Foo).Bar"
      assert_format "&(Foo).Bar", "&Foo.Bar"
      assert_format "&(Foo.Bar).Baz", "&Foo.Bar.Baz"
    end

    test "local/arity" do
      assert_format "&(foo/1)", "&foo/1"
      assert_format "&(foo/bar)", "& foo / bar"
    end

    test "operator/arity" do
      assert_same "&+/2"
      assert_same "&and/2"
      assert_same "& &&/2"
    end

    test "Module.remote/arity" do
      assert_format "&(Mod.foo/1)", "&Mod.foo/1"
      assert_format "&(Mod.++/1)", "&Mod.++/1"
      assert_format ~s[&(Mod."foo bar"/1)], ~s[&Mod."foo bar"/1]

      # Invalid
      assert_format "&(Mod.foo/bar)", "& Mod.foo() / bar"

      # This is "invalid" as a special form but we don't
      # have enough knowledge to know that, so let's just
      # make sure we format it properly with proper wrapping.
      assert_same "&(1 + 2).foo/1"

      bad = """
      &my_function.foo().bar()/3
      """
      good = """
      &my_function.
         foo().bar/3
      """
      assert_format bad, good, @short_length
    end
  end

  describe "when" do
    test "with keywords" do
      assert_same "foo when bar: :baz"
    end

    test "with keywords on line breaks" do
      bad = "foo when bar: :baz"
      good = """
      foo when
        bar: :baz
      """
      assert_format bad, good, @short_length
    end
  end
end
