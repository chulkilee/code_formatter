defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

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

  describe "anonymous functions" do
    test "with a single clause and no arguments" do
      assert_format "fn  ->:ok  end", "fn -> :ok end"

      bad = "fn -> :foo end"
      good = """
      fn ->
        :foo
      end
      """
      assert_format bad, good, @short_length
    end

    test "with a single clause and arguments" do
      assert_format "fn  x ,y-> x + y  end", "fn x, y -> x + y end"

      # TODO: x -> should be in the same line
      bad = "fn x -> foo(x) end"
      good = """
      fn
        x ->
          foo(x)
      end
      """
      assert_format bad, good, @short_length

      bad = "fn one, two, three -> foo(x) end"
      good = """
      fn
        one,
        two,
        three ->
          foo(x)
      end
      """
      assert_format bad, good, @short_length
    end

    test "with multiple clauses" do
      assert_same """
      fn
        1 -> :ok
        2 -> :ok
      end
      """, @short_length

      assert_same """
      fn
        1 ->
          :ok
        2 ->
          :error
      end
      """, @short_length

      assert_same """
      fn
        arg11,
        arg12 ->
          body1
        arg21,
        arg22 ->
          body2
      end
      """, @short_length
    end

    test "with heredocs" do
      assert_same """
      fn
        arg1 ->
          '''
          foo
          '''
        arg2 ->
          '''
          bar
          '''
      end
      """
    end

    test "with multiple empty clauses" do
      assert_same """
      fn
        () -> :ok1
        () -> :ok2
      end
      """
    end

    test "with when in clauses" do
      assert_same """
      fn
        a1 when a + b -> :ok
        b1 when c + d -> :ok
      end
      """

      long = """
      fn
        a1, a2 when a + b -> :ok
        b1, b2 when c + d -> :ok
      end
      """
      assert_same long

      good = """
      fn
        a1, a2 when
            a + b ->
          :ok
        b1, b2 when
            c + d ->
          :ok
      end
      """
      assert_format long, good, @medium_length
    end

    test "uses block context for the body of each clause" do
      assert_same "fn -> @foo bar end"
    end
  end
end
