defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]

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
      assert_same ~S"""
      ~S(foo\
bar)
      """
    end

    test "with nested new lines" do
      assert_same ~S"""
      foo do
        ~S(foo\
      bar)
      end
      """

      assert_same ~S"""
      foo do
        ~s(#{bar}
)
      end
      """
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

    test "with heredoc syntax and modifier" do
      assert_same ~S"""
      ~s'''
      foo
      '''rsa
      """
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

      assert_same "fn () when node() == :nonode@nohost -> true end"
    end

    test "with a single clause and arguments" do
      assert_format "fn  x ,y-> x + y  end", "fn x, y -> x + y end"

      bad = "fn x -> foo(x) end"
      good = """
      fn x ->
        foo(x)
      end
      """
      assert_format bad, good, @short_length

      bad = "fn one, two, three -> foo(x) end"
      good = """
      fn one,
         two,
         three ->
        foo(x)
      end
      """
      assert_format bad, good, @short_length
    end

    test "with a single clause and when" do
      assert_same """
      fn arg when
           guard ->
        :ok
      end
      """, @short_length
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

      assert_same """
      fn
        arg11,
        arg12 ->
          body1

        arg21,
        arg22 ->
          body2

        arg31,
        arg32 ->
          body3
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
            a +
              b ->
          :ok
        b1, b2 when
            c +
              d ->
          :ok
      end
      """
      assert_format long, good, @short_length
    end

    test "uses block context for the body of each clause" do
      assert_same "fn -> @foo bar end"
    end

    test "preserves user choice even when it fits" do
      assert_same """
      fn
        1 ->
          :ok
        2 ->
          :ok
      end
      """

      assert_same """
      fn
        1 ->
          :ok

        2 ->
          :ok

        3 ->
          :ok
      end
      """
    end
  end

  describe "anonymous functions types" do
    test "with a single clause and no arguments" do
      assert_format "(->:ok)", "(-> :ok)"
      assert_same "(-> :really_long_atom)", @short_length
      assert_same "(() when node() == :nonode@nohost -> true)"
    end

    test "with a single clause and arguments" do
      assert_format "( x ,y-> x + y  )", "(x, y -> x + y)"

      bad = "(x -> :really_long_atom)"
      good = """
      (x ->
         :really_long_atom)
      """
      assert_format bad, good, @short_length

      bad = "(one, two, three -> foo(x))"
      good = """
      (one,
       two,
       three ->
         foo(x))
      """
      assert_format bad, good, @short_length
    end

    test "with multiple clauses" do
      assert_same """
      (
        1 -> :ok
        2 -> :ok
      )
      """, @short_length

      assert_same """
      (
        1 ->
          :ok
        2 ->
          :error
      )
      """, @short_length

      assert_same """
      (
        arg11,
        arg12 ->
          body1
        arg21,
        arg22 ->
          body2
      )
      """, @short_length

      assert_same """
      (
        arg11,
        arg12 ->
          body1

        arg21,
        arg22 ->
          body2

        arg31,
        arg32 ->
          body2
      )
      """, @short_length
    end

    test "with heredocs" do
      assert_same """
      (
        arg1 ->
          '''
          foo
          '''
        arg2 ->
          '''
          bar
          '''
      )
      """
    end

    test "with multiple empty clauses" do
      assert_same """
      (
        () -> :ok1
        () -> :ok2
      )
      """
    end

    test "preserves user choice even when it fits" do
      assert_same """
      (
        1 ->
          :ok
        2 ->
          :ok
      )
      """

      assert_same """
      (
        1 ->
          :ok

        2 ->
          :ok

        3 ->
          :ok
      )
      """
    end
  end

  describe "blocks" do
    test "with multiple lines" do
      assert_same """
      foo = bar
      baz = bat
      """
    end

    test "with multiple lines with line limit" do
      assert_same """
      foo =
        bar(one)

      baz =
        bat(two)

      a(b)
      """, @short_length

      assert_same """
      foo =
        bar(one)

      a(b)

      baz =
        bat(two)
      """, @short_length

      assert_same """
      a(b)

      foo =
        bar(one)

      baz =
        bat(two)
      """, @short_length

      assert_same """
      foo =
        bar(one)

      one =
        two(ghi)

      baz =
        bat(two)
      """, @short_length
    end

    test "with multiple lines with line limit inside block" do
      assert_same """
      block do
        a =
          b(foo)

        c =
          d(bar)

        e =
          f(baz)
      end
      """, @short_length
    end

    test "with multiple lines with cancel expressions" do
      assert_same """
      foo(%{
        long_key: 1
      })

      bar(%{
        long_key: 1
      })

      baz(%{
        long_key: 1
      })
      """, @short_length
    end

    test "with heredoc" do
      assert_same """
      block do
        '''
        a

        b

        c
        '''
      end
      """
    end

    test "keeps user newlines" do
      assert_same """
      defmodule Mod do
        field(:foo)
        field(:bar)
        field(:baz)
        belongs_to(:one)
        belongs_to(:two)
        timestamp()
        lock()
        has_many(:three)
        has_many(:four)
        :ok
        has_one(:five)
        has_one(:six)
        foo = 1
        bar = 2
        :before
        baz = 3
        :after
      end
      """

      bad = """
      defmodule Mod do
        field(:foo)

        field(:bar)

        field(:baz)


        belongs_to(:one)
        belongs_to(:two)


        timestamp()

        lock()


        has_many(:three)
        has_many(:four)


        :ok


        has_one(:five)
        has_one(:six)


        foo = 1
        bar = 2


        :before
        baz = 3
        :after
      end
      """

      good = """
      defmodule Mod do
        field(:foo)

        field(:bar)

        field(:baz)

        belongs_to(:one)
        belongs_to(:two)

        timestamp()

        lock()

        has_many(:three)
        has_many(:four)

        :ok

        has_one(:five)
        has_one(:six)

        foo = 1
        bar = 2

        :before
        baz = 3
        :after
      end
      """

      assert_format bad, good
    end

    test "with multiple defs" do
      assert_same """
      def foo(:one), do: 1
      def foo(:two), do: 2
      def foo(:three), do: 3
      """
    end

    test "with module attributes" do
      assert_same """
      defmodule Foo do
        @constant 1
        @constant 2

        @doc '''
        foo
        '''
        def foo do
          :ok
        end

        @spec bar :: 1
        @spec bar :: 2
        def bar do
          :ok
        end

        @other_constant 3

        @spec baz :: 4
        @doc '''
        baz
        '''
        def baz do
          :ok
        end

        @another_constant 5
        @another_constant 5

        @doc '''
        baz
        '''
        @spec baz :: 6
        def baz do
          :ok
        end
      end
      """
    end
  end

  describe "renames deprecated calls" do
    test "without deprecation option" do
      assert_same "Enum.partition(foo, bar)"
      assert_same "&Enum.partition/2"
    end

    test "with matching deprecation option" do
      assert_format "Enum.partition(foo, bar)",
                    "Enum.split_with(foo, bar)",
                    rename_deprecated_at: "1.4.0"

      assert_format "Enum.partition(foo, bar)",
                    "Enum.split_with(foo, bar)",
                    rename_deprecated_at: "1.4.0"
    end

    test "without matching deprecation option" do
      assert_same "Enum.partition(foo, bar)",
                  rename_deprecated_at: "1.3.0"

      assert_same "Enum.partition(foo, bar)",
                  rename_deprecated_at: "1.3.0"
    end

    test "raises on invalid version" do
      assert_raise ArgumentError, ~r"invalid version", fn ->
        assert_same "Enum.partition(foo, bar)", rename_deprecated_at: "1.3"
      end
    end
  end
end
