defmodule CodeFormatter.CallsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "next break fits" do
    test "does not apply to function calls" do
      bad = "foo(very_long_call(bar))"
      good = """
      foo(
        very_long_call(
          bar
        )
      )
      """
      assert_format bad, good, @short_length
    end

    test "does not apply to strings" do
      bad = "foo(\"very long string\")"
      good = """
      foo(
        "very long string"
      )
      """
      assert_format bad, good, @short_length
    end

    test "for functions" do
      assert_same """
      foo(fn x -> y end)
      """

      assert_same """
      foo(fn
        a1 -> :ok
        b2 -> :error
      end)
      """

      assert_same """
      foo(bar, fn
        a1 -> :ok
        b2 -> :error
      end)
      """

      assert_same """
      foo(fn x ->
        :really_long_atom
      end)
      """, @medium_length

      assert_same """
      foo(bar, fn
        a1 ->
          :ok
        b2 ->
          :really_long_error
      end)
      """, @medium_length
    end

    test "for heredocs" do
      assert_same """
      foo('''
      bar
      ''')
      """

      assert_same to_string('''
      foo("""
      bar
      """)
      ''')

      assert_same """
      foo(~S'''
      bar
      ''')
      """

      assert_same """
      foo(~S'''
      very long line does trigger another break
      ''')
      """, @short_length
    end

    test "for binaries" do
      bad = "foo(<<1, 2, 3, 4>>)"
      good = """
      foo(<<
        1,
        2,
        3,
        4
      >>)
      """
      assert_format bad, good, @short_length
    end

    test "for lists" do
      bad = "foo([1, 2, 3, 4])"
      good = """
      foo([
        1,
        2,
        3,
        4
      ])
      """
      assert_format bad, good, @short_length
    end

    test "with keyword lists" do
      assert_same """
      foo(:hello, foo: foo, bar: '''
      baz
      ''')
      """
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
      fun(
        x,
        y,
        z
      )
      """
      assert_format bad, good, @short_length
    end

    test "with keyword lists" do
      assert_same "foo(foo: 1, bar: 2)"

      assert_same "foo(:hello, foo: 1, bar: 2)"

      assert_same """
      foo(
        :hello,
        foo: 1,
        bar: 2
      )
      """, @short_length
    end
  end

  describe "remote calls" do
    test "with no arguments" do
      assert_format "Foo . Bar . baz", "Foo.Bar.baz()"
      assert_format ":erlang.\nget_stacktrace", ":erlang.get_stacktrace()"
      assert_format "@foo.bar", "@foo.bar()"
      assert_format "(@foo).bar", "@foo.bar()"
      assert_format "__MODULE__.start_link", "__MODULE__.start_link()"
      assert_format "foo.bar.baz.bong", "foo.bar().baz().bong()"
      assert_format "(1 + 2).foo", "(1 + 2).foo()"
    end

    test "with arguments" do
      assert_format "Foo . Bar. baz(1, 2, 3)", "Foo.Bar.baz(1, 2, 3)"
      assert_format ":erlang.\nget(\n:some_key\n)", ":erlang.get(:some_key)"
      assert_same "@foo.bar(1, 2, 3)"
      assert_same "__MODULE__.start_link(1, 2, 3)"
      assert_same "foo.bar(1).baz(2, 3)"
    end

    test "inspects function names correctly" do
      assert_same ~S[MyModule."my function"(1, 2)]
      assert_same ~S[MyModule."Foo.Bar"(1, 2)]
      assert_same ~S[Kernel.+(1, 2)]
      assert_same ~S[:erlang.+(1, 2)]
      assert_same ~S[foo."bar baz"(1, 2)]
    end

    test "splits on arguments and dot on line limit" do
      bad = """
      MyModule.Foo.bar(:one, :two, :three)
      """
      good = """
      MyModule.Foo.bar(
        :one,
        :two,
        :three
      )
      """
      assert_format bad, good, @medium_length

      bad = """
      my_function.foo().bar(2, 3).baz(4, 5)
      """
      good = """
      my_function.
        foo().
        bar(2, 3).
        baz(4, 5)
      """
      assert_format bad, good, @medium_length
    end

    test "doesn't split on parens on empty arguments" do
      assert_same "Mod.func()", @short_length
    end

    test "with keyword lists" do
      assert_same "mod.foo(foo: 1, bar: 2)"

      assert_same "mod.foo(:hello, foo: 1, bar: 2)"

      assert_same """
      mod.foo(
        :hello,
        foo: 1,
        bar: 2
      )
      """, @short_length

      assert_same """
      really_long_module_name.
        foo(
          :hello,
          foo: 1,
          bar: 2
        )
      """, @short_length
    end
  end

  describe "anonymous function calls" do
    test "without arguments" do
      assert_format "foo . ()", "foo.()"
      assert_format "(foo.()).().()", "foo.().().()"
      assert_same "@foo.()"
      assert_same "(1 + 1).()"
      assert_same ":foo.()"
    end

    test "with arguments" do
      assert_format "foo . (1, 2  ,  3 )", "foo.(1, 2, 3)"
      assert_format "foo . (1, 2 ).(3,4)", "foo.(1, 2).(3, 4)"
      assert_same "@foo.(:one, :two)"
      assert_same "foo.(1 + 1).(hello)"
    end

    test "splits on dot on line limit" do
      bad = "my_function.()"
      good = """
      my_function.
        ()
      """
      assert_format bad, good, @short_length
    end

    test "splits on arguments on line limit" do
      bad = """
      my_function.(1, 2, 3)
      """
      good = """
      my_function.
        (
          1,
          2,
          3
        )
      """
      assert_format bad, good, @short_length

      bad = """
      my_function.(1, 2).f(3, 4).(5, 6)
      """
      good = """
      my_function.
        (1, 2).
        f(3, 4).
        (5, 6)
      """
      assert_format bad, good, @short_length
    end

    test "with keyword lists" do
      assert_same "foo.(foo: 1, bar: 2)"

      assert_same "foo.(:hello, foo: 1, bar: 2)"

      assert_same """
      foo.(
        :hello,
        foo: 1,
        bar: 2
      )
      """, @short_length
    end
  end
end
