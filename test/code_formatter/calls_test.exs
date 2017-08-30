defmodule CodeFormatter.CallsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "cancel break" do
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

    test "for tuples" do
      bad = "long_call({1, 2})"
      good = """
      long_call(
        {1, 2}
      )
      """
      assert_format bad, good, @short_length

      bad = "foo({1, 2, 3, 4})"
      good = """
      foo({
        1,
        2,
        3,
        4
      })
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

    test "without parens" do
      assert_same "import :foo, :bar"
    end

    test "without parens on line limit" do
      bad = "import :long_atom, :other_arg"
      good = """
      import :long_atom,
             :other_arg
      """
      assert_format bad, good, @short_length

      bad = "import :atom, opts: [foo: :bar]"
      good = """
      import :atom,
        opts: [foo: :bar]
      """
      assert_format bad, good, @medium_length

      bad = "import :atom, really_long_key: [foo: :bar]"
      good = """
      import :atom,
        really_long_key: [
          foo: :bar
        ]
      """
      assert_format bad, good, @medium_length

      assert_same """
      import :foo,
        one: two,
        three: four,
        five: [6, 7, 8, 9]
      """, @medium_length
    end

    test "call on call" do
      assert_same "unquote(call)()"
      assert_same "unquote(call)(one, two)"
      assert_same """
      unquote(call)(one, two) do
        :ok
      end
      """
    end

    test "call on call on line limit" do
      bad = "foo(bar)(one, two, three)"
      good = """
      foo(bar)(
        one,
        two,
        three
      )
      """
      assert_format bad, good, @short_length
    end
  end

  describe "remote calls" do
    test "with no arguments" do
      assert_format "Foo . Bar . baz", "Foo.Bar.baz()"
      assert_format ":erlang.\nget_stacktrace", ":erlang.get_stacktrace()"
      assert_format "@foo.bar()", "@foo.bar"
      assert_format "(@foo).bar()", "@foo.bar"
      assert_format "__MODULE__.start_link", "__MODULE__.start_link()"
      assert_format "Foo.bar.baz.bong", "Foo.bar().baz.bong"
      assert_format "(1 + 2).foo()", "(1 + 2).foo"
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
      My_function.foo().bar(2, 3).baz(4, 5)
      """
      good = """
      My_function.foo().bar(
        2,
        3
      ).baz(4, 5)
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
      mod.really_long_function_name(
        :hello,
        foo: 1,
        bar: 2
      )
      """, @short_length

      assert_same """
      really_long_module_name.foo(
        :hello,
        foo: 1,
        bar: 2
      )
      """, @short_length
    end

    test "wraps left side in parens if it is an anonymous function" do
      assert_same "(fn -> :ok end).foo"
    end

    test "wraps left side in parens if it is a do-end block" do
      assert_same """
      (if true do
         :ok
       end).foo
      """
    end

    test "wraps left side in parens if it is a do-end block as an argument" do
      assert_same """
      import (if true do
                :ok
              end).foo
      """
    end

    test "call on call" do
      assert_same "foo.bar(call)()"
      assert_same "foo.bar(call)(one, two)"
      assert_same """
      foo.bar(call)(one, two) do
        :ok
      end
      """
    end

    test "call on call on line limit" do
      bad = "a.b(foo)(one, two, three)"
      good = """
      a.b(foo)(
        one,
        two,
        three
      )
      """
      assert_format bad, good, @short_length
    end

    test "on vars" do
      assert_format "foo.bar()", "foo.bar"
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

    test "does not split on dot on line limit" do
      assert_same "my_function.()", @short_length
    end

    test "splits on arguments on line limit" do
      bad = """
      my_function.(1, 2, 3)
      """
      good = """
      my_function.(
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
      my_function.(
        1,
        2
      ).f(3, 4).(
        5,
        6
      )
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

    test "wraps left side in parens if it is an anonymous function" do
      assert_same "(fn -> :ok end).()"
    end

    test "wraps left side in parens if it is a do-end block" do
      assert_same """
      (if true do
         :ok
       end).()
      """
    end

    test "wraps left side in parens if it is a do-end block as an argument" do
      assert_same """
      import (if true do
                :ok
              end).()
      """
    end
  end

  describe "do-end blocks" do
    test "with non-block keywords" do
      bad = "foo do: nil"
      good = """
      foo do
        nil
      end
      """
      assert_format bad, good

      bad = "foo else: this, do: that"
      good = """
      foo do
        that
      else
        this
      end
      """
      assert_format bad, good

      assert_same "foo(do: this, do: that)"
      assert_same "foo(do: this, other: that)"
      assert_same "foo(do: this, else: this, else: that)"
    end

    test "with multiple keywords" do
      bad = """
      foo do
        :do
      else
        :else
      rescue
        :rescue
      catch
        :catch
      after
        :after
      end
      """

      good = """
      foo do
        :do
      rescue
        :rescue
      catch
        :catch
      else
        :else
      after
        :after
      end
      """

      assert_format bad, good
    end

    test "with multiple keywords and arrows" do
      assert_same """
      foo do
        a1 -> a2
        b1 -> b2
      rescue
        a1 -> a2
        b1 -> b2
      catch
        a1 -> a2
        b1 -> b2
      else
        a1 -> a2
        b1 -> b2
      after
        a1 -> a2
        b1 -> b2
      end
      """
    end

    test "with no extra arguments" do
      bad = "foo do end"
      good = """
      foo do
        nil
      end
      """
      assert_format bad, good
    end

    test "with no extra arguments and line breaks" do
      assert_same """
      foo do
        a1 ->
          really_long_line
        b1 ->
          b2
      rescue
        c1
      catch
        d1 -> d1
        e1 -> e1
      else
        f2
      after
        g1 ->
          really_long_line
        h1 ->
          h2
      end
      """, @medium_length
    end

    test "with extra arguments" do
      bad = "foo bar, baz do end"
      good = """
      foo bar, baz do
        nil
      end
      """
      assert_format bad, good
    end

    test "with extra arguments and line breaks" do
      assert_same """
      foo bar, baz do
        a1 ->
          really_long_line
        b1 ->
          b2
      rescue
        c1
      catch
        d1 -> d1
        e1 -> e1
      else
        f2
      after
        g1 ->
          really_long_line
        h1 ->
          h2
      end
      """, @medium_length

      assert_same """
      foo really,
          long,
          list,
          of,
          arguments do
        a1 ->
          really_long_line
        b1 ->
          b2
      rescue
        c1
      catch
        d1 -> d1
        e1 -> e1
      else
        f2
      after
        g1 ->
          really_long_line
        h1 ->
          h2
      end
      """, @medium_length
    end

    test "inside call" do
      bad = "foo (bar do :ok end)"
      good = """
      foo(
        bar do
          :ok
        end
      )
      """
      assert_format bad, good

      bad = "import (bar do :ok end)"
      good = """
      import (bar do
                :ok
              end)
      """
      assert_format bad, good
    end

    test "inside operator" do
      bad = "foo + bar do :ok end"
      good = """
      foo +
        bar do
          :ok
        end
      """
      assert_format bad, good
    end

    test "inside operator inside argument" do
      bad = "fun foo + (bar do :ok end)"
      good = """
      fun(
        foo +
          bar do
            :ok
          end
      )
      """
      assert_format bad, good

      bad = "if foo + (bar do :ok end) do :ok end"
      good = """
      if foo +
           (bar do
              :ok
            end) do
        :ok
      end
      """
      assert_format bad, good
    end

    test "inside operator inside argument with remote call" do
      bad = "if foo + (Bar.baz do :ok end) do :ok end"
      good = """
      if foo +
           (Bar.baz do
              :ok
            end) do
        :ok
      end
      """
      assert_format bad, good
    end
  end

  describe "tuple calls" do
    test "without arguments" do
      assert_format "foo . {}", "foo.{}"
    end

    test "with arguments" do
      assert_format "foo.{bar,baz,bat,}", "foo.{bar, baz, bat}"
    end

    test "with arguments on line limit" do
      bad = "foo.{bar,baz,bat,}"
      good = """
      foo.{
        bar,
        baz,
        bat
      }
      """
      assert_format bad, good, @short_length

      bad = "really_long_expression.{bar,baz,bat,}"
      good = """
      really_long_expression.{
        bar,
        baz,
        bat
      }
      """
      assert_format bad, good, @short_length
    end

    test "with keywords" do
      assert_same "expr.{:hello, foo: bar, baz: bat}"
    end
  end

  describe "access" do
    test "with one argument" do
      assert_format "foo[ bar ]", "foo[bar]"
    end

    test "with arguments on line limit" do
      bad = "foo[really_long_argument()]"
      good = """
      foo[
        really_long_argument()
      ]
      """
      assert_format bad, good, @short_length

      bad = "really_long_expression[really_long_argument()]"
      good = """
      really_long_expression[
        really_long_argument()
      ]
      """
      assert_format bad, good, @short_length
    end

    test "with do-end blocks" do
      assert_same """
      (if true do
         false
       end)[key]
      """
    end

    test "with keywords" do
      assert_format "expr[foo: bar, baz: bat]", "expr[[foo: bar, baz: bat]]"
    end
  end
end
