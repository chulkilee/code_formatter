defmodule CodeFormatter.CallsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

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
      MyModule.Foo.
        bar(
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
      my_function.foo().
        bar(2, 3).
        baz(4, 5)
      """
      assert_format bad, good, @medium_length
    end

    test "doesn't split on parens on empty arguments" do
      assert_same "Mod.func()", @short_length
    end
  end
end
