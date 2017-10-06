defmodule CodeFormatter.CommentsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]

  describe "at the root" do
    test "for empty documents" do
      assert_same "# hello world"
    end

    test "are reformatted" do
      assert_format "#oops", "# oops"
    end

    test "before and after expressions" do
      assert_same """
      # before comment
      :hello
      """

      assert_same """
      :hello
      # after comment
      """

      assert_same """
      # before comment
      :hello
      # after comment
      """
    end

    test "on expressions" do
      bad = """
      :hello # this is hello
      :world # this is world
      """

      good = """
      # this is hello
      :hello
      # this is world
      :world
      """

      assert_format bad, good

      bad = """
      foo    # this is foo
      |> bar # this is bar
      |> baz # this is baz
      """

      good = """
      # this is foo
      # this is bar
      # this is baz
      foo
      |> bar
      |> baz
      """

      assert_format bad, good, @short_length

      bad = """
      foo   # this is foo
      | bar # this is bar
      | baz # this is baz
      """

      good = """
      # this is foo
      # this is bar
      # this is baz
      foo
      | bar
      | baz
      """

      assert_format bad, good, @short_length
    end

    test "empty comment" do
      assert_same """
      #
      :foo
      """
    end

    test "before and after expressions with newlines" do
      assert_same """
      # before comment
      # second line

      :hello

      # middle comment 1

      #

      # middle comment 2

      :world

      # after comment
      # second line
      """
    end
  end

  describe "interpolation" do
    test "with comment outside before, during and after" do
      assert_same ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      assert_same ~S"""
      IO.puts("Hello #{world}")
      # comment
      """
    end

    test "with trailing comments" do
      # This is trailing so we move the comment out
      trailing = ~S"""
      IO.puts("Hello #{world}") # comment
      """

      assert_format trailing, ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      IO.puts("Hello #{world # comment
      }")
      """

      assert_format ambiguous, ~S"""
      # comment
      IO.puts("Hello #{world}")
      """
    end

    test "with comment inside before and after" do
      assert_same ~S"""
      IO.puts(
        "Hello #{
          # comment
          world
        }"
      )
      """

      assert_same ~S"""
      IO.puts(
        "Hello #{
          world
          # comment
        }"
      )
      """
    end
  end

  describe "parens blocks" do
    test "with comment outside before and after" do
      assert_same ~S"""
      # comment
      assert (
               hello
               world
             )
      """

      assert_same ~S"""
      assert (
               hello
               world
             )

      # comment
      """
    end

    test "with trailing comments" do
      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert ( # comment
               hello
               world
             )
      """

      assert_format ambiguous, ~S"""
      # comment
      assert (
               hello
               world
             )
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      assert (
               hello
               world
             ) # comment
      """

      assert_format ambiguous, ~S"""
      assert (
               hello
               world
             )

      # comment
      """
    end

    test "with comment inside before and after" do
      assert_same ~S"""
      assert (
               # comment
               hello
               world
             )
      """

      assert_same ~S"""
      assert (
               hello
               world
               # comment
             )
      """
    end
  end
end
