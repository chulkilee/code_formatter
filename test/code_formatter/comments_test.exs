defmodule CodeFormatter.CommentsTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

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
      assert_same """
      :hello # this is hello
      :world # this is world
      """
    end

    test "empty comment" do
      assert_same """
      #
      :foo #
      """
    end

    test "before, on and after expressions with newlines" do
      assert_same """
      # before comment
      # second line

      :hello # this is hello

      # middle comment 1

      #

      # middle comment 2

      :world # this is world

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
      IO.puts("Hello #{world}") # comment
      """

      assert_same ~S"""
      IO.puts("Hello #{world}")
      # comment
      """

      # This is ambiguous so we move the comment out
      ambiguous = ~S"""
      IO.puts("Hello #{world # comment
      }")
      """

      assert_format ambiguous, ~S"""
      IO.puts("Hello #{world}") # comment
      """
    end

    test "with comment inside before, during and after" do
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
          world # comment
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
    test "with comment outside before, during and after" do
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

    test "with comment inside before, during and after" do
      assert_same ~S"""
      assert (
               # comment
               hello
               world
             )
      """

      assert_same ~S"""
      assert (
               hello # comment1
               world # comment2
             )
      """

      # TODO: Support end_line in parens
      # assert_same ~S"""
      # assert (
      #          hello
      #          world
      #          # comment
      #        )
      # """
    end
  end
end
