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

    test "before, during and after interpolation" do
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

    test "before and during inside interpolation" do
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
end
