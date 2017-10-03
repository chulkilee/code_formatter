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
end
