defmodule CodeFormatter.ContainersTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]

  describe "tuples" do
    # TODO: Test tuple with keywords as last element

    test "without arguments" do
      assert_format "{ }", "{}"
    end

    test "with arguments" do
      assert_format "{1,2}", "{1, 2}"
      assert_format "{1,2,3}", "{1, 2, 3}"
    end

    test "is strict on line limits" do
      bad = "{1, 2, 3, 4}"
      good = """
      {
        1,
        2,
        3,
        4
      }
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "{1,}", "{1}"
      assert_format "{1, 2, 3,}", "{1, 2, 3}"
    end
  end

  describe "lists" do
    # TODO: Test list that finishes with keywords entries

    test "empty" do
      assert_format "[ ]", "[]"
      assert_format "[\n]", "[]"
    end

    test "with elements" do
      assert_format "[ 1 , 2,3, 4 ]", "[1, 2, 3, 4]"
    end

    test "with tail" do
      assert_format "[1,2,3|4]", "[1, 2, 3 | 4]"
    end

    test "are strict on line limit" do
      bad = """
      [11, 22, 33, 44]
      """
      good = """
      [
        11,
        22,
        33,
        44
      ]
      """
      assert_format bad, good, @short_length

      bad = """
      [11, 22, 33 | 44]
      """
      good = """
      [
        11,
        22,
        33 | 44
      ]
      """
      assert_format bad, good, @short_length

      bad = """
      [1, 2, 3 | 4]
      """
      good = """
      [
        1,
        2,
        3 | 4
      ]
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "[1,]", "[1]"
      assert_format "[1, 2, 3,]", "[1, 2, 3]"
    end
  end

  describe "bitstrings" do
    test "without arguments" do
      assert_format "<< >>", "<<>>"
      assert_format "<<\n>>", "<<>>"
    end

    test "with arguments" do
      assert_format "<<1,2,3>>", "<<1, 2, 3>>"
    end

    test "add parens on first and last in case of ambiguity" do
      assert_format "<< <<>>, <<>>, <<>> >>", "<<(<<>>), <<>>, (<<>>)>>"
      assert_format "<< <<>>::1, <<>>::2, <<>>::3 >>", "<<(<<>>)::1, <<>>::2, <<>>::3>>"
    end

    test "with modifiers" do
      assert_format "<< 1 :: 1 >>", "<<1::1>>"
      assert_format "<< 1 :: 2 + 3 >>", "<<1::(2 + 3)>>"
      assert_format "<< 1 :: 2 - integer >>", "<<1::2-integer>>"
      assert_format "<< 1 :: 2 - unit(3) >>", "<<1::2-unit(3)>>"
      assert_format "<< 1 :: 2 - unit(3) - 4 * 5 >>", "<<1::2-unit(3)-(4 * 5)>>"
    end

    test "is strict on line limits" do
      bad = "<<1, 2, 3, 4>>"
      good = """
      <<
        1,
        2,
        3,
        4
      >>
      """
      assert_format bad, good, @short_length
    end
  end
end
