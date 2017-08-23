defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  describe "integer literals" do
    test "in decimal base" do
      assert_same "0"
      assert_same "100"
    end

    @tag :skip
    test "in decimal base with leading zeros" do
      assert_same "007"
    end

    test "in binary base" do
      assert_same "0b0"
      assert_same "0b1"
      assert_same "0b101"
    end

    @tag :skip
    test "in binary base with leading zeros" do
      assert_same "0b01"
    end

    test "in octal base" do
      assert_same "0o77"
      assert_same "0o0"
    end

    @tag :skip
    test "in octal base with leading zeros" do
      assert_same "0o01"
    end

    test "in hex base" do
      assert_same "0x1"
      assert_format "0xabcdef", "0xABCDEF"
    end

    @tag :skip
    test "in hex base with leading zeros" do
      assert_same "0x01"
    end

    test "as chars" do
      assert_same "?a"
      assert_same "?1"
      assert_same "?è"
      assert_same "??"
      assert_same "?\\\\"
      assert_same "?\\s"
    end
  end

  describe "string literals (double-quoted)" do
    test "without escapes" do
      assert_same ~S["foo"]
    end

    test "with escapes" do
      assert_same ~S["f\a\b\ro"]
    end

    test "converts literal new lines into escaped new lines" do
      assert_format """
      "fo
      o"
      """, ~S["fo\no"]
    end
  end
end
