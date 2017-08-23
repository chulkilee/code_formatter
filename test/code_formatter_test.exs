defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  describe "integer literals" do
    test "in decimal base" do
      assert_format "0"
      assert_format "100"
    end

    @tag :skip
    test "in decimal base with leading zeros" do
      assert_format "007"
    end

    test "in binary base" do
      assert_format "0b0"
      assert_format "0b1"
      assert_format "0b101"
    end

    @tag :skip
    test "in binary base with leading zeros" do
      assert_format "0b01"
    end

    test "in octal base" do
      assert_format "0o77"
      assert_format "0o0"
    end

    @tag :skip
    test "in octal base with leading zeros" do
      assert_format "0o01"
    end

    test "in hex base" do
      assert_format "0x1"
      assert_format "0xabcdef", "0xABCDEF"
    end

    @tag :skip
    test "in hex base with leading zeros" do
      assert_format "0x01"
    end

    test "as chars" do
      assert_format "?a"
      assert_format "?1"
      assert_format "?è"
      assert_format "??"
      assert_format "?\\\\"
      assert_format "?\\s"
    end
  end

  describe "string literals (double-quoted)" do
    test "without escapes" do
      assert_format ~S["foo"]
    end

    test "with escapes" do
      assert_format ~S["f\a\b\ro"]
    end

    test "converts literal new lines into escaped new lines" do
      assert_format """
      "fo
      o"
      """, ~S["fo\no"]
    end
  end
end
