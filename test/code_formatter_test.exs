defmodule CodeFormatterTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  @short_length [line_length: 10]

  describe "integer literals" do
    test "in decimal base" do
      assert_same "0"
      assert_same "100"
      assert_same "007"
      assert_same "10000"
      assert_format "100000", "100_000"
      assert_format "1000000", "1_000_000"
    end

    test "in binary base" do
      assert_same "0b0"
      assert_same "0b1"
      assert_same "0b101"
      assert_same "0b01"
      assert_format "0b111_111", "0b111111"
    end

    test "in octal base" do
      assert_same "0o77"
      assert_same "0o0"
      assert_same "0o01"
      assert_format "0o777_777", "0o777777"
    end

    test "in hex base" do
      assert_same "0x1"
      assert_format "0xabcdef", "0xABCDEF"
      assert_same "0x01"
      assert_format "0xFFF_FFF", "0xFFFFFF"
    end

    test "as chars" do
      assert_same "?a"
      assert_same "?1"
      assert_same "?è"
      assert_same "??"
      assert_same "?\\\\"
      assert_same "?\\s"
      assert_same "?🎾"
    end
  end

  describe "float literals" do
    test "with normal notation" do
      assert_same "0.0"
      assert_same "1.0"
      assert_same "123.456"
      assert_same "0.0000001"
      assert_same "001.100"
      assert_format "0_10000_0.000_000", "0_100_000.000000"
    end

    test "with scientific notation" do
      assert_same "1.0e1"
      assert_same "1.0e-1"
      assert_same "1.0e01"
      assert_same "1.0e-01"
      assert_same "001.100e-010"
      assert_format "0_1_00_0_000.100e-010", "01_000_000.100e-010"

      assert_format "1.0E01", "1.0e01"
      assert_format "1.0E-01", "1.0e-01"
    end
  end

  describe "string literals (double-quoted)" do
    test "without escapes" do
      assert_same ~S["foo"]
    end

    test "with escapes" do
      assert_same ~S["f\a\b\ro"]
      assert_same ~S["double \" quote"]
    end

    test "converts literal new lines into escaped new lines" do
      assert_format """
      "fo
      o"
      """, ~S["fo\no"]
    end

    test "with interpolation" do
      assert_same ~S["one #{2} three"]
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      "one #{"two"} three"
      """

      good = ~S"""
      "one #{
        "two"
      } three"
      """

      assert_format bad, good, @short_length
    end
  end

  describe "atom literals" do
    test "without escapes" do
      assert_same ~S[:foo]
    end

    test "with escapes" do
      assert_same ~S[:"f\a\b\ro"]
      assert_same ~S[:"double \" quote"]
    end

    test "does not reformat aliases" do
      assert_same ~S[:"Elixir.String"]
    end

    test "removes quotes when they are not necessary" do
      assert_format ~S[:"foo"], ~S[:foo]
      assert_format ~S[:"++"], ~S[:++]
    end

    test "with interpolation" do
      assert_same ~S[:"one #{2} three"]
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      :"one #{"two"} three"
      """

      good = ~S"""
      :"one #{
        "two"
      } three"
      """

      assert_format bad, good, @short_length
    end
  end

  describe "charlist literals" do
    test "without escapes" do
      assert_same ~S['foo']
    end

    test "with escapes" do
      assert_same ~S['f\a\b\ro']
      assert_same ~S['single \' quote']
    end

    test "converts literal new lines into escaped new lines" do
      assert_format """
      'fo
      o'
      """, ~S['fo\no']
    end

    test "with interpolation" do
      assert_same ~S['one #{2} three']
    end

    test "with interpolation on line limit" do
      bad = ~S"""
      'one #{"two"} three'
      """

      good = ~S"""
      'one #{
        "two"
      } three'
      """

      assert_format bad, good, @short_length
    end
  end
end
