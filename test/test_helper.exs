ExUnit.start()

defmodule CodeFormatter.Case do
  defmacro assert_format(good) do
    quote bind_quoted: [good: good] do
      assert_format(good, good)
    end
  end

  defmacro assert_format(bad, good) do
    quote bind_quoted: [bad: bad, good: good] do
      assert CodeFormatter.format(bad) == good
      assert CodeFormatter.format(good) == good
    end
  end
end
