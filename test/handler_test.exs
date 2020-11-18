defmodule K8STrafficDrainHandlerTest do
  use ExUnit.Case, async: false

  @handler FT.K8S.TrafficDrainHandler

  test "SIGTERM starts draining, then stops after delay" do
    delay = 75

    start_supervised({@handler, [shutdown_delay_ms: delay, test_mode: self()]})

    refute @handler.draining?()

    {time, _} = :timer.tc(fn ->
      :gen_event.notify(:erl_signal_server, :sigterm)
      assert_receive :draining
      refute_received {:stopping, _}
      assert @handler.draining?(), "expected draining to have started"

      assert_receive {:stopping, state}, delay * 2
      assert  {_, ^delay, _} = state
    end)

    assert time >= delay * 1000, "expected stop after delay time"
    assert time < ((delay + 100) * 1000), "expected stopping message within 100ms of delay time"
  end

  test "SIGTERM starts draining, then stops after delay with custom table name" do
    delay = 75

    start_supervised({@handler, [shutdown_delay_ms: delay, test_mode: self(), table_name: :test123]})

    refute @handler.draining?(:test123)

    {time, _} = :timer.tc(fn ->
      :gen_event.notify(:erl_signal_server, :sigterm)
      assert_receive :draining
      refute_received {:stopping, _}
      assert @handler.draining?(:test123), "expected draining to have started"

      assert_receive {:stopping, state}, delay * 2
      assert  {_, ^delay, _} = state
    end)

    assert time >= delay * 1000, "expected stop after delay time"
    assert time < ((delay + 100) * 1000), "expected stopping message within 100ms of delay time"
  end
end
