defmodule K8STrafficDrainPlugTest do
  use ExUnit.Case, async: false
  use Plug.Test

  @handler FT.K8S.TrafficDrainHandler

  test "__traffic endpoint returns 200 normally" do
    start_supervised({@handler, [shutdown_delay_ms: 10, test_mode: true]})

    refute @handler.draining?()

    conn = conn(:get, "/__traffic")
    conn = FT.K8S.TrafficDrainPlug.call(conn, [])
    assert conn.status == 200
    assert conn.halted
    refute @handler.draining?()
  end

  test "__traffic endpoint returns 500 when draining" do
    start_supervised({@handler, [shutdown_delay_ms: 10, test_mode: self()]})

    :gen_event.notify(:erl_signal_server, :sigterm)
    assert_receive :draining

    assert @handler.draining?()

    conn = conn(:get, "/__traffic")
    conn = FT.K8S.TrafficDrainPlug.call(conn, [])
    assert conn.status == 500
    assert conn.halted

    assert_receive {:stopping, _}
  end

  test "plug passes through other requests" do
    start_supervised({@handler, [shutdown_delay_ms: 10, test_mode: true]})
    conn = conn(:get, "/__whatever")
    conn = FT.K8S.TrafficDrainPlug.call(conn, [])
    assert conn.status == nil
    refute conn.halted
  end

end
