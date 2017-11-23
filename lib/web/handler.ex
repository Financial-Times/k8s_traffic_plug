defmodule FT.K8S.TrafficDrainHandler do
  @moduledoc """
  Support for putting an Application into 'connection-draining' mode, for gracefully handling
  the K8S [termination life-cycle](https://kubernetes.io/docs/concepts/workloads/pods/pod/#termination-of-pods).

  Adds a handler for [`erl_signal_server`](http://erlang.org/doc/man/kernel_app.html) (needs OTP 20+),
  removing the default one, and handling the `:sigterm` event itself.

  When a `SIGTERM` is received, the handler enters 'connection draining mode',
  during which `draining?/0` starts returning `true`, and then, after a delay, the handler
  calls `System.stop/1`, carefully halting the Erlang VM.

  Other signals are handled by passing to the default `erl_signal_handler` implementation.

  When used with `FT.K8S.TrafficDrainPlug` in a pipeline, when 'connection draining mode' is
  entered, the `/__traffic` endpoint starts returning a `500 Draining` response, rather than
  its normal `200 OK`.

  ## Usage

  Call `start_link/1` from your Application's `start/2` function, or add this module to a supervisor (it
  supports `child_spec/1`), passing an optional `shutdown_delay_ms` for the time between entering
  'connection draining mode' and `System.stop/0`, e.g.:

  ```
  Supervisor.start_link([
    {FT.K8S.TrafficDrainHandler, [shutdown_delay_ms: 5000]}
  ], strategy: :one_for_one)
  ```

  The delay should be long enough to allow long running requests to finish, and smaller than
  the [K8S grace period](https://kubernetes.io/docs/concepts/workloads/pods/pod/#termination-of-pods)
  (30 seconds by default), else K8S will terminate the VM with `SIGKILL` before it can shut down
  gracefully. If not specified the delay is 20,000ms.

  You may not want to install the handler in `:dev` environment, since it will delay stopping the
  app: use environment specific configuation to set a lower delay or exclude the handler.
  """

  if(hd(:erlang.system_info(:otp_release)) < ?2, do: raise("#{__MODULE__} Needs OTP 20+"))

  require Logger

  @table_name __MODULE__

  @doc """
  Start the signal handler.

  Delegates to `k8s_signal_handler` module (written in Erlang).

  Returns `:ignore` (which is a NOP as far as supervisors are concerned), because
  the process is actually started, and supervised, by `erl_signal_server`.

  ## Options
  * `shutdown_delay_ms` - milliseconds between start of connection draining and ordered
    shut-down using `System.stop/0`.
  * `test_mode` - for testing: for details see `k8s_signal_handler.erl`.

  """
  def start_link(opts) do
    Logger.debug(fn -> "#{__MODULE__} start_link #{inspect opts}" end)
    delay = Keyword.get(opts, :shutdown_delay_ms, 20_000)
    test_mode = Keyword.get(opts, :test_mode, false)

    :k8s_signal_handler.start_link([@table_name, delay, test_mode])
  end

  @doc "child spec for use with supervisor"
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]}
    }
  end

  @doc "Has 'connection draining' mode has been entered?"
  @spec draining?() :: boolean
  def draining? do
    case :ets.lookup(@table_name, :draining) do
      [] -> false
      [{:draining, draining}] -> draining
    end
  end

  @doc "Start draining; primarily for testing."
  def start_draining do
    :gen_event.notify(:erl_signal_server, :sigterm)
  end

end
