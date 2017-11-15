defmodule FT.K8S.TrafficDrainPlug do
  @moduledoc """
  Module that starts responding with 500 from `/__traffic`, when traffic is being drained.

  Add the plug to your Phoenix Endpoint or Plug pipeline:

  ```
  plug FT.K8S.TrafficDrainPlug  # NB serves /__traffic only
  ```

  > `FT.K8S.TrafficDrainHandler` must be also be started, or else this plug will fail at runtime.

  ## Kubernetes Configuration

  Use the `/__traffic` endpoint as the `readinessProbe` in your Pod's configuration, e.g.:

  ```YAML
      containers:
        name: ...
        image: ...
        ports:
        - containerPort: 8080
        readinessProbe:
          httpGet:
            path: /__traffic
            port: 8080
          initialDelaySeconds: 20
          periodSeconds: 2
  ```

  Note that the `periodSeconds` above means that `/__traffic` will be polled every 2 seconds, which you
  should take into acount when setting the delay in `FT.K8S.TrafficDrainHandler`, so as not to shut-down
  before the K8S load-balancers have noticed that the endpoint has started rejecting traffic, allowing
  any pending requests to complete thereafter.
  """

  import Plug.Conn

  @behaviour Plug

  @impl true
  def init(options), do: options

  @impl true
  def call(conn = %{path_info: ["__traffic"]}, _config) do
    draining = FT.K8S.TrafficDrainHandler.draining?()

    case draining do
      true ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(500, "Draining")
        |> halt
      false ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(200, "Serving")
        |> halt
    end
  end

  @impl true
  def call(conn, _config), do: conn

end
