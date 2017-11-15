# Kubernetes Traffic Plug

Support for graceful server shutdown under Kubernetes [termination life-cycle](s://kubernetes.io/docs/concepts/workloads/pods/pod/#termination-of-pods).

The plug supports a `/__traffic` endpoint which normally returns 200. When the VM 
receives a `SIGTERM` signal, the endpoint starts returning HTTP error code 500, 
which, when used with an HTTP `readinessProbe`, ensures that traffic is routed away 
from the pod, before a delayed graceful shutdown begins.

See modules for detailed documentation.

## Installation

```elixir
def deps do
  [
    {:k8s_traffic_plug, github: "Financial-Times/k8s_traffic_plug"}
  ]
end
```
