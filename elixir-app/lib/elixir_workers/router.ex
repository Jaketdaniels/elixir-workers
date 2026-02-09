defmodule ElixirWorkers.Router do
  @security_headers %{
    "x-content-type-options" => "nosniff",
    "x-frame-options" => "DENY",
    "strict-transport-security" => "max-age=31536000; includeSubDomains"
  }

  @sensitive_headers [
    "authorization", "cookie", "set-cookie",
    "x-forwarded-for", "x-real-ip",
    "cf-connecting-ip", "cf-ipcountry", "cf-ray",
    "cf-visitor", "cf-worker", "cf-access-jwt-assertion",
    "true-client-ip"
  ]
  @sensitive_headers_set MapSet.new(@sensitive_headers)

  def handle(%{"method" => "GET", "url" => "/"} = _req) do
    %{
      "status" => 200,
      "headers" => :maps.merge(@security_headers, %{
        "content-type" => "text/html; charset=utf-8",
        "content-security-policy" => "default-src 'none'; style-src 'unsafe-inline'"
      }),
      "body" =>
        <<"<!DOCTYPE html><html><head><title>Elixir Workers</title></head><body>",
          "<h1>Running Elixir on Cloudflare Workers</h1>",
          "<p>Powered by AtomVM + WASI</p>",
          "</body></html>">>
    }
  end

  def handle(%{"method" => "GET", "url" => "/api/health"} = _req) do
    %{
      "status" => 200,
      "headers" => :maps.merge(@security_headers, %{"content-type" => "application/json"}),
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "status" => "ok",
          "runtime" => "atomvm-wasi",
          "platform" => "cloudflare-workers"
        })
    }
  end

  def handle(%{"method" => "POST", "url" => "/api/echo", "body" => body, "headers" => hdrs} = _req) do
    safe_hdrs = filter_sensitive_headers(hdrs)

    %{
      "status" => 200,
      "headers" => :maps.merge(@security_headers, %{"content-type" => "application/json"}),
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "echo" => body,
          "method" => "POST",
          "headers" => safe_hdrs
        })
    }
  end

  def handle(%{"method" => method} = _req) do
    %{
      "status" => 404,
      "headers" => :maps.merge(@security_headers, %{"content-type" => "application/json"}),
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "error" => "not_found",
          "method" => method
        })
    }
  end

  def handle(_req) do
    %{
      "status" => 400,
      "headers" => :maps.merge(@security_headers, %{"content-type" => "application/json"}),
      "body" => ElixirWorkers.JSON.encode(%{"error" => "bad_request"})
    }
  end

  defp filter_sensitive_headers(hdrs) when is_map(hdrs) do
    :maps.filter(fn key, _val ->
      MapSet.member?(@sensitive_headers_set, String.downcase(to_string(key))) == false
    end, hdrs)
  end

  defp filter_sensitive_headers(hdrs), do: hdrs
end
