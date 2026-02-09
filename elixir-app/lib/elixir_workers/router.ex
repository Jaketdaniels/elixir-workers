defmodule ElixirWorkers.Router do
  def handle(%{"method" => "GET", "url" => "/"} = _req) do
    %{
      "status" => 200,
      "headers" => %{"content-type" => "text/html; charset=utf-8"},
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
      "headers" => %{"content-type" => "application/json"},
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "status" => "ok",
          "runtime" => "atomvm-wasi",
          "platform" => "cloudflare-workers"
        })
    }
  end

  def handle(%{"method" => "POST", "url" => "/api/echo", "body" => body, "headers" => hdrs} = _req) do
    %{
      "status" => 200,
      "headers" => %{"content-type" => "application/json"},
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "echo" => body,
          "method" => "POST",
          "headers" => hdrs
        })
    }
  end

  def handle(%{"method" => method, "url" => url} = _req) do
    %{
      "status" => 404,
      "headers" => %{"content-type" => "application/json"},
      "body" =>
        ElixirWorkers.JSON.encode(%{
          "error" => "not_found",
          "method" => method,
          "path" => url
        })
    }
  end

  def handle(_req) do
    %{
      "status" => 400,
      "headers" => %{"content-type" => "application/json"},
      "body" => ElixirWorkers.JSON.encode(%{"error" => "bad_request"})
    }
  end
end
