# Security Policy

## Supported Versions

We release security patches for the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in elixir-workers, please report it by emailing the maintainers or opening a private security advisory on GitHub.

**Please do not open public issues for security vulnerabilities.**

We will acknowledge receipt of your vulnerability report within 48 hours and send you regular updates about our progress. If you're curious about the status of your disclosure please feel free to email us again.

## Security Features

### Input Validation

- **Request Body Size Limit**: The framework enforces a 1 MB limit on request bodies to prevent memory exhaustion attacks.
- **JSON Depth Limit**: JSON parsing is limited to 32 levels of nesting to prevent stack overflow attacks.
- **URL Percent Decoding**: Malformed percent-encoded sequences are handled safely without crashing.

### Memory Safety (C/WASM)

- **Buffer Overflow Protection**: The C stdin reader includes checks for integer overflow and enforces a 10 MB maximum input size.
- **Memory Allocation Limits**: All dynamic memory allocations are checked for success and bounded.

### Security Headers

The default middleware applies the following security headers:

- `X-Content-Type-Options: nosniff` - Prevents MIME type sniffing
- `X-Frame-Options: DENY` - Prevents clickjacking attacks
- `Strict-Transport-Security: max-age=31536000; includeSubDomains` - Enforces HTTPS
- `Referrer-Policy: strict-origin-when-cross-origin` - Controls referrer information
- `Permissions-Policy: geolocation=(), microphone=(), camera=()` - Restricts browser features
- `Content-Security-Policy: default-src 'self'; ...` - Prevents XSS attacks

### XSS Protection

- **HTML Escaping**: The `ElixirWorkers.HTML.escape/1` function properly escapes the following characters:
  - `&` → `&amp;`
  - `<` → `&lt;`
  - `>` → `&gt;`
  - `"` → `&quot;`
  - `'` → `&#39;`

### CORS Configuration

The CORS middleware is opt-in and provides configurable controls:

- `origin` - Allowed origins (default: `*`, should be restricted in production)
- `methods` - Allowed HTTP methods
- `headers` - Allowed request headers
- `max_age` - Preflight cache duration

**Security Recommendation**: Always configure CORS with specific origins in production:

```elixir
def middleware do
  [
    &ElixirWorkers.Middleware.security_headers/1,
    &ElixirWorkers.Middleware.cors(&1, %{
      "origin" => "https://yourapp.com",
      "methods" => "GET,POST",
      "headers" => "content-type,authorization"
    }),
    &ElixirWorkers.Middleware.parse_body/1
  ]
end
```

## Best Practices

### 1. Keep Dependencies Updated

Always use the latest version of the framework and its dependencies:

```bash
mix deps.update elixir_workers
npm update wrangler
```

### 2. Validate All User Input

Never trust user input. Always validate and sanitize:

```elixir
def create_user(conn) do
  params = conn["parsed_body"]
  
  # Validate input
  cond do
    not is_binary(params["email"]) -> 
      Conn.send_resp(conn, 400, "Invalid email")
    not is_binary(params["name"]) or byte_size(params["name"]) > 100 ->
      Conn.send_resp(conn, 400, "Invalid name")
    true ->
      # Process valid input
      ...
  end
end
```

### 3. Use Parameterized Queries

When using D1 databases, always use parameterized queries to prevent SQL injection:

```elixir
# Good - parameterized query
ElixirWorkers.D1.query("DB", "SELECT * FROM users WHERE email = ?", [email])

# Bad - string interpolation (vulnerable to SQL injection)
ElixirWorkers.D1.query("DB", "SELECT * FROM users WHERE email = '#{email}'", [])
```

### 4. Sanitize HTML Output

Always escape user-generated content when rendering HTML:

```elixir
import ElixirWorkers.HTML

def render_comment(comment) do
  tag("div", %{"class" => "comment"}, escape(comment))
end
```

### 5. Configure Security Headers

Customize the Content-Security-Policy for your application's needs:

```elixir
def security_headers(conn) do
  conn
  |> Conn.put_resp_header("content-security-policy", 
       "default-src 'self'; script-src 'self' 'unsafe-inline' https://cdn.example.com")
end
```

### 6. Restrict CORS Origins

In production, never use wildcard (`*`) for CORS origins:

```elixir
# Development (permissive)
ElixirWorkers.Middleware.cors(conn, %{"origin" => "*"})

# Production (restrictive)
ElixirWorkers.Middleware.cors(conn, %{"origin" => "https://yourapp.com"})
```

### 7. Use HTTPS in Production

Configure Cloudflare Workers to enforce HTTPS:

```jsonc
// wrangler.jsonc
{
  "compatibility_flags": ["nodejs_compat"],
  "dev": {
    "port": 8797
  }
  // Cloudflare automatically enforces HTTPS in production
}
```

### 8. Implement Rate Limiting

Consider implementing rate limiting using Cloudflare's built-in features or custom logic:

```elixir
def rate_limit(conn) do
  ip = get_in(conn, ["cf", "ip"]) || "unknown"
  # Implement your rate limiting logic here
  # Consider using KV to track request counts per IP
  conn
end
```

### 9. Handle Secrets Securely

Never commit secrets to your repository. Use Cloudflare Workers environment variables:

```bash
# Set secrets via wrangler CLI
wrangler secret put API_KEY

# Access in Elixir
api_key = conn["env"]["API_KEY"]
```

### 10. Monitor and Log Security Events

Log security-relevant events for auditing:

```elixir
def handle_auth(conn) do
  case authenticate(conn) do
    {:ok, user} ->
      # Continue processing
      ...
    {:error, reason} ->
      # Log failed authentication attempt
      IO.puts("Auth failed: #{reason} from IP: #{get_in(conn, ["cf", "ip"])}")
      Conn.send_resp(conn, 401, "Unauthorized")
  end
end
```

## Known Limitations

### 1. No Built-in Authentication

The framework does not provide built-in authentication. You must implement your own authentication logic or integrate with third-party services.

### 2. No Built-in Rate Limiting

Rate limiting must be implemented by the application developer or using Cloudflare's Rate Limiting features.

### 3. Single-Threaded Execution

Each request creates a fresh WASM instance with no shared state. This prevents certain classes of concurrency bugs but also means rate limiting requires external storage (KV).

### 4. Limited Cryptography

AtomVM has limited support for cryptographic operations. For sensitive operations, consider:
- Using Cloudflare Workers' Web Crypto API (not yet exposed to Elixir)
- Delegating to external authentication services
- Using pre-signed URLs for sensitive resources

## Security Checklist

Before deploying to production:

- [ ] Update all dependencies to their latest versions
- [ ] Configure restrictive CORS origins
- [ ] Implement input validation for all user inputs
- [ ] Use parameterized queries for database operations
- [ ] Escape all user-generated HTML content
- [ ] Configure appropriate Content-Security-Policy
- [ ] Store secrets in environment variables, not in code
- [ ] Implement authentication and authorization
- [ ] Consider implementing rate limiting
- [ ] Enable Cloudflare's WAF (Web Application Firewall)
- [ ] Review and test error handling to avoid information leakage
- [ ] Set up monitoring and alerting for security events

## Additional Resources

- [OWASP Top Ten](https://owasp.org/www-project-top-ten/)
- [Cloudflare Workers Security](https://developers.cloudflare.com/workers/platform/security/)
- [Elixir Security Guidelines](https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html#security)
