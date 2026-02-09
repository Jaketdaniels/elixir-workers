import wasm from "../atomvm.wasm";
import avm from "../app.avm";

const encoder = new TextEncoder(), decoder = new TextDecoder();
let A = null;
const getAvm = () => (A ??= new Uint8Array(avm));

class WasmExit extends Error { constructor(c) { super(); this.code = c; } }

const S = () => 0, N = () => 52;
const envStubs = {
  dist_send_message: S, dist_send_unlink_id_ack: S, dist_send_payload_exit: S,
  ets_delete_owned_tables: S, ets_init: S, ets_destroy: S, dist_spawn_reply: S,
  dist_monitor: S, ets_delete: S, ets_drop_table: S, ets_insert: S,
  ets_create_table_maybe_gc: S, ets_update_counter_maybe_gc: S,
  dist_send_link: S, dist_send_unlink_id: S, ets_lookup_maybe_gc: S,
  ets_lookup_element_maybe_gc: S,
};

const FAVICON_SVG = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="-15 -8 155 145"><linearGradient id="a" gradientUnits="userSpaceOnUse" x1="835.592" y1="-36.546" x2="821.211" y2="553.414" gradientTransform="matrix(.1297 0 0 .2 -46.03 17.198)"><stop offset="0" stop-color="#d9d8dc"/><stop offset="1" stop-color="#fff" stop-opacity=".385"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#a)" d="M64.4.5C36.7 13.9 1.9 83.4 30.9 113.9c26.8 33.5 85.4 1.3 68.4-40.5-21.5-36-35-37.9-34.9-72.9z"/><linearGradient id="b" gradientUnits="userSpaceOnUse" x1="942.357" y1="-40.593" x2="824.692" y2="472.243" gradientTransform="matrix(.1142 0 0 .2271 -47.053 17.229)"><stop offset="0" stop-color="#8d67af" stop-opacity=".672"/><stop offset="1" stop-color="#9f8daf"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#b)" d="M64.4.2C36.8 13.6 1.9 82.9 31 113.5c10.7 12.4 28 16.5 37.7 9.1 26.4-18.8 7.4-53.1 10.4-78.5C68.1 33.9 64.2 11.3 64.4.2z"/><linearGradient id="c" gradientUnits="userSpaceOnUse" x1="924.646" y1="120.513" x2="924.646" y2="505.851" gradientTransform="matrix(.1227 0 0 .2115 -46.493 17.206)"><stop offset="0" stop-color="#26053d" stop-opacity=".762"/><stop offset="1" stop-color="#b7b4b4" stop-opacity=".278"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#c)" d="M56.7 4.3c-22.3 15.9-28.2 75-24.1 94.2 8.2 48.1 75.2 28.3 69.6-16.5-6-29.2-48.8-39.2-45.5-77.7z"/><linearGradient id="d" gradientUnits="userSpaceOnUse" x1="428.034" y1="198.448" x2="607.325" y2="559.255" gradientTransform="matrix(.1848 0 0 .1404 -42.394 17.138)"><stop offset="0" stop-color="#91739f" stop-opacity=".46"/><stop offset="1" stop-color="#32054f" stop-opacity=".54"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#d)" d="M78.8 49.8c10.4 13.4 12.7 22.6 6.8 27.9-27.7 19.4-61.3 7.4-54-37.3C22.1 63 4.5 96.8 43.3 101.6c20.8 3.6 54 2 58.9-16.1-.2-15.9-10.8-22.9-23.4-35.7z"/><linearGradient id="e" gradientUnits="userSpaceOnUse" x1="907.895" y1="540.636" x2="590.242" y2="201.281" gradientTransform="matrix(.1418 0 0 .1829 -45.23 17.18)"><stop offset="0" stop-color="#463d49" stop-opacity=".331"/><stop offset="1" stop-color="#340a50" stop-opacity=".821"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#e)" d="M38.1 36.4c-2.9 21.2 35.1 77.9 58.3 71-17.7 35.6-56.9-21.2-64-41.7 1.5-11 2.2-16.4 5.7-29.3z"/><linearGradient id="f" gradientUnits="userSpaceOnUse" x1="1102.297" y1="100.542" x2="1008.071" y2="431.648" gradientTransform="matrix(.106 0 0 .2448 -47.595 17.242)"><stop offset="0" stop-color="#715383" stop-opacity=".145"/><stop offset="1" stop-color="#f4f4f4" stop-opacity=".234"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#f)" d="M60.4 49.7c.8 7.9 3.9 20.5 0 28.8S38.7 102 43.6 115.3c11.4 24.8 37.1-4.4 36.9-19 1.1-11.8-6.6-38.7-1.8-52.5L76.5 41l-13.6-4c-2.2 3.2-3 7.5-2.5 12.7z"/><linearGradient id="g" gradientUnits="userSpaceOnUse" x1="1354.664" y1="140.06" x2="1059.233" y2="84.466" gradientTransform="matrix(.09173 0 0 .2828 -48.536 17.28)"><stop offset="0" stop-color="#a5a1a8" stop-opacity=".356"/><stop offset="1" stop-color="#370c50" stop-opacity=".582"/></linearGradient><path fill-rule="evenodd" clip-rule="evenodd" fill="url(#g)" d="M65.3 10.8C36 27.4 48 53.4 49.3 81.6l19.1-55.4c-1.4-5.7-2.3-9.5-3.1-15.4z"/><path fill-rule="evenodd" clip-rule="evenodd" fill="#330A4C" fill-opacity=".316" d="M68.3 26.1c-14.8 11.7-14.1 31.3-18.6 54 8.1-21.3 4.1-38.2 18.6-54z"/><path fill-rule="evenodd" clip-rule="evenodd" fill="#FFF" d="M45.8 119.7c8 1.1 12.1 2.2 12.5 3 .3 4.2-11.1 1.2-12.5-3z"/><path fill-rule="evenodd" clip-rule="evenodd" fill="#EDEDED" fill-opacity=".603" d="M49.8 10.8c-6.9 7.7-14.4 21.8-18.2 29.7-1 6.5-.5 15.7.6 23.5.9-18.2 7.5-39.2 17.6-53.2z"/></svg>`;

// --- WASI Runtime ---

function mkWasi(stdin, args) {
  const sin = encoder.encode(stdin);
  let sp = 0, sout = new Uint8Array(65536), soutP = 0;
  const files = new Map(), fds = new Map();
  let nfd = 4, mem;

  const v = () => new DataView(mem.buffer);
  const b = () => new Uint8Array(mem.buffer);
  const resolvePath = (pp, pl) => {
    let p = decoder.decode(b().subarray(pp, pp + pl));
    if (p[0] === "." && p[1] === "/") p = p.substring(2);
    else if (p[0] === "/") p = p.substring(1);
    return p;
  };

  return {
    setMem(m) { mem = m; },
    addFile(p, d) { files.set(p, d); },
    stdout() { return decoder.decode(sout.subarray(0, soutP)); },
    imports: {
      args_get(ap, bp) {
        const dv = v(), buf = b();
        for (const a of args) {
          dv.setUint32(ap, bp, true); ap += 4;
          const e = encoder.encode(a + "\0");
          buf.set(e, bp); bp += e.length;
        }
        return 0;
      },
      args_sizes_get(cp, sp) {
        const dv = v(); let s = 0;
        for (const a of args) s += a.length + 1;
        dv.setUint32(cp, args.length, true);
        dv.setUint32(sp, s, true);
        return 0;
      },
      clock_time_get(_, __, r) { v().setBigUint64(r, BigInt(Date.now()) * 1000000n, true); return 0; },
      clock_res_get(_, r) { v().setBigUint64(r, 1000000n, true); return 0; },
      environ_get: S,
      environ_sizes_get(cp, sp) { const dv = v(); dv.setUint32(cp, 0, true); dv.setUint32(sp, 0, true); return 0; },
      fd_close(fd) { fds.delete(fd); return 0; },
      fd_fdstat_get(fd, r) {
        const dv = v();
        b().fill(0, r, r + 24);
        if (fd <= 2) dv.setUint8(r, 2);
        else if (fd === 3) dv.setUint8(r, 3);
        else if (fds.has(fd)) dv.setUint8(r, 4);
        else return 8;
        dv.setBigUint64(r + 8, 0xFFFFFFFFFFFFFFFFn, true);
        dv.setBigUint64(r + 16, 0xFFFFFFFFFFFFFFFFn, true);
        return 0;
      },
      fd_read(fd, iovs, iovsLen, rp) {
        const dv = v(), buf = b(); let tot = 0;
        for (let i = 0; i < iovsLen; i++) {
          const ptr = dv.getUint32(iovs + i * 8, true), len = dv.getUint32(iovs + i * 8 + 4, true);
          let src, srcP;
          if (fd === 0) { src = sin; srcP = sp; }
          else { const f = fds.get(fd); if (!f) return 8; src = f.d; srcP = f.p; }
          const n = Math.min(len, src.length - srcP);
          buf.set(src.subarray(srcP, srcP + n), ptr);
          if (fd === 0) sp += n; else fds.get(fd).p += n;
          tot += n;
          if (n < len) break;
        }
        dv.setUint32(rp, tot, true);
        return 0;
      },
      fd_seek(fd, off, w, rp) {
        const f = fds.get(fd); if (!f) return 8;
        const o = Number(off);
        f.p = w === 0 ? o : w === 1 ? f.p + o : f.d.length + o;
        v().setBigUint64(rp, BigInt(f.p), true);
        return 0;
      },
      fd_write(fd, iovs, iovsLen, rp) {
        const dv = v(), buf = b(); let tot = 0;
        for (let i = 0; i < iovsLen; i++) {
          const ptr = dv.getUint32(iovs + i * 8, true), len = dv.getUint32(iovs + i * 8 + 4, true);
          if (fd !== 1 && fd !== 2) return 8;
          if (soutP + len > sout.length) { const ns = new Uint8Array(sout.length * 2); ns.set(sout); sout = ns; }
          sout.set(buf.subarray(ptr, ptr + len), soutP);
          soutP += len; tot += len;
        }
        dv.setUint32(rp, tot, true);
        return 0;
      },
      fd_prestat_get(fd, r) {
        if (fd !== 3) return 8;
        const dv = v(); dv.setUint32(r, 0, true); dv.setUint32(r + 4, 1, true);
        return 0;
      },
      fd_prestat_dir_name(fd, p) { if (fd !== 3) return 8; b()[p] = 47; return 0; },
      path_open(_, __, pp, pl, ___, ____, _____, ______, rp) {
        const d = files.get(resolvePath(pp, pl)); if (!d) return 44;
        const fd = nfd++; fds.set(fd, { d, p: 0 });
        v().setUint32(rp, fd, true); return 0;
      },
      path_filestat_get(_, __, pp, pl, r) {
        const d = files.get(resolvePath(pp, pl)); if (!d) return 44;
        b().fill(0, r, r + 64);
        const dv = v(); dv.setUint8(r + 16, 4); dv.setBigUint64(r + 32, BigInt(d.length), true);
        return 0;
      },
      proc_exit(c) { throw new WasmExit(c); },
      random_get(p, l) { crypto.getRandomValues(new Uint8Array(mem.buffer, p, l)); return 0; },
      sched_yield: S, poll_oneoff: S, proc_raise: N,
      sock_recv: N, sock_send: N, sock_shutdown: N,
      fd_advise: S, fd_allocate: S, fd_datasync: S,
      fd_fdstat_set_flags: S, fd_fdstat_set_rights: S,
      fd_filestat_get() { return 8; },
      fd_filestat_set_size: S, fd_filestat_set_times: S,
      fd_pread: N, fd_pwrite: N, fd_readdir: N, fd_renumber: N,
      fd_sync: S, fd_tell: N,
      path_create_directory: N, path_filestat_set_times: N,
      path_link: N, path_readlink: N, path_remove_directory: N,
      path_rename: N, path_symlink: N, path_unlink_file: N,
    }
  };
}

// --- WASM Execution ---

function runWasm(stdinJson) {
  const w = mkWasi(stdinJson, ["atomvm", "app.avm"]);
  w.addFile("app.avm", getAvm());
  const inst = new WebAssembly.Instance(wasm, { wasi_snapshot_preview1: w.imports, env: envStubs });
  w.setMem(inst.exports.memory);

  try {
    inst.exports._start();
  } catch (e) {
    if (e instanceof WasmExit) {
      if (e.code !== 0) {
        console.error("WASM exit " + e.code + ": " + w.stdout().substring(0, 500));
        throw new Error("runtime error");
      }
    } else {
      throw e;
    }
  }

  return parseOutput(w.stdout());
}

// Parse the first complete JSON object from WASM stdout.
function parseOutput(out) {
  const i = out.indexOf("{");
  if (i < 0) return { error: "no output" };

  let d = 0, s = false, esc = false, end = -1;
  for (let j = i; j < out.length; j++) {
    const c = out[j];
    if (esc) { esc = false; continue; }
    if (c === "\\") { esc = true; continue; }
    if (c === '"') { s = !s; continue; }
    if (s) continue;
    if (c === "{") d++;
    if (c === "}") { d--; if (!d) { end = j; break; } }
  }
  if (end < 0) return { error: "incomplete" };

  return JSON.parse(out.substring(i, end + 1));
}

// --- Env Extraction ---

// Extract plain string env vars (skip KV/D1/R2 binding objects).
function extractEnvVars(workerEnv) {
  const vars = {};
  for (const key of Object.keys(workerEnv)) {
    const val = workerEnv[key];
    if (typeof val === "string" || typeof val === "number" || typeof val === "boolean") {
      vars[key] = String(val);
    }
  }
  return vars;
}

// --- Binding Fulfillment ---

// Fulfill all binding needs from a _needs response. Returns a bindings map.
async function fulfillNeeds(needs, workerEnv) {
  const bindings = {};

  const promises = needs.map(async (need) => {
    try {
      switch (need.type) {
        case "kv_get": {
          const ns = workerEnv[need.ns];
          if (!ns) { bindings[need.id] = null; return; }
          bindings[need.id] = await ns.get(need.key);
          break;
        }
        case "kv_get_meta": {
          const ns = workerEnv[need.ns];
          if (!ns) { bindings[need.id] = null; return; }
          const { value, metadata } = await ns.getWithMetadata(need.key);
          bindings[need.id] = { value, metadata };
          break;
        }
        case "kv_list": {
          const ns = workerEnv[need.ns];
          if (!ns) { bindings[need.id] = null; return; }
          const opts = {};
          if (need.prefix) opts.prefix = need.prefix;
          if (need.limit) opts.limit = need.limit;
          if (need.cursor) opts.cursor = need.cursor;
          const result = await ns.list(opts);
          bindings[need.id] = {
            keys: result.keys.map(k => ({ name: k.name, metadata: k.metadata })),
            list_complete: result.list_complete,
            cursor: result.cursor,
          };
          break;
        }
        case "d1_query": {
          const db = workerEnv[need.db];
          if (!db) { bindings[need.id] = null; return; }
          const stmt = db.prepare(need.sql);
          const res = need.params && need.params.length
            ? await stmt.bind(...need.params).all()
            : await stmt.all();
          bindings[need.id] = { rows: res.results };
          break;
        }
        default:
          console.warn("Unknown need type:", need.type);
          bindings[need.id] = null;
      }
    } catch (e) {
      console.error("Binding fulfillment error:", need.type, need.id, e.message);
      bindings[need.id] = null;
    }
  });

  await Promise.all(promises);
  return bindings;
}

// --- Effect Execution ---

// Execute write effects after the response is sent.
async function executeEffects(effects, workerEnv) {
  for (const eff of effects) {
    try {
      switch (eff.type) {
        case "kv_put": {
          const ns = workerEnv[eff.ns];
          if (!ns) break;
          const opts = {};
          if (eff.expiration_ttl) opts.expirationTtl = eff.expiration_ttl;
          if (eff.metadata) opts.metadata = eff.metadata;
          await ns.put(eff.key, eff.value, opts);
          break;
        }
        case "kv_delete": {
          const ns = workerEnv[eff.ns];
          if (!ns) break;
          await ns.delete(eff.key);
          break;
        }
        case "d1_exec": {
          const db = workerEnv[eff.db];
          if (!db) break;
          const stmt = db.prepare(eff.sql);
          if (eff.params && eff.params.length) await stmt.bind(...eff.params).run();
          else await stmt.run();
          break;
        }
        case "d1_batch": {
          const db = workerEnv[eff.db];
          if (!db) break;
          const stmts = eff.statements.map(s => {
            const stmt = db.prepare(s.sql);
            return s.params && s.params.length ? stmt.bind(...s.params) : stmt;
          });
          await db.batch(stmts);
          break;
        }
        default:
          console.warn("Unknown effect type:", eff.type);
      }
    } catch (e) {
      console.error("Effect execution error:", eff.type, e.message);
    }
  }
}

// --- Worker Entry Point ---

const MAX_BODY_SIZE = 1024 * 1024; // 1 MB

export default {
  async fetch(request, workerEnv, ctx) {
    try {
      const url = new URL(request.url);

      // Serve Elixir drop favicon
      if (url.pathname === "/favicon.ico" || url.pathname === "/favicon.svg") {
        return new Response(FAVICON_SVG, {
          headers: { "content-type": "image/svg+xml", "cache-control": "public, max-age=86400" },
        });
      }

      // Extract headers
      const h = {};
      request.headers.forEach((v, k) => { h[k] = v; });

      // Read body with size limit
      let body = "";
      if (request.body) {
        const cl = request.headers.get("content-length");
        if (cl && parseInt(cl, 10) > MAX_BODY_SIZE) {
          return new Response(JSON.stringify({ error: "payload too large" }), {
            status: 413, headers: { "content-type": "application/json" },
          });
        }
        body = await request.text();
        if (encoder.encode(body).length > MAX_BODY_SIZE) {
          return new Response(JSON.stringify({ error: "payload too large" }), {
            status: 413, headers: { "content-type": "application/json" },
          });
        }
      }

      // Build enriched request for Elixir
      const enrichedReq = {
        method: request.method,
        url: url.pathname + url.search,
        headers: h,
        body,
        env: extractEnvVars(workerEnv),
        cf: request.cf ? { ...request.cf } : {},
      };

      // Pass 1: run WASM
      let result = runWasm(JSON.stringify(enrichedReq));

      if (result.error) {
        return new Response(JSON.stringify(result), {
          status: 502, headers: { "content-type": "application/json" },
        });
      }

      // Pass 2: if Elixir needs binding data, fulfill and re-run
      if (result._needs && result._needs.length > 0) {
        const bindings = await fulfillNeeds(result._needs, workerEnv);

        enrichedReq.bindings = bindings;
        enrichedReq._state = result._state || {};

        result = runWasm(JSON.stringify(enrichedReq));

        if (result.error) {
          return new Response(JSON.stringify(result), {
            status: 502, headers: { "content-type": "application/json" },
          });
        }
      }

      // Execute write effects after response (non-blocking)
      if (result._effects && result._effects.length > 0) {
        ctx.waitUntil(executeEffects(result._effects, workerEnv));
      }

      // Return HTTP response
      const respHeaders = { ...result.headers };
      delete respHeaders._effects; // clean up internal fields
      return new Response(result.body, { status: result.status, headers: respHeaders });
    } catch (e) {
      console.error("Worker error:", e.message || "unknown", e.stack || "");
      return new Response(JSON.stringify({ error: "internal server error" }), {
        status: 500, headers: { "content-type": "application/json" },
      });
    }
  },
};
