import wasm from "../atomvm.wasm";
import avm from "../app.avm";

const E = new TextEncoder(), D = new TextDecoder();
let A = null;
const getAvm = () => (A ??= new Uint8Array(avm));

class X extends Error { constructor(c) { super(); this.code = c; } }

const S = () => 0, N = () => 52;
const env = {
  dist_send_message: S, dist_send_unlink_id_ack: S, dist_send_payload_exit: S,
  ets_delete_owned_tables: S, ets_init: S, ets_destroy: S, dist_spawn_reply: S,
  dist_monitor: S, ets_delete: S, ets_drop_table: S, ets_insert: S,
  ets_create_table_maybe_gc: S, ets_update_counter_maybe_gc: S,
  dist_send_link: S, dist_send_unlink_id: S, ets_lookup_maybe_gc: S,
  ets_lookup_element_maybe_gc: S,
};

function mkWasi(stdin, args) {
  const sin = E.encode(stdin);
  let sp = 0, sout = new Uint8Array(65536), soutP = 0;
  const files = new Map(), fds = new Map();
  let nfd = 4, mem;

  const v = () => new DataView(mem.buffer);
  const b = () => new Uint8Array(mem.buffer);
  const resolvePath = (pp, pl) => {
    let p = D.decode(b().subarray(pp, pp + pl));
    if (p[0] === "." && p[1] === "/") p = p.substring(2);
    else if (p[0] === "/") p = p.substring(1);
    return p;
  };

  return {
    setMem(m) { mem = m; },
    addFile(p, d) { files.set(p, d); },
    stdout() { return D.decode(sout.subarray(0, soutP)); },
    imports: {
      args_get(ap, bp) {
        const dv = v(), buf = b();
        for (const a of args) {
          dv.setUint32(ap, bp, true); ap += 4;
          const e = E.encode(a + "\0");
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
      proc_exit(c) { throw new X(c); },
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

const MAX_BODY_SIZE = 1024 * 1024; // 1 MB

export default {
  async fetch(request) {
    try {
      const url = new URL(request.url);
      const h = {}; request.headers.forEach((v, k) => { h[k] = v; });
      let body = "";
      if (request.body) {
        const cl = request.headers.get("content-length");
        if (cl && parseInt(cl, 10) > MAX_BODY_SIZE) return new Response(JSON.stringify({ error: "payload too large" }), { status: 413, headers: { "content-type": "application/json" } });
        body = await request.text();
        if (E.encode(body).length > MAX_BODY_SIZE) return new Response(JSON.stringify({ error: "payload too large" }), { status: 413, headers: { "content-type": "application/json" } });
      }
      const json = JSON.stringify({ method: request.method, url: url.pathname + url.search, headers: h, body });

      const w = mkWasi(json, ["atomvm", "app.avm"]);
      w.addFile("app.avm", getAvm());
      const inst = await WebAssembly.instantiate(wasm, { wasi_snapshot_preview1: w.imports, env });
      w.setMem(inst.exports.memory);

      try { inst.exports._start(); } catch (e) {
        if (e instanceof X) { if (e.code !== 0) { console.error("WASM exit " + e.code + ": " + w.stdout().substring(0, 500)); throw new Error("runtime error"); } }
        else throw e;
      }

      const out = w.stdout(), i = out.indexOf("{");
      if (i < 0) return new Response(JSON.stringify({ error: "no output" }), { status: 502, headers: { "content-type": "application/json" } });

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
      if (end < 0) return new Response(JSON.stringify({ error: "incomplete" }), { status: 502, headers: { "content-type": "application/json" } });

      const r = JSON.parse(out.substring(i, end + 1));
      return new Response(r.body, { status: r.status, headers: r.headers });
    } catch (e) {
      console.error("Worker error:", e.message || "unknown", e.stack || "");
      return new Response(JSON.stringify({ error: "internal server error" }), { status: 500, headers: { "content-type": "application/json" } });
    }
  },
};
