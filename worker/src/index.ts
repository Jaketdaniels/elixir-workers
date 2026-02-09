import atomvmModule from "../atomvm.wasm";
import appAvm from "../app.avm";

const enc = new TextEncoder();
const dec = new TextDecoder();

let avmCache: Uint8Array | null = null;
function getAvm(): Uint8Array {
  return (avmCache ??= new Uint8Array(appAvm as ArrayBuffer));
}

class WasiExit extends Error {
  code: number;
  constructor(c: number) {
    super();
    this.code = c;
  }
}

class Wasi {
  mem!: WebAssembly.Memory;
  private sin: Uint8Array;
  private sp = 0;
  private sout: Uint8Array;
  private soutPos = 0;
  private files = new Map<string, Uint8Array>();
  private fds = new Map<number, { d: Uint8Array; p: number }>();
  private nfd = 4;
  private args: string[];

  constructor(stdin: string, args: string[]) {
    this.sin = enc.encode(stdin);
    this.sout = new Uint8Array(65536);
    this.args = args;
  }

  addFile(p: string, d: Uint8Array) {
    this.files.set(p, d);
  }

  stdout(): string {
    return dec.decode(this.sout.subarray(0, this.soutPos));
  }

  private v() {
    return new DataView(this.mem.buffer);
  }
  private b() {
    return new Uint8Array(this.mem.buffer);
  }

  get imports(): Record<string, Function> {
    return {
      args_get: (ap: number, bp: number) => {
        const v = this.v(), b = this.b();
        for (const a of this.args) {
          v.setUint32(ap, bp, true);
          ap += 4;
          const e = enc.encode(a + "\0");
          b.set(e, bp);
          bp += e.length;
        }
        return 0;
      },
      args_sizes_get: (cp: number, sp: number) => {
        const v = this.v();
        let s = 0;
        for (const a of this.args) s += enc.encode(a + "\0").length;
        v.setUint32(cp, this.args.length, true);
        v.setUint32(sp, s, true);
        return 0;
      },
      clock_time_get: (_: number, __: bigint, r: number) => {
        this.v().setBigUint64(r, BigInt(Date.now()) * 1000000n, true);
        return 0;
      },
      clock_res_get: (_: number, r: number) => {
        this.v().setBigUint64(r, 1000000n, true);
        return 0;
      },
      environ_get: () => 0,
      environ_sizes_get: (cp: number, sp: number) => {
        const v = this.v();
        v.setUint32(cp, 0, true);
        v.setUint32(sp, 0, true);
        return 0;
      },
      fd_close: (fd: number) => {
        this.fds.delete(fd);
        return 0;
      },
      fd_fdstat_get: (fd: number, r: number) => {
        const v = this.v(), b = this.b();
        b.fill(0, r, r + 24);
        if (fd <= 2) v.setUint8(r, 2);
        else if (fd === 3) v.setUint8(r, 3);
        else if (this.fds.has(fd)) v.setUint8(r, 4);
        else return 8;
        v.setBigUint64(r + 8, 0xFFFFFFFFFFFFFFFFn, true);
        v.setBigUint64(r + 16, 0xFFFFFFFFFFFFFFFFn, true);
        return 0;
      },
      fd_read: (fd: number, iovs: number, iovsLen: number, rp: number) => {
        const v = this.v(), b = this.b();
        let tot = 0;
        for (let i = 0; i < iovsLen; i++) {
          const ptr = v.getUint32(iovs + i * 8, true);
          const len = v.getUint32(iovs + i * 8 + 4, true);
          if (fd === 0) {
            const n = Math.min(len, this.sin.length - this.sp);
            b.set(this.sin.subarray(this.sp, this.sp + n), ptr);
            this.sp += n;
            tot += n;
            if (n < len) break;
          } else {
            const f = this.fds.get(fd);
            if (!f) return 8;
            const n = Math.min(len, f.d.length - f.p);
            b.set(f.d.subarray(f.p, f.p + n), ptr);
            f.p += n;
            tot += n;
            if (n < len) break;
          }
        }
        v.setUint32(rp, tot, true);
        return 0;
      },
      fd_seek: (fd: number, off: bigint, w: number, rp: number) => {
        const f = this.fds.get(fd);
        if (!f) return 8;
        const o = Number(off);
        if (w === 0) f.p = o;
        else if (w === 1) f.p += o;
        else f.p = f.d.length + o;
        this.v().setBigUint64(rp, BigInt(f.p), true);
        return 0;
      },
      fd_write: (fd: number, iovs: number, iovsLen: number, rp: number) => {
        const v = this.v(), b = this.b();
        let tot = 0;
        for (let i = 0; i < iovsLen; i++) {
          const ptr = v.getUint32(iovs + i * 8, true);
          const len = v.getUint32(iovs + i * 8 + 4, true);
          if (fd === 1 || fd === 2) {
            if (this.soutPos + len > this.sout.length) {
              const ns = new Uint8Array(this.sout.length * 2);
              ns.set(this.sout);
              this.sout = ns;
            }
            this.sout.set(b.subarray(ptr, ptr + len), this.soutPos);
            this.soutPos += len;
            tot += len;
          } else return 8;
        }
        v.setUint32(rp, tot, true);
        return 0;
      },
      fd_prestat_get: (fd: number, r: number) => {
        if (fd !== 3) return 8;
        const v = this.v();
        v.setUint32(r, 0, true);
        v.setUint32(r + 4, 1, true);
        return 0;
      },
      fd_prestat_dir_name: (fd: number, p: number) => {
        if (fd !== 3) return 8;
        this.b()[p] = 47;
        return 0;
      },
      path_open: (_: number, __: number, pp: number, pl: number, ___: number, ____: bigint, _____: bigint, ______: number, rp: number) => {
        let p = dec.decode(this.b().subarray(pp, pp + pl));
        if (p.startsWith("./")) p = p.substring(2);
        if (p.startsWith("/")) p = p.substring(1);
        const d = this.files.get(p);
        if (!d) return 44;
        const fd = this.nfd++;
        this.fds.set(fd, { d, p: 0 });
        this.v().setUint32(rp, fd, true);
        return 0;
      },
      path_filestat_get: (_: number, __: number, pp: number, pl: number, r: number) => {
        let p = dec.decode(this.b().subarray(pp, pp + pl));
        if (p.startsWith("./")) p = p.substring(2);
        if (p.startsWith("/")) p = p.substring(1);
        const d = this.files.get(p);
        if (!d) return 44;
        const b = this.b();
        b.fill(0, r, r + 64);
        const v = this.v();
        v.setUint8(r + 16, 4);
        v.setBigUint64(r + 32, BigInt(d.length), true);
        return 0;
      },
      proc_exit: (c: number): never => {
        throw new WasiExit(c);
      },
      random_get: (p: number, l: number) => {
        crypto.getRandomValues(new Uint8Array(this.mem.buffer, p, l));
        return 0;
      },
      sched_yield: () => 0,
      poll_oneoff: () => 0,
      proc_raise: () => 52,
      sock_recv: () => 52,
      sock_send: () => 52,
      sock_shutdown: () => 52,
      fd_advise: () => 0,
      fd_allocate: () => 0,
      fd_datasync: () => 0,
      fd_fdstat_set_flags: () => 0,
      fd_fdstat_set_rights: () => 0,
      fd_filestat_get: () => 8,
      fd_filestat_set_size: () => 0,
      fd_filestat_set_times: () => 0,
      fd_pread: () => 52,
      fd_pwrite: () => 52,
      fd_readdir: () => 52,
      fd_renumber: () => 52,
      fd_sync: () => 0,
      fd_tell: () => 52,
      path_create_directory: () => 52,
      path_filestat_set_times: () => 52,
      path_link: () => 52,
      path_readlink: () => 52,
      path_remove_directory: () => 52,
      path_rename: () => 52,
      path_symlink: () => 52,
      path_unlink_file: () => 52,
    };
  }
}

const envStubs: Record<string, Function> = {
  dist_send_message: () => 0, dist_send_unlink_id_ack: () => 0,
  dist_send_payload_exit: () => 0, ets_delete_owned_tables: () => 0,
  ets_init: () => 0, ets_destroy: () => 0, dist_spawn_reply: () => 0,
  dist_monitor: () => 0, ets_delete: () => 0, ets_drop_table: () => 0,
  ets_insert: () => 0, ets_create_table_maybe_gc: () => 0,
  ets_update_counter_maybe_gc: () => 0, dist_send_link: () => 0,
  dist_send_unlink_id: () => 0, ets_lookup_maybe_gc: () => 0,
  ets_lookup_element_maybe_gc: () => 0,
};

function errResp(msg: string, raw: string): Response {
  return new Response(
    JSON.stringify({ error: msg, raw_output: raw.substring(0, 500) }),
    { status: 502, headers: { "content-type": "application/json" } }
  );
}

export default {
  async fetch(request: Request): Promise<Response> {
    try {
      const url = new URL(request.url);
      const headers: Record<string, string> = {};
      request.headers.forEach((v, k) => { headers[k] = v; });
      const body = request.body ? await request.text() : "";

      const json = JSON.stringify({
        method: request.method,
        url: url.pathname + url.search,
        headers,
        body,
      });

      const wasi = new Wasi(json, ["atomvm", "app.avm"]);
      wasi.addFile("app.avm", getAvm());

      const inst = await WebAssembly.instantiate(atomvmModule, {
        wasi_snapshot_preview1: wasi.imports,
        env: envStubs,
      });
      wasi.mem = inst.exports.memory as WebAssembly.Memory;

      try {
        (inst.exports._start as Function)();
      } catch (e) {
        if (e instanceof WasiExit) {
          if (e.code !== 0)
            throw new Error(`exit ${e.code}: ${wasi.stdout().substring(0, 500)}`);
        } else throw e;
      }

      const out = wasi.stdout();
      const i = out.indexOf("{");
      if (i === -1) return errResp("No JSON in output", out);

      let depth = 0, inStr = false, esc = false, end = -1;
      for (let j = i; j < out.length; j++) {
        const c = out[j];
        if (esc) { esc = false; continue; }
        if (c === "\\") { esc = true; continue; }
        if (c === '"') { inStr = !inStr; continue; }
        if (inStr) continue;
        if (c === "{") depth++;
        if (c === "}") { depth--; if (depth === 0) { end = j; break; } }
      }
      if (end === -1) return errResp("Incomplete JSON", out);

      const resp = JSON.parse(out.substring(i, end + 1));
      return new Response(resp.body, { status: resp.status, headers: resp.headers });
    } catch (e) {
      return new Response(
        JSON.stringify({ error: "AtomVM failed", details: e instanceof Error ? e.message : "unknown" }),
        { status: 500, headers: { "content-type": "application/json" } }
      );
    }
  },
};
