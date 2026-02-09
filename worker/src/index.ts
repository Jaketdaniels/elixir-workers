/**
 * Cloudflare Worker entry point for elixir-workers.
 *
 * Bridges HTTP requests to AtomVM running as a WASI WebAssembly module.
 *
 * Protocol:
 *   Request  → stdin as JSON: {"method","url","headers","body"}
 *   Response ← stdout as JSON: {"status","headers","body"}
 *
 * The WASI module (AtomVM + your Elixir .avm) runs per-request.
 * WASM compilation is cached by the runtime between requests.
 */

// Import the compiled AtomVM WASM module
import atomvmModule from "../atomvm.wasm";
// Import the packaged Elixir application
import appAvm from "../app.avm";

interface ElixirRequest {
  method: string;
  url: string;
  headers: Record<string, string>;
  body: string;
}

interface ElixirResponse {
  status: number;
  headers: Record<string, string>;
  body: string;
}

/**
 * Minimal WASI implementation for AtomVM.
 * Only implements what AtomVM actually uses: args, clock, random, stdio, proc_exit.
 */
class AtomVMWasi {
  private memory: WebAssembly.Memory | null = null;
  private stdin: Uint8Array;
  private stdinPos: number = 0;
  private stdout: Uint8Array[] = [];
  private args: string[];

  constructor(stdinData: string, args: string[]) {
    const encoder = new TextEncoder();
    this.stdin = encoder.encode(stdinData);
    this.args = args;
  }

  setMemory(memory: WebAssembly.Memory) {
    this.memory = memory;
  }

  getStdout(): string {
    const decoder = new TextDecoder();
    return this.stdout.map((chunk) => decoder.decode(chunk)).join("");
  }

  private view(): DataView {
    return new DataView(this.memory!.buffer);
  }

  /** WASI snapshot_preview1 imports */
  get imports(): Record<string, Function> {
    return {
      args_get: this.argsGet.bind(this),
      args_sizes_get: this.argsSizesGet.bind(this),
      clock_time_get: this.clockTimeGet.bind(this),
      clock_res_get: this.clockResGet.bind(this),
      environ_get: this.environGet.bind(this),
      environ_sizes_get: this.environSizesGet.bind(this),
      fd_close: this.fdClose.bind(this),
      fd_fdstat_get: this.fdFdstatGet.bind(this),
      fd_read: this.fdRead.bind(this),
      fd_seek: this.fdSeek.bind(this),
      fd_write: this.fdWrite.bind(this),
      fd_prestat_get: this.fdPrestatGet.bind(this),
      fd_prestat_dir_name: this.fdPrestatDirName.bind(this),
      path_open: this.pathOpen.bind(this),
      path_filestat_get: this.pathFilestatGet.bind(this),
      proc_exit: this.procExit.bind(this),
      random_get: this.randomGet.bind(this),
      sched_yield: () => 0,
      poll_oneoff: () => 0,
      proc_raise: () => 52, // ENOSYS
      sock_recv: () => 52,
      sock_send: () => 52,
      sock_shutdown: () => 52,
      fd_advise: () => 0,
      fd_allocate: () => 0,
      fd_datasync: () => 0,
      fd_fdstat_set_flags: () => 0,
      fd_fdstat_set_rights: () => 0,
      fd_filestat_get: () => 8, // EBADF
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

  /* Virtual filesystem: the .avm file */
  private files: Map<string, Uint8Array> = new Map();
  private openFds: Map<number, { data: Uint8Array; pos: number }> = new Map();
  private nextFd = 4; // 0=stdin, 1=stdout, 2=stderr, 3=preopen dir

  addFile(path: string, data: Uint8Array) {
    this.files.set(path, data);
  }

  /* ---- WASI syscall implementations ---- */

  private argsGet(argvPtr: number, argvBufPtr: number): number {
    const view = this.view();
    const encoder = new TextEncoder();
    const buffer = new Uint8Array(this.memory!.buffer);

    for (const arg of this.args) {
      view.setUint32(argvPtr, argvBufPtr, true);
      argvPtr += 4;
      const encoded = encoder.encode(arg + "\0");
      buffer.set(encoded, argvBufPtr);
      argvBufPtr += encoded.length;
    }
    return 0;
  }

  private argsSizesGet(countPtr: number, bufSizePtr: number): number {
    const view = this.view();
    const encoder = new TextEncoder();
    let totalSize = 0;
    for (const arg of this.args) {
      totalSize += encoder.encode(arg + "\0").length;
    }
    view.setUint32(countPtr, this.args.length, true);
    view.setUint32(bufSizePtr, totalSize, true);
    return 0;
  }

  private clockTimeGet(
    id: number,
    _precision: bigint,
    retptr: number
  ): number {
    const view = this.view();
    const now = BigInt(Date.now()) * BigInt(1_000_000);
    view.setBigUint64(retptr, now, true);
    return 0;
  }

  private clockResGet(id: number, retptr: number): number {
    const view = this.view();
    view.setBigUint64(retptr, BigInt(1_000_000), true);
    return 0;
  }

  private environGet(envPtr: number, envBufPtr: number): number {
    // No environment variables
    return 0;
  }

  private environSizesGet(countPtr: number, bufSizePtr: number): number {
    const view = this.view();
    view.setUint32(countPtr, 0, true);
    view.setUint32(bufSizePtr, 0, true);
    return 0;
  }

  private fdClose(fd: number): number {
    this.openFds.delete(fd);
    return 0;
  }

  private fdFdstatGet(fd: number, retptr: number): number {
    const view = this.view();
    // Zero out the struct
    for (let i = 0; i < 24; i++) {
      view.setUint8(retptr + i, 0);
    }
    if (fd === 0) {
      view.setUint8(retptr, 2); // character device
    } else if (fd === 1 || fd === 2) {
      view.setUint8(retptr, 2); // character device
    } else if (fd === 3) {
      view.setUint8(retptr, 3); // directory
    } else if (this.openFds.has(fd)) {
      view.setUint8(retptr, 4); // regular file
    } else {
      return 8; // EBADF
    }
    // Set rights to allow everything
    view.setBigUint64(retptr + 8, BigInt("0xFFFFFFFFFFFFFFFF"), true);
    view.setBigUint64(retptr + 16, BigInt("0xFFFFFFFFFFFFFFFF"), true);
    return 0;
  }

  private fdRead(
    fd: number,
    iovsPtr: number,
    iovsLen: number,
    retptr: number
  ): number {
    const view = this.view();
    const buffer = new Uint8Array(this.memory!.buffer);
    let totalRead = 0;

    for (let i = 0; i < iovsLen; i++) {
      const ptr = view.getUint32(iovsPtr + i * 8, true);
      const len = view.getUint32(iovsPtr + i * 8 + 4, true);

      if (fd === 0) {
        // stdin
        const remaining = this.stdin.length - this.stdinPos;
        const toRead = Math.min(len, remaining);
        buffer.set(
          this.stdin.subarray(this.stdinPos, this.stdinPos + toRead),
          ptr
        );
        this.stdinPos += toRead;
        totalRead += toRead;
        if (toRead < len) break;
      } else if (this.openFds.has(fd)) {
        // Virtual file
        const file = this.openFds.get(fd)!;
        const remaining = file.data.length - file.pos;
        const toRead = Math.min(len, remaining);
        buffer.set(
          file.data.subarray(file.pos, file.pos + toRead),
          ptr
        );
        file.pos += toRead;
        totalRead += toRead;
        if (toRead < len) break;
      } else {
        return 8; // EBADF
      }
    }

    view.setUint32(retptr, totalRead, true);
    return 0;
  }

  private fdSeek(
    fd: number,
    offset: bigint,
    whence: number,
    retptr: number
  ): number {
    if (!this.openFds.has(fd)) return 8; // EBADF
    const file = this.openFds.get(fd)!;
    const off = Number(offset);

    switch (whence) {
      case 0: // SEEK_SET
        file.pos = off;
        break;
      case 1: // SEEK_CUR
        file.pos += off;
        break;
      case 2: // SEEK_END
        file.pos = file.data.length + off;
        break;
    }

    const view = this.view();
    view.setBigUint64(retptr, BigInt(file.pos), true);
    return 0;
  }

  private fdWrite(
    fd: number,
    ciovsPtr: number,
    ciovsLen: number,
    retptr: number
  ): number {
    const view = this.view();
    const buffer = new Uint8Array(this.memory!.buffer);
    let totalWritten = 0;

    for (let i = 0; i < ciovsLen; i++) {
      const ptr = view.getUint32(ciovsPtr + i * 8, true);
      const len = view.getUint32(ciovsPtr + i * 8 + 4, true);

      if (fd === 1 || fd === 2) {
        // stdout/stderr
        const chunk = buffer.slice(ptr, ptr + len);
        this.stdout.push(chunk);
        totalWritten += len;
      } else {
        return 8; // EBADF
      }
    }

    view.setUint32(retptr, totalWritten, true);
    return 0;
  }

  private fdPrestatGet(fd: number, retptr: number): number {
    if (fd === 3) {
      const view = this.view();
      view.setUint32(retptr, 0, true); // type = dir
      view.setUint32(retptr + 4, 1, true); // name length "/"
      return 0;
    }
    return 8; // EBADF — signals end of preopens
  }

  private fdPrestatDirName(
    fd: number,
    pathPtr: number,
    pathLen: number
  ): number {
    if (fd === 3) {
      const buffer = new Uint8Array(this.memory!.buffer);
      buffer[pathPtr] = 47; // '/'
      return 0;
    }
    return 8;
  }

  private pathOpen(
    dirFd: number,
    _dirflags: number,
    pathPtr: number,
    pathLen: number,
    _oflags: number,
    _fsRightsBase: bigint,
    _fsRightsInheriting: bigint,
    _fdflags: number,
    retptr: number
  ): number {
    const buffer = new Uint8Array(this.memory!.buffer);
    const decoder = new TextDecoder();
    let path = decoder.decode(buffer.subarray(pathPtr, pathPtr + pathLen));

    // Normalize path
    if (path.startsWith("./")) path = path.substring(2);
    if (path.startsWith("/")) path = path.substring(1);

    const fileData = this.files.get(path);
    if (!fileData) {
      return 44; // ENOENT
    }

    const fd = this.nextFd++;
    this.openFds.set(fd, { data: fileData, pos: 0 });

    const view = this.view();
    view.setUint32(retptr, fd, true);
    return 0;
  }

  private pathFilestatGet(
    fd: number,
    flags: number,
    pathPtr: number,
    pathLen: number,
    retptr: number
  ): number {
    const buffer = new Uint8Array(this.memory!.buffer);
    const decoder = new TextDecoder();
    let path = decoder.decode(buffer.subarray(pathPtr, pathPtr + pathLen));
    if (path.startsWith("./")) path = path.substring(2);
    if (path.startsWith("/")) path = path.substring(1);

    const fileData = this.files.get(path);
    if (!fileData) return 44; // ENOENT

    const view = this.view();
    // Zero out stat struct (64 bytes)
    for (let i = 0; i < 64; i++) {
      view.setUint8(retptr + i, 0);
    }
    // Set filetype to regular file
    view.setUint8(retptr + 16, 4);
    // Set size
    view.setBigUint64(retptr + 32, BigInt(fileData.length), true);
    return 0;
  }

  private procExit(code: number): never {
    throw new ProcessExit(code);
  }

  private randomGet(bufPtr: number, bufLen: number): number {
    const buffer = new Uint8Array(this.memory!.buffer, bufPtr, bufLen);
    crypto.getRandomValues(buffer);
    return 0;
  }
}

class ProcessExit extends Error {
  code: number;
  constructor(code: number) {
    super(`proc_exit=${code}`);
    this.code = code;
  }
}

/**
 * Serialize an HTTP request into the JSON format AtomVM expects on stdin.
 */
async function serializeRequest(request: Request): Promise<string> {
  const url = new URL(request.url);
  const headers: Record<string, string> = {};
  request.headers.forEach((value, key) => {
    headers[key] = value;
  });

  const body = request.body ? await request.text() : "";

  const req: ElixirRequest = {
    method: request.method,
    url: url.pathname + url.search,
    headers,
    body,
  };

  return JSON.stringify(req);
}

/**
 * Parse AtomVM's stdout JSON into an HTTP Response.
 */
function deserializeResponse(stdout: string): Response {
  // Find the first complete JSON object in stdout
  // (AtomVM may print debug output after the JSON response)
  const firstBrace = stdout.indexOf("{");
  if (firstBrace === -1) {
    return new Response(
      JSON.stringify({
        error: "Invalid response from Elixir application",
        raw_output: stdout.substring(0, 1000),
      }),
      { status: 502, headers: { "content-type": "application/json" } }
    );
  }

  // Find the matching closing brace by counting nesting depth
  let depth = 0;
  let inString = false;
  let escape = false;
  let endPos = -1;

  for (let i = firstBrace; i < stdout.length; i++) {
    const ch = stdout[i];
    if (escape) {
      escape = false;
      continue;
    }
    if (ch === "\\") {
      escape = true;
      continue;
    }
    if (ch === '"') {
      inString = !inString;
      continue;
    }
    if (inString) continue;
    if (ch === "{") depth++;
    if (ch === "}") {
      depth--;
      if (depth === 0) {
        endPos = i;
        break;
      }
    }
  }

  if (endPos === -1) {
    return new Response(
      JSON.stringify({
        error: "Incomplete JSON from Elixir application",
        raw_output: stdout.substring(0, 1000),
      }),
      { status: 502, headers: { "content-type": "application/json" } }
    );
  }

  try {
    const resp: ElixirResponse = JSON.parse(
      stdout.substring(firstBrace, endPos + 1)
    );

    return new Response(resp.body, {
      status: resp.status,
      headers: resp.headers,
    });
  } catch {
    return new Response(
      JSON.stringify({
        error: "Failed to parse Elixir response",
        raw_output: stdout.substring(0, 1000),
      }),
      { status: 502, headers: { "content-type": "application/json" } }
    );
  }
}

/**
 * Run AtomVM with the given stdin data and return stdout.
 */
async function runAtomVM(
  stdinData: string,
  avmData: Uint8Array
): Promise<string> {
  const wasi = new AtomVMWasi(stdinData, ["atomvm", "app.avm"]);

  // Add the .avm file to the virtual filesystem
  wasi.addFile("app.avm", avmData);

  // Stub out undefined env imports (ETS, distribution — excluded from WASI build)
  const envStubs: Record<string, Function> = {
    dist_send_message: () => 0,
    dist_send_unlink_id_ack: () => 0,
    dist_send_payload_exit: () => 0,
    ets_delete_owned_tables: () => 0,
    ets_init: () => 0,
    ets_destroy: () => 0,
    dist_spawn_reply: () => 0,
    dist_monitor: () => 0,
    ets_delete: () => 0,
    ets_drop_table: () => 0,
    ets_insert: () => 0,
    ets_create_table_maybe_gc: () => 0,
    ets_update_counter_maybe_gc: () => 0,
    dist_send_link: () => 0,
    dist_send_unlink_id: () => 0,
    ets_lookup_maybe_gc: () => 0,
    ets_lookup_element_maybe_gc: () => 0,
  };

  // Instantiate the WebAssembly module
  const instance = await WebAssembly.instantiate(atomvmModule, {
    wasi_snapshot_preview1: wasi.imports,
    env: envStubs,
  });

  // Set the memory reference
  wasi.setMemory(instance.exports.memory as WebAssembly.Memory);

  // Run the WASI module
  try {
    const start = instance.exports._start as Function;
    start();
  } catch (e) {
    if (e instanceof ProcessExit) {
      if (e.code !== 0) {
        // Non-zero exit — include stdout in the error for debugging
        const output = wasi.getStdout();
        throw new Error(
          `AtomVM exited with code ${e.code}. stdout: ${output.substring(0, 2000)}`
        );
      }
    } else {
      throw e;
    }
  }

  return wasi.getStdout();
}

/**
 * Convert the imported .avm module data to a Uint8Array.
 * Cloudflare Workers imports binary files as ArrayBuffer.
 */
function getAvmData(): Uint8Array {
  if (appAvm instanceof ArrayBuffer) {
    return new Uint8Array(appAvm);
  }
  // If it's already a Uint8Array or similar
  return new Uint8Array(appAvm as ArrayBuffer);
}

export default {
  async fetch(
    request: Request,
    env: Record<string, unknown>,
    ctx: ExecutionContext
  ): Promise<Response> {
    try {
      // Serialize the incoming HTTP request to JSON
      const requestJson = await serializeRequest(request);

      // Get the .avm application data
      const avmData = getAvmData();

      // Run AtomVM with the request
      const stdout = await runAtomVM(requestJson, avmData);

      // Parse the response
      return deserializeResponse(stdout);
    } catch (error) {
      const message =
        error instanceof Error ? error.message : "Unknown error";
      return new Response(
        JSON.stringify({
          error: "AtomVM execution failed",
          details: message,
        }),
        {
          status: 500,
          headers: { "content-type": "application/json" },
        }
      );
    }
  },
};
