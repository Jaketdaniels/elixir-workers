declare module "*.wasm" {
  const module: WebAssembly.Module;
  export default module;
}

declare module "*.avm" {
  const data: ArrayBuffer;
  export default data;
}
