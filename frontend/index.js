const initWASI = async () => {
  const { WASI, OpenFile, File, ConsoleStdout } = await import('./browser_wasi_shim/index.js');
  const ghc_wasm_jsffi = await import('./ghc_wasm_jsffi.js');

  const wasi = new WASI([], [], [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(msg => console.log(`[WASI] ${msg}`)),
    ConsoleStdout.lineBuffered(msg => console.error(`[WASI] ${msg}`))
  ], { debug: import.meta.env?.DEV });

  const exports = {};
  try {
    const { instance } = await WebAssembly.instantiateStreaming(
      fetch('bin.wasm'),
      {
        wasi_snapshot_preview1: wasi.wasiImport,
        ghc_wasm_jsffi: ghc_wasm_jsffi.default(exports)
      }
    );

    Object.assign(exports, instance.exports);
    wasi.initialize(instance);
    await instance.exports.hs_start();

    return instance;
  } catch (err) {
    console.error('[WASI] Init failed:', err);
    throw err;
  }
};

initWASI().catch(console.error);