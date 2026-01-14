import * as fs from 'node:fs';
import * as path from 'node:path';
import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

async function run() {
  const result = await abap.Classes["ZCL_PDF_DEMO"].run_base64();
  console.dir(result);
  fs.writeFileSync(path.join(".", "demo.pdf"), Buffer.from(result.get(), "base64"));
}


run();