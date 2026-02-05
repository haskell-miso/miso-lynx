import { mts } from './miso/mts';
import { bts } from './miso/bts';

/* init bts () */
bts();

/* init mts() */
globalThis['renderPage'] = mts();

/* etc. */
globalThis['processData'] = () => {};
