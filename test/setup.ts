import * as matchers from '@testing-library/jest-dom/matchers';
import { expect } from 'vitest';
import 'vitest-canvas-mock';
import { initializeLibs } from '../src/lib';

expect.extend(matchers);
// N.B., only initializeLibs() here, not scamper.ts's initialize(): importing
// scamper.ts triggers its module-load-time renderer registration (a
// fire-and-forget `import("./web/renderers.js")`), and doing that from this
// shared global setup -- before any individual test file's own vi.mock(...)
// calls have been registered -- grabs real (unmocked) transitive
// dependencies (e.g. src/fs/opfs.ts) out from under tests that mock them.
// Test files that actually need Scamper.getInstance() (or anything else
// from scamper.ts) call its initialize() themselves, after their own mocks.
await initializeLibs();
