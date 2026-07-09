/* eslint-disable vue/one-component-per-file -- inline host wrappers for provide/inject tests */
import { defineComponent, shallowRef } from "vue"
import { flushPromises, mount } from "@vue/test-utils"
import { afterEach, describe, expect, test, vi } from "vitest"
import { Loc, LoggingChannel } from "../src/lpm"
import Scamper, { type DisplayRequest } from "../src/scamper"
import type { CodeMirrorEditorAdapter } from "../src/web/components/codemirror-editor-adapter"
import type { EditorAccessor } from "../src/web/components/editor-context"
import IdeHeader from "../src/web/components/IdeHeader.vue"
import {
  provideScamperSession,
  type ScamperSession,
  type ScamperSessionOptions,
} from "../src/web/components/use-scamper-session"
import type { ResultsPaneType } from "../src/web/components/use-results-pane"

interface MockRun extends DisplayRequest {
  resolve(): void
}

function deferred(): { promise: Promise<void>; resolve: () => void } {
  let resolve!: () => void
  const promise = new Promise<void>((r) => {
    resolve = r
  })
  return { promise, resolve }
}

function makeMockRun(id: string, tracing = false): MockRun {
  const { promise, resolve } = deferred()
  return {
    id,
    tracing,
    done: promise,
    resolve,
  }
}

/** Mimics Scamper.execute returning a DisplayRun handle. */
function mockExecute(scamper: Scamper) {
  return vi.spyOn(scamper, "execute").mockImplementation(async () => {
    return makeMockRun(crypto.randomUUID())
  })
}

function makeAdapter(): CodeMirrorEditorAdapter {
  return {
    getDoc: () => "1",
    isLoaded: () => true,
    initializeDoc: () => {
      /* noop */
    },
    initializeDummyDoc: () => {
      /* noop */
    },
    getCursorLoc: () => new Loc(0, 0, 0),
    coordsAtPos: () => null,
    onViewChange: () => () => {
      /* noop */
    },
  }
}

function makePane(): ResultsPaneType {
  const ch = new LoggingChannel(false, false)
  return {
    display: ch,
    reset: vi.fn(),
    scrollToBottom: vi.fn(),
  }
}

function mountSession(
  options: Omit<ScamperSessionOptions, "editor"> = {},
  pane: ResultsPaneType | null = makePane(),
): ScamperSession {
  let session!: ScamperSession
  const editor: EditorAccessor = () => makeAdapter()

  const Host = defineComponent({
    setup() {
      const paneRef = shallowRef<ResultsPaneType | null>(pane)
      session = provideScamperSession(paneRef, { ...options, editor })
      return () => null
    },
    template: "<div/>",
  })

  mount(Host)
  return session
}

describe("useScamperSession", () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  test("execute stops the previous display task before scheduling", async () => {
    const scamper = Scamper.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    const runs: MockRun[] = []
    vi.spyOn(scamper, "execute").mockImplementation(async () => {
      const run = makeMockRun(crypto.randomUUID())
      runs.push(run)
      return run
    })

    const session = mountSession()

    await session.execute()
    const firstId = session.currentRun.value
    await session.execute()

    expect(firstId).not.toBeNull()
    expect(cancel).toHaveBeenCalledWith(firstId)
    expect(runs).toHaveLength(2)
    expect(session.currentRun.value).not.toBe(firstId)
  })

  test("stopRun does not cancel queries", async () => {
    const scamper = Scamper.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    vi.spyOn(scamper, "query").mockResolvedValue({
      id: "query-1",
      done: expect.anything() as Promise<void>,
    })
    mockExecute(scamper)

    const session = mountSession()
    await session.query()
    await session.execute()
    const runId = session.currentRun.value
    session.stopRun()

    expect(runId).not.toBeNull()
    expect(cancel).toHaveBeenCalledWith(runId)
    expect(cancel).not.toHaveBeenCalledWith("query-1")
    expect(session.queries.value).toHaveLength(1)
    expect(session.currentRun.value).toBeNull()
  })

  test("stopAll cancels display task and all queries", async () => {
    const scamper = Scamper.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    vi.spyOn(scamper, "query")
      .mockResolvedValueOnce({
        id: "query-1",
        done: expect.anything() as Promise<void>,
      })
      .mockResolvedValueOnce({
        id: "query-2",
        done: expect.anything() as Promise<void>,
      })
    mockExecute(scamper)

    const session = mountSession()
    await session.query()
    await session.query()
    await session.execute()
    const runId = session.currentRun.value
    session.stopAll()

    expect(cancel).toHaveBeenCalledWith("query-1")
    expect(cancel).toHaveBeenCalledWith("query-2")
    expect(runId).not.toBeNull()
    expect(cancel).toHaveBeenCalledWith(runId)
    expect(session.queries.value).toHaveLength(0)
    expect(session.currentRun.value).toBeNull()
  })

  test("done clears currentRun when display task completes", async () => {
    const scamper = Scamper.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(async () => {
      lastRun = makeMockRun(crypto.randomUUID())
      return lastRun
    })

    const session = mountSession()
    await session.execute()
    expect(session.currentRun.value).not.toBeNull()
    if (!lastRun) {
      expect.fail()
    }
    lastRun.resolve()
    await flushPromises()
    expect(session.currentRun.value).toBeNull()
  })

  test("execute clears currentRun when compile fails", async () => {
    const scamper = Scamper.getInstance()
    vi.spyOn(scamper, "execute").mockResolvedValue(null)

    const session = mountSession()
    await session.execute()

    expect(session.currentRun.value).toBeNull()
  })

  test("stale done does not clear a newer run", async () => {
    const scamper = Scamper.getInstance()
    const runs: MockRun[] = []
    vi.spyOn(scamper, "execute").mockImplementation(async () => {
      const run = makeMockRun(crypto.randomUUID())
      runs.push(run)
      return run
    })

    const session = mountSession()
    await session.execute()
    const firstId = session.currentRun.value
    await session.execute()
    const secondId = session.currentRun.value

    expect(firstId).not.toBeNull()
    expect(secondId).not.toBeNull()
    expect(firstId).not.toBe(secondId)

    const firstRun = runs[0]
    const secondRun = runs[1]
    expect(firstRun).toBeDefined()
    expect(secondRun).toBeDefined()

    firstRun.resolve()
    await flushPromises()
    expect(session.currentRun.value).toBe(secondId)

    secondRun.resolve()
    await flushPromises()
    expect(session.currentRun.value).toBeNull()
  })

  test("execute is a no-op when the results pane is unavailable", async () => {
    const scamper = Scamper.getInstance()
    const execute = vi.spyOn(scamper, "execute")
    vi.spyOn(scamper, "query").mockResolvedValue({
      id: "query-1",
      done: expect.anything() as Promise<void>,
    })
    const onRunScheduled = vi.fn()

    const session = mountSession({ onRunScheduled }, null)
    await session.query()
    await session.execute()

    expect(execute).not.toHaveBeenCalled()
    expect(session.queries.value).toHaveLength(1)
    expect(onRunScheduled).toHaveBeenCalledTimes(1)
  })

  test("onRunScheduled is called when execute is scheduled", async () => {
    const scamper = Scamper.getInstance()
    mockExecute(scamper)
    const onRunScheduled = vi.fn()

    const session = mountSession({ onRunScheduled })
    await session.execute()

    expect(onRunScheduled).toHaveBeenCalledOnce()
  })

  test("execute sets currentRun from returned handle", async () => {
    const scamper = Scamper.getInstance()
    const session = mountSession()
    vi.spyOn(scamper, "execute").mockResolvedValue(makeMockRun("task-1"))

    await session.execute()
    expect(session.currentRun.value).toBe("task-1")
  })

  test("isTracing reflects trace mode only, not normal run", async () => {
    const scamper = Scamper.getInstance()
    vi.spyOn(scamper, "execute").mockImplementation(async ({ isTracing }) => {
      return makeMockRun(crypto.randomUUID(), isTracing ?? false)
    })

    const session = mountSession()
    await session.execute()
    expect(session.isTracing.value).toBe(false)

    await session.execute({ tracing: true })
    expect(session.isTracing.value).toBe(true)

    await session.execute()
    expect(session.isTracing.value).toBe(false)
  })

  test("isTracing clears when display task completes or is stopped", async () => {
    const scamper = Scamper.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(async ({ isTracing }) => {
      lastRun = makeMockRun(crypto.randomUUID(), isTracing ?? false)
      return lastRun
    })

    const session = mountSession()
    await session.execute({ tracing: true })
    expect(session.isTracing.value).toBe(true)
    expect(lastRun).toBeDefined()

    if (!lastRun) {
      expect.fail()
    }
    lastRun.resolve()
    await flushPromises()
    expect(session.isTracing.value).toBe(false)

    await session.execute({ tracing: true })
    session.stopRun()
    expect(session.isTracing.value).toBe(false)
  })

  test("IdeHeader unwraps currentRun ref for run/stop UI", async () => {
    const scamper = Scamper.getInstance()
    mockExecute(scamper)

    const Host = defineComponent({
      components: { IdeHeader },
      setup() {
        const paneRef = shallowRef<ResultsPaneType | null>(makePane())
        const editor: EditorAccessor = () => makeAdapter()
        provideScamperSession(paneRef, { editor })
      },
      template: "<IdeHeader currentFile='null' />",
    })

    const wrapper = mount(Host)
    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(false)

    await wrapper.find('[aria-label="Run"]').trigger("click")
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(false)
  })

  test("IdeHeader shows Run after display task completes", async () => {
    const scamper = Scamper.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(async () => {
      lastRun = makeMockRun(crypto.randomUUID())
      return lastRun
    })

    const Host = defineComponent({
      components: { IdeHeader },
      setup() {
        const paneRef = shallowRef<ResultsPaneType | null>(makePane())
        const editor: EditorAccessor = () => makeAdapter()
        provideScamperSession(paneRef, { editor })
      },
      template: "<IdeHeader currentFile='null' />",
    })

    const wrapper = mount(Host)
    await wrapper.find('[aria-label="Run"]').trigger("click")
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(true)
    expect(lastRun).toBeDefined()

    if (!lastRun) {
      expect.fail()
    }
    lastRun.resolve()
    await flushPromises()
    await wrapper.vm.$nextTick()

    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(false)
  })
})
