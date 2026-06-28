/* eslint-disable vue/one-component-per-file -- inline host wrappers for provide/inject tests */
import { defineComponent, shallowRef } from "vue"
import { flushPromises, mount } from "@vue/test-utils"
import { afterEach, describe, expect, test, vi } from "vitest"
import { Loc, LoggingChannel } from "../src/lpm"
import { type DisplayRun, ScamperInstance } from "../src/scamper-instance"
import type { CodeMirrorEditorAdapter } from "../src/web/components/codemirror-editor-adapter"
import type { EditorAccessor } from "../src/web/components/editor-context"
import IdeHeader from "../src/web/components/IdeHeader.vue"
import {
  provideScamperSession,
  type ScamperSession,
  type ScamperSessionOptions,
} from "../src/web/components/use-scamper-session"
import type { ResultsPaneType } from "../src/web/components/use-results-pane"

interface MockRun extends DisplayRun {
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

/** Mimics ScamperInstance.execute returning a DisplayRun handle. */
function mockExecute(scamper: ScamperInstance) {
  return vi.spyOn(scamper, "execute").mockImplementation(() => {
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

  test("execute stops the previous display task before scheduling", () => {
    const scamper = ScamperInstance.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    const runs: MockRun[] = []
    vi.spyOn(scamper, "execute").mockImplementation(() => {
      const run = makeMockRun(crypto.randomUUID())
      runs.push(run)
      return run
    })

    const session = mountSession()

    session.execute()
    const firstId = session.currentRun.value
    session.execute()

    expect(firstId).not.toBeNull()
    expect(cancel).toHaveBeenCalledWith(firstId)
    expect(runs).toHaveLength(2)
    expect(session.currentRun.value).not.toBe(firstId)
  })

  test("stopRun does not cancel queries", () => {
    const scamper = ScamperInstance.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    vi.spyOn(scamper, "query").mockReturnValue("query-1")
    mockExecute(scamper)

    const session = mountSession()
    session.query()
    session.execute()
    const runId = session.currentRun.value
    session.stopRun()

    expect(runId).not.toBeNull()
    expect(cancel).toHaveBeenCalledWith(runId)
    expect(cancel).not.toHaveBeenCalledWith("query-1")
    expect(session.queries.value).toHaveLength(1)
    expect(session.currentRun.value).toBeNull()
  })

  test("stopAll cancels display task and all queries", () => {
    const scamper = ScamperInstance.getInstance()
    const cancel = vi.spyOn(scamper, "cancel")
    vi.spyOn(scamper, "query")
      .mockReturnValueOnce("query-1")
      .mockReturnValueOnce("query-2")
    mockExecute(scamper)

    const session = mountSession()
    session.query()
    session.query()
    session.execute()
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
    const scamper = ScamperInstance.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(() => {
      lastRun = makeMockRun(crypto.randomUUID())
      return lastRun
    })

    const session = mountSession()
    session.execute()
    expect(session.currentRun.value).not.toBeNull()
    if (!lastRun) {
      expect.fail()
    }
    lastRun.resolve()
    await flushPromises()
    expect(session.currentRun.value).toBeNull()
  })

  test("execute clears currentRun when compile fails", () => {
    const scamper = ScamperInstance.getInstance()
    vi.spyOn(scamper, "execute").mockReturnValue(null)

    const session = mountSession()
    session.execute()

    expect(session.currentRun.value).toBeNull()
  })

  test("stale done does not clear a newer run", async () => {
    const scamper = ScamperInstance.getInstance()
    const runs: MockRun[] = []
    vi.spyOn(scamper, "execute").mockImplementation(() => {
      const run = makeMockRun(crypto.randomUUID())
      runs.push(run)
      return run
    })

    const session = mountSession()
    session.execute()
    const firstId = session.currentRun.value
    session.execute()
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

  test("execute is a no-op when the results pane is unavailable", () => {
    const scamper = ScamperInstance.getInstance()
    const execute = vi.spyOn(scamper, "execute")
    vi.spyOn(scamper, "query").mockReturnValue("query-1")
    const onRunScheduled = vi.fn()

    const session = mountSession({ onRunScheduled }, null)
    session.query()
    session.execute()

    expect(execute).not.toHaveBeenCalled()
    expect(session.queries.value).toHaveLength(1)
    expect(onRunScheduled).toHaveBeenCalledTimes(1)
  })

  test("onRunScheduled is called when execute is scheduled", () => {
    const scamper = ScamperInstance.getInstance()
    mockExecute(scamper)
    const onRunScheduled = vi.fn()

    const session = mountSession({ onRunScheduled })
    session.execute()

    expect(onRunScheduled).toHaveBeenCalledOnce()
  })

  test("execute sets currentRun from returned handle", () => {
    const scamper = ScamperInstance.getInstance()
    const session = mountSession()
    vi.spyOn(scamper, "execute").mockReturnValue(makeMockRun("task-1"))

    session.execute()
    expect(session.currentRun.value).toBe("task-1")
  })

  test("isTracing reflects trace mode only, not normal run", () => {
    const scamper = ScamperInstance.getInstance()
    vi.spyOn(scamper, "execute").mockImplementation(({ isTracing }) => {
      return makeMockRun(crypto.randomUUID(), isTracing ?? false)
    })

    const session = mountSession()
    session.execute()
    expect(session.isTracing.value).toBe(false)

    session.execute({ tracing: true })
    expect(session.isTracing.value).toBe(true)

    session.execute()
    expect(session.isTracing.value).toBe(false)
  })

  test("isTracing clears when display task completes or is stopped", async () => {
    const scamper = ScamperInstance.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(({ isTracing }) => {
      lastRun = makeMockRun(crypto.randomUUID(), isTracing ?? false)
      return lastRun
    })

    const session = mountSession()
    session.execute({ tracing: true })
    expect(session.isTracing.value).toBe(true)
    expect(lastRun).toBeDefined()

    if (!lastRun) {
      expect.fail()
    }
    await flushPromises()
    expect(session.isTracing.value).toBe(false)

    session.execute({ tracing: true })
    session.stopRun()
    expect(session.isTracing.value).toBe(false)
  })

  test("IdeHeader unwraps currentRun ref for run/stop UI", async () => {
    const scamper = ScamperInstance.getInstance()
    mockExecute(scamper)

    const Host = defineComponent({
      components: { IdeHeader },
      setup() {
        const paneRef = shallowRef<ResultsPaneType | null>(makePane())
        const editor: EditorAccessor = () => makeAdapter()
        provideScamperSession(paneRef, { editor })
      },
      template: "<IdeHeader />",
    })

    const wrapper = mount(Host)
    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(false)

    await wrapper.find('[aria-label="Run"]').trigger("click")
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(false)
  })

  test("IdeHeader shows Run after display task completes", async () => {
    const scamper = ScamperInstance.getInstance()
    let lastRun: MockRun | undefined
    vi.spyOn(scamper, "execute").mockImplementation(() => {
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
      template: "<IdeHeader />",
    })

    const wrapper = mount(Host)
    await wrapper.find('[aria-label="Run"]').trigger("click")
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(true)
    expect(lastRun).toBeDefined()

    if (!lastRun) {
      expect.fail()
    }
    await flushPromises()
    await wrapper.vm.$nextTick()

    expect(wrapper.find('[aria-label="Run"]').exists()).toBe(true)
    expect(wrapper.find('[aria-label="Stop"]').exists()).toBe(false)
  })
})
