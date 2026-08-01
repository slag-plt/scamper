import { afterEach, describe, expect, test } from 'vitest'
import { flushPromises, mount } from '@vue/test-utils'
import AppModal from '../../../src/app/web/components/AppModal.vue'
import ModalHost from '../../../src/app/web/components/ModalHost.vue'
import {
  activeModal,
  dismissModal,
  modalAlert,
  modalConfirm,
  modalPrompt,
  resolveModal,
} from '../../../src/app/web/composables/use-modals'

// The modal queue is module-global state; drain it between tests so a leaked
// request from one test can't bleed into the next.
afterEach(() => {
  while (activeModal.value !== null) dismissModal()
})

describe('use-modals service (no DOM)', () => {
  test('modalAlert enqueues a single-button request and resolves when answered', async () => {
    let resolved = false
    const p = modalAlert({ message: 'hi' }).then(() => {
      resolved = true
    })
    expect(activeModal.value?.kind).toBe('alert')
    expect(activeModal.value?.cancelLabel).toBeUndefined()
    resolveModal(undefined)
    await p
    expect(resolved).toBe(true)
    expect(activeModal.value).toBeNull()
  })

  test('modalConfirm resolves true on confirm, false on dismiss', async () => {
    const confirmed = modalConfirm({ message: 'ok?' })
    resolveModal(true)
    expect(await confirmed).toBe(true)

    const cancelled = modalConfirm({ message: 'ok?' })
    dismissModal()
    expect(await cancelled).toBe(false)
  })

  test('modalPrompt resolves the entered string, or null on dismiss', async () => {
    const entered = modalPrompt({ message: 'name?' })
    resolveModal('scratch.scm')
    expect(await entered).toBe('scratch.scm')

    const cancelled = modalPrompt({ message: 'name?' })
    dismissModal()
    expect(await cancelled).toBeNull()
  })

  test('requests queue in order; the head is always active', async () => {
    const first = modalConfirm({ message: 'first' })
    const second = modalConfirm({ message: 'second' })
    expect(activeModal.value?.message).toBe('first')
    resolveModal(true)
    expect(activeModal.value?.message).toBe('second')
    resolveModal(false)
    expect(await first).toBe(true)
    expect(await second).toBe(false)
  })

  test('danger flag is carried through for confirm', () => {
    void modalConfirm({ message: 'delete?', danger: true })
    expect(activeModal.value?.danger).toBe(true)
  })
})

describe('ModalHost + AppModal integration', () => {
  test('confirm renders message and buttons; primary resolves true', async () => {
    const wrapper = mount(ModalHost)
    const confirmed = modalConfirm({
      message: 'Delete file?',
      confirmLabel: 'Delete',
    })
    await flushPromises()
    expect(wrapper.text()).toContain('Delete file?')
    const buttons = wrapper.findAll('button')
    expect(buttons.map((b) => b.text())).toEqual(['Cancel', 'Delete'])
    await wrapper.find('.modal-button--primary').trigger('click')
    expect(await confirmed).toBe(true)
    wrapper.unmount()
  })

  test('confirm cancel button resolves false', async () => {
    const wrapper = mount(ModalHost)
    const confirmed = modalConfirm({ message: 'sure?' })
    await flushPromises()
    await wrapper.findAll('button')[0].trigger('click')
    expect(await confirmed).toBe(false)
    wrapper.unmount()
  })

  test('prompt seeds its input with defaultValue and resolves the edited text', async () => {
    const wrapper = mount(ModalHost)
    const entered = modalPrompt({ message: 'Rename', defaultValue: 'old.scm' })
    await flushPromises()
    const input = wrapper.find('input')
    expect((input.element as HTMLInputElement).value).toBe('old.scm')
    await input.setValue('new.scm')
    await wrapper.find('form').trigger('submit')
    expect(await entered).toBe('new.scm')
    wrapper.unmount()
  })

  test('a second queued modal appears and resolves after the first is answered (#305 depth>=2)', async () => {
    const wrapper = mount(ModalHost)
    const first = modalConfirm({ message: 'First?' })
    const second = modalPrompt({ message: 'Second name', defaultValue: 'x.scm' })
    await flushPromises()
    expect(wrapper.text()).toContain('First?')
    await wrapper.find('.modal-button--primary').trigger('click')
    expect(await first).toBe(true)
    await flushPromises()
    // The successor must actually render (regression: it used to stay hidden
    // because `open` never transitioned when the queue advanced).
    expect(wrapper.text()).toContain('Second name')
    const input = wrapper.find('input')
    expect((input.element as HTMLInputElement).value).toBe('x.scm')
    await input.setValue('y.scm')
    await wrapper.find('form').trigger('submit')
    expect(await second).toBe('y.scm')
    wrapper.unmount()
  })

  test('dismissing the first queued modal still shows the second (#305 depth>=2)', async () => {
    const wrapper = mount(ModalHost)
    const first = modalConfirm({ message: 'First?' })
    const second = modalConfirm({ message: 'Second?' })
    await flushPromises()
    await wrapper.findAll('button')[0].trigger('click') // Cancel the first
    expect(await first).toBe(false)
    await flushPromises()
    expect(wrapper.text()).toContain('Second?')
    await wrapper.find('.modal-button--primary').trigger('click')
    expect(await second).toBe(true)
    wrapper.unmount()
  })

  test('alert shows a single confirm button and resolves on click', async () => {
    const wrapper = mount(ModalHost)
    const done = modalAlert({ message: 'File exists!' })
    await flushPromises()
    const buttons = wrapper.findAll('button')
    expect(buttons).toHaveLength(1)
    expect(buttons[0].text()).toBe('OK')
    await buttons[0].trigger('click')
    await expect(done).resolves.toBeUndefined()
    wrapper.unmount()
  })
})

describe('AppModal dismissal semantics', () => {
  test('a native close (Esc) emits dismiss and update:open', async () => {
    const wrapper = mount(AppModal, { props: { open: true } })
    // jsdom has no showModal, so simulate the native close event Esc fires.
    wrapper.find('dialog').element.dispatchEvent(new Event('close'))
    await flushPromises()
    expect(wrapper.emitted('dismiss')).toHaveLength(1)
    expect(wrapper.emitted('update:open')?.[0]).toEqual([false])
    wrapper.unmount()
  })

  test('a backdrop click (target is the dialog) dismisses', async () => {
    const wrapper = mount(AppModal, { props: { open: true } })
    const dialog = wrapper.find('dialog')
    await dialog.trigger('click') // target defaults to the dialog element itself
    expect(wrapper.emitted('dismiss')).toHaveLength(1)
    wrapper.unmount()
  })

  test('a click inside the panel does not dismiss', async () => {
    const wrapper = mount(AppModal, { props: { open: true } })
    await wrapper.find('.app-modal__panel').trigger('click')
    expect(wrapper.emitted('dismiss')).toBeUndefined()
    wrapper.unmount()
  })

  test('when not dismissable, Esc/cancel is prevented', () => {
    const wrapper = mount(AppModal, { props: { open: true, dismissable: false } })
    const cancel = new Event('cancel', { cancelable: true })
    wrapper.find('dialog').element.dispatchEvent(cancel)
    expect(cancel.defaultPrevented).toBe(true)
    wrapper.unmount()
  })
})
