<script setup lang="ts">
// Signing in to the file server (#357).
//
// Email and password, and no way to create an account: there is no mail server,
// so an administrator makes each account and passes the password on directly. A
// forgotten password is a message to them, not a link -- hence the note rather
// than a "forgot your password?" that could not work.
//
// Built on AppModal like every other dialog in the IDE, so it gets showModal()
// -- and with it the top layer, the ::backdrop, focus trapping, and Esc -- from
// one place rather than reimplementing them here.
import { ref } from 'vue'
import AppModal from './AppModal.vue'
import type { SignInMethods } from '../auth-client'

const props = defineProps<{
  open: boolean
  methods: SignInMethods
  /** Set while a sign-in is in flight, so the form cannot be submitted twice. */
  busy: boolean
  error: string | null
}>()

const emit = defineEmits<{
  password: [email: string, password: string]
  close: []
}>()

const email = ref('')
const password = ref('')

function submit() {
  if (props.busy) return
  emit('password', email.value, password.value)
}
</script>

<template>
  <AppModal :open="open" title="Sign in to Scamper" @dismiss="emit('close')">
    <p class="explain">
      Your files are kept on the Scamper server, so they survive a cleared
      browser and follow you between computers.
    </p>

    <form v-if="methods.password" class="sign-in-form" @submit.prevent="submit">
      <label>
        Email
        <input v-model="email" type="email" autocomplete="email" required />
      </label>
      <label>
        Password
        <input
          v-model="password"
          type="password"
          autocomplete="current-password"
          required
        />
      </label>

      <p v-if="error !== null" class="error" role="alert">{{ error }}</p>

      <button type="submit" :disabled="busy">Sign in</button>
    </form>

    <p v-else-if="error !== null" class="error" role="alert">{{ error }}</p>

    <p class="note">
      Accounts are created by your instructor or system administrator. Ask them
      if you need one, or if you have forgotten your password.
    </p>

    <template #footer>
      <button type="button" class="link" @click="emit('close')">
        Keep working in this browser instead
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
.explain {
  margin: 0;
  font-size: 0.9rem;
  color: var(--fg);
  opacity: 0.8;
}

.sign-in-form {
  display: flex;
  flex-direction: column;
}

label {
  display: block;
  margin-bottom: 0.75rem;
  font-size: 0.9rem;
}

input {
  display: block;
  width: 100%;
  margin-top: 0.25rem;
  padding: 0.4rem;
  font: inherit;
  color: var(--fg);
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 4px;
}

button[type='submit'] {
  align-self: flex-start;
  margin-top: 0.5rem;
  padding: 0.4rem 1rem;
  font: inherit;
  color: var(--accent-fg);
  background: var(--accent);
  border: 1px solid transparent;
  border-radius: 4px;
  cursor: pointer;
}

button[type='submit']:disabled {
  opacity: 0.6;
  cursor: default;
}

.error {
  margin: 0.5rem 0 0;
  color: var(--danger);
  font-size: 0.9rem;
}

.note {
  margin: 0;
  font-size: 0.85rem;
  color: var(--fg);
  opacity: 0.75;
}

.link {
  border: none;
  background: none;
  padding: 0;
  color: var(--fg);
  text-decoration: underline;
  cursor: pointer;
  font: inherit;
  font-size: 0.85rem;
  opacity: 0.75;
}
</style>
