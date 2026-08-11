<script setup lang="ts">
// Signing in to the file server (#357).
//
// Email and password, and no way to create an account: there is no mail server,
// so an administrator makes each account and passes the password on directly. A
// forgotten password is a message to them, not a link -- hence the note rather
// than a "forgot your password?" that could not work.
import { ref } from 'vue'
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
  <dialog v-if="open" class="sign-in" open aria-labelledby="sign-in-title">
    <h2 id="sign-in-title">Sign in to Scamper</h2>

    <p class="explain">
      Your files are kept on the Scamper server, so they survive a cleared
      browser and follow you between computers.
    </p>

    <form v-if="methods.password" @submit.prevent="submit">
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

    <button type="button" class="link dismiss" @click="emit('close')">
      Keep working in this browser instead
    </button>
  </dialog>
</template>

<style scoped>
.sign-in {
  position: fixed;
  inset: 0;
  margin: auto;
  z-index: 100;
  width: min(26rem, 92vw);
  padding: 1.5rem;
  border: 1px solid var(--border-color, #ccc);
  border-radius: 0.5rem;
  background: var(--bg-color, #fff);
  color: var(--fg-color, #000);
}

h2 {
  margin: 0 0 0.5rem;
  font-size: 1.15rem;
}

.explain {
  margin: 0 0 1rem;
  font-size: 0.9rem;
  opacity: 0.8;
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
}

button[type='submit'] {
  margin-top: 0.5rem;
  padding: 0.4rem 1rem;
  font: inherit;
}

.error {
  margin: 0.5rem 0 0;
  color: var(--error-color, #b00020);
  font-size: 0.9rem;
}

.note {
  margin: 1.25rem 0 0;
  font-size: 0.85rem;
  opacity: 0.75;
}

.link {
  border: none;
  background: none;
  padding: 0;
  color: inherit;
  text-decoration: underline;
  cursor: pointer;
  font: inherit;
  font-size: 0.85rem;
}

.dismiss {
  display: block;
  margin-top: 0.75rem;
  opacity: 0.75;
}
</style>
