<script setup lang="ts">
// Signing in to the file server (#357).
//
// Two ways in, and which are shown comes from the server rather than being
// assumed: an institution running Office 365 gets the Microsoft button, and
// email/password is what works without any of that configured. A server
// offering neither never reaches this dialog.
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
  microsoft: []
  password: [email: string, password: string]
  register: [name: string, email: string, password: string]
  close: []
}>()

/** Signing in and creating an account share the dialog; this picks which. */
const registering = ref(false)
const name = ref('')
const email = ref('')
const password = ref('')

function submit() {
  if (props.busy) return
  if (registering.value) {
    emit('register', name.value, email.value, password.value)
  } else {
    emit('password', email.value, password.value)
  }
}
</script>

<template>
  <dialog v-if="open" class="sign-in" open aria-labelledby="sign-in-title">
    <h2 id="sign-in-title">
      {{ registering ? 'Create an account' : 'Sign in to Scamper' }}
    </h2>

    <p class="explain">
      Your files are kept on the Scamper server, so they survive a cleared
      browser and follow you between computers.
    </p>

    <button
      v-if="methods.microsoft"
      class="microsoft"
      :disabled="busy"
      @click="emit('microsoft')"
    >
      Continue with Microsoft
    </button>

    <p v-if="methods.microsoft && methods.password" class="or">or</p>

    <form v-if="methods.password" @submit.prevent="submit">
      <label v-if="registering">
        Name
        <input v-model="name" type="text" autocomplete="name" required />
      </label>
      <label>
        Email
        <input v-model="email" type="email" autocomplete="email" required />
      </label>
      <label>
        Password
        <input
          v-model="password"
          type="password"
          :autocomplete="registering ? 'new-password' : 'current-password'"
          required
        />
      </label>

      <p v-if="error !== null" class="error" role="alert">{{ error }}</p>

      <div class="actions">
        <button type="submit" :disabled="busy">
          {{ registering ? 'Create account' : 'Sign in' }}
        </button>
        <button type="button" class="link" @click="registering = !registering">
          {{ registering ? 'I already have an account' : 'Create an account' }}
        </button>
      </div>
    </form>

    <p v-else-if="error !== null" class="error" role="alert">{{ error }}</p>

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

.microsoft {
  width: 100%;
  padding: 0.6rem;
  font-size: 1rem;
}

.or {
  margin: 0.75rem 0;
  text-align: center;
  font-size: 0.85rem;
  opacity: 0.7;
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

.actions {
  display: flex;
  gap: 0.75rem;
  align-items: center;
  margin-top: 1rem;
}

.error {
  margin: 0.5rem 0 0;
  color: var(--error-color, #b00020);
  font-size: 0.9rem;
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
  margin-top: 1.25rem;
  opacity: 0.75;
}
</style>
