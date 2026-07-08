<script setup lang="ts">
import { ConnectorHeight } from "./query-sizing"

const connectorHeight = `${ConnectorHeight.toString()}lh`
</script>

<template>
  <svg id="connectors"></svg>
</template>

<style scoped>
#connectors {
  width: 100%;
  height: v-bind("connectorHeight");
}
</style>
