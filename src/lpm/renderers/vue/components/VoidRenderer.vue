<template>
  <!--
    How a void value is drawn: an element that takes up no space at all. A
    statement producing void keeps its place in the output -- so nothing that
    counts output blocks shifts -- while showing the reader nothing (#596).

    Styled inline rather than through `<style scoped>` so the element is hidden
    wherever it lands, including output the IDE hands to a page of its own.
  -->
  <div style="display: none"></div>
</template>
