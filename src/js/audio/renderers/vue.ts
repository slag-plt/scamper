import * as L from '../../../lpm';
import VueRenderer from '../../../lpm/renderers/vue.js';
import SampleRenderer from './SampleRenderer.vue';
import AudioPipelineRenderer from './AudioPipelineRenderer.vue';

VueRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'sample'),
  () => SampleRenderer,
);
VueRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'audio-pipeline'),
  () => AudioPipelineRenderer,
);
