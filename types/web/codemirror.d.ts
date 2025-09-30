import { EditorState } from "@codemirror/state";
export type EditorStateConfig = {
    output: HTMLElement;
    dirtyAction: () => void;
    isReadOnly: boolean;
};
export declare function mkFreshEditorState(doc: string, config: EditorStateConfig): EditorState;
export declare function mkNoFileEditorState(output: HTMLElement): EditorState;
//# sourceMappingURL=codemirror.d.ts.map