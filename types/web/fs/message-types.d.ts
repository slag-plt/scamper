export type FsRequest = {
    type: 'ReadFile';
    path: string;
    lock?: boolean;
} | {
    type: 'WriteFile';
    path: string;
    content: string;
    lock?: boolean;
} | {
    type: 'MoveFile';
    source: string;
    destination: string;
};
export type FsResponse = {
    type: 'FileContent';
    content: string;
} | {
    type: 'WriteComplete';
} | {
    type: 'MoveComplete';
} | {
    type: 'Error';
    message: string;
};
//# sourceMappingURL=message-types.d.ts.map