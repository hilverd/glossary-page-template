import path from 'path';
import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
    plugins: [elmPlugin()],
    build: {
        emptyOutDir: false,
        lib: {
            entry: path.resolve(__dirname, 'src/Worker.elm'),
            name: 'Worker',
            fileName: 'worker',
            formats: ['umd']
        }
    }
});
