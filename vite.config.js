import path from 'path'
import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
    plugins: [elmPlugin()],
    build: {
        rollupOptions: {
            input: {
                index: path.resolve(__dirname, 'index.html'),
                glossary: path.resolve(__dirname, 'glossary.html'),
                empty: path.resolve(__dirname, 'empty.html'),
                "python-3.11.2-glossary": path.resolve(__dirname, 'examples/python-3.11.2-glossary.html'),
                "uk-parliament": path.resolve(__dirname, 'examples/uk-parliament.html'),
                "open-glossary-of-edge-computing": path.resolve(__dirname, 'examples/open-glossary-of-edge-computing.html'),
                "canadas-weather-and-meterology-glossary.html": path.resolve(__dirname, 'examples/canadas-weather-and-meterology-glossary.html'),
                "glossary-of-group-theory.html": path.resolve(__dirname, 'examples/glossary-of-group-theory.html'),
            }
        }
    },
    server: {
        port: 3000
    }
})
