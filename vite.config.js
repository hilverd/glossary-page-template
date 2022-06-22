import { defineConfig } from 'vite'
const path = require('path')
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
    plugins: [elmPlugin()],
    build: {
        rollupOptions: {
            input: {
                index: path.resolve(__dirname, 'index.html'),
                glossary: path.resolve(__dirname, 'glossary.html'),
                empty: path.resolve(__dirname, 'empty.html'),
                "uk-parliament": path.resolve(__dirname, 'examples/uk-parliament.html'),
                "open-glossary-of-edge-computing": path.resolve(__dirname, 'examples/open-glossary-of-edge-computing.html'),
                "canadas-weather-and-meterology-glossary.html": path.resolve(__dirname, 'examples/canadas-weather-and-meterology-glossary.html')
            }
        }
    }
})
