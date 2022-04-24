import { defineConfig } from 'vite'
const path = require('path')
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
    plugins: [elmPlugin()],
    build: {
        rollupOptions: {
            input: {
                glossary: path.resolve(__dirname, 'glossary.html'),
                empty: path.resolve(__dirname, 'empty.html')
            }
        }
    }
})