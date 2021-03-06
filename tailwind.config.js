const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  content: [
    './index.html',
    './glossary.{css,html,js}',
    './src/**/*.elm'
  ],
  plugins: [
    require('@tailwindcss/forms'),
  ],
  theme: {
    extend: {
      screens: {
        '3xl': '1800px',
        'print': { 'raw': 'print' },
      },
      fontFamily: {
        sans: ['Overpass', ...defaultTheme.fontFamily.sans],
      },
    },
  },
}
